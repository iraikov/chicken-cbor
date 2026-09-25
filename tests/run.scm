;;;; run.scm - tests for the cbor egg

(import scheme
        (scheme base)
        (scheme file)
        (chicken base)
        (chicken bitwise)
        (chicken blob)
        (chicken condition)
        (chicken file)
        (chicken format)
        (chicken keyword)
        (chicken random)
        (srfi 4)
        (srfi 69)
        test
        cbor-core cbor-token cbor-term cbor cbor-deflate)

(include-relative "appendix-a.scm")

;;; Helpers

(define (hex->bytevector h)
  (let loop ((i 0) (acc '()))
    (if (>= i (string-length h))
        (apply bytevector (reverse acc))
        (loop (+ i 2) (cons (string->number (substring h i (+ i 2)) 16) acc)))))

(define (bytevector->hex bv)
  (apply string-append
         (map (lambda (b) (string-append (if (< b 16) "0" "") (number->string b 16)))
              (u8vector->list bv))))

(define (decoder-for bv) (make-decoder (open-input-bytevector bv)))

;; Decodes BV as exactly one term; signals an error for trailing bytes.
(define (decode-term-exactly bv)
  (let* ((d (decoder-for bv))
         (t (decode-term d)))
    (when (eof-object? t) (cbor-error d 'eof "empty input"))
    (unless (eq? (peek-token-type d) 'eof)
      (cbor-error d 'malformed "trailing bytes"))
    t))

(define (term->bytevector t)
  (let ((p (open-output-bytevector)))
    (encode-term t p)
    (get-output-bytevector p)))

(define (cbor-error? thunk)
  (condition-case (begin (thunk) #f)
    ((exn cbor) #t)))

;; Converts a term to the plain form used for the expected values of
;; Appendix A: arrays as vectors, maps as (map (key . value) ...),
;; null as the symbol null.  The form is built from the encoded bytes.
(define (term-plain t)
  (let* ((bv (term->bytevector t))
         (d (decoder-for bv)))
    (plain-item d)))

(define (plain-item d)
  (case (peek-token-type d)
    ((uint nint) (decode-int d))
    ((string string-indef) (decode-string d))
    ((bytes bytes-indef) (decode-bytes d))
    ((list-len list-len-indef)
     (let ((n (decode-list-len-or-indef d)))
       (list->vector
        (if n
            (let loop ((i 0) (acc '()))
              (if (= i n) (reverse acc) (loop (+ i 1) (cons (plain-item d) acc))))
            (let loop ((acc '()))
              (if (decode-break-or d) (reverse acc) (loop (cons (plain-item d) acc))))))))
    ((map-len map-len-indef)
     (let ((n (decode-map-len-or-indef d)))
       (cons 'map
             (if n
                 (let loop ((i 0) (acc '()))
                   (if (= i n)
                       (reverse acc)
                       (let* ((k (plain-item d)) (v (plain-item d)))
                         (loop (+ i 1) (cons (cons k v) acc)))))
                 (let loop ((acc '()))
                   (if (decode-break-or d)
                       (reverse acc)
                       (let* ((k (plain-item d)) (v (plain-item d)))
                         (loop (cons (cons k v) acc)))))))))
    ((bool) (decode-bool d))
    ((null) (decode-null d) 'null)
    ((float16 float32 float64) (decode-float d))
    ((tag) (let ((tag (decode-tag d)))
             (if (memv tag '(2 3))
                 (let* ((bv (decode-bytes d))
                        (mag (let loop ((i 0) (v 0))
                               (if (= i (bytevector-length bv))
                                   v
                                   (loop (+ i 1) (+ (* v 256) (bytevector-u8-ref bv i)))))))
                   (if (= tag 2) mag (- -1 mag)))
                 (list 'tagged tag (plain-item d)))))
    (else (list 'other (decode-simple d)))))

(define (plain=? a b)
  (cond ((and (number? a) (number? b)) (= a b))
        ((and (vector? a) (vector? b))
         (and (= (vector-length a) (vector-length b))
              (let loop ((i 0))
                (or (= i (vector-length a))
                    (and (plain=? (vector-ref a i) (vector-ref b i)) (loop (+ i 1)))))))
        ((and (pair? a) (pair? b))
         (and (plain=? (car a) (car b)) (plain=? (cdr a) (cdr b))))
        (else (equal? a b))))

;; Compares Scheme values, treating SRFI-4 vectors and flonums by
;; their bits, so that NaN and -0.0 compare correctly.
(define (value=? a b)
  (define (bits v)
    (cond ((f32vector? v) (f32vector->blob/shared v))
          ((f64vector? v) (f64vector->blob/shared v))
          ((s8vector? v) (s8vector->blob/shared v))
          ((u16vector? v) (u16vector->blob/shared v))
          ((s16vector? v) (s16vector->blob/shared v))
          ((s32vector? v) (s32vector->blob/shared v))
          ((u64vector? v) (u64vector->blob/shared v))
          ((s64vector? v) (s64vector->blob/shared v))
          (else #f)))
  (cond ((and (flonum? a) (flonum? b)) (or (eqv? a b) (and (nan? a) (nan? b))))
        ((bits a) => (lambda (ba) (and (bits b)
                                       (eq? (vector-type a) (vector-type b))
                                       (equal? ba (bits b)))))
        ((u32vector? a) (and (u32vector? b) (equal? (u32vector->list a) (u32vector->list b))))
        ((and (pair? a) (pair? b)) (and (value=? (car a) (car b)) (value=? (cdr a) (cdr b))))
        ((and (vector? a) (vector? b))
         (value=? (vector->list a) (vector->list b)))
        ((and (hash-table? a) (hash-table? b))
         (and (= (hash-table-size a) (hash-table-size b))
              (let loop ((keys (hash-table-keys a)))
                (or (null? keys)
                    (and (hash-table-exists? b (car keys))
                         (value=? (hash-table-ref a (car keys)) (hash-table-ref b (car keys)))
                         (loop (cdr keys)))))))
        ((and (cbor-tagged? a) (cbor-tagged? b))
         (and (= (cbor-tagged-tag a) (cbor-tagged-tag b))
              (value=? (cbor-tagged-value a) (cbor-tagged-value b))))
        ((and (cbor-simple? a) (cbor-simple? b))
         (= (cbor-simple-value a) (cbor-simple-value b)))
        (else (equal? a b))))

(define (vector-type v)
  (cond ((f32vector? v) 'f32) ((f64vector? v) 'f64) ((s8vector? v) 's8)
        ((u16vector? v) 'u16) ((s16vector? v) 's16) ((s32vector? v) 's32)
        ((u64vector? v) 'u64) ((s64vector? v) 's64) (else #f)))

(define (roundtrip x) (bytevector->cbor (cbor->bytevector x)))

(define tmp-file "cbor-test.tmp")

(define (file-roundtrip x)
  (write-cbor-file tmp-file x)
  (let ((y (read-cbor-file tmp-file)))
    (delete-file* tmp-file)
    y))

;;; RFC 8949 Appendix A

(test-group "RFC 8949 Appendix A"
  (for-each
   (lambda (entry)
     (let* ((hex (car entry))
            (roundtrip? (cadr entry))
            (expected (caddr entry))
            (t (decode-term-exactly (hex->bytevector hex))))
       (case (car expected)
         ((decoded) (test-assert (string-append "decode " hex)
                                 (plain=? (term-plain t) (cadr expected))))
         ((diagnostic) (test (string-append "diagnostic " hex)
                             (cadr expected) (term->diagnostic t))))
       (when roundtrip?
         (test (string-append "re-encode " hex) hex (bytevector->hex (term->bytevector t))))
       (test-assert (string-append "native decode " hex)
                    (begin (bytevector->cbor (hex->bytevector hex)) #t))))
   appendix-a-vectors))

;;; RFC 8949 Appendix F: inputs that are not well-formed

(define not-well-formed
  '(;; End of input in a head
    "18" "19" "1a" "1b" "1901" "1a0102" "1b01020304050607" "38" "58" "78"
    "98" "9a01ff00" "b8" "d8" "f8" "f900" "fa0000" "fb000000"
    ;; Definite-length strings with short data
    "41" "61" "5affffffff00" "5bffffffffffffffff010203" "7affffffff00"
    "7b7fffffffffffffff010203"
    ;; Definite-length maps and arrays with too few items
    "81" "818181818181818181" "8200" "a1" "a20102" "a100" "a2000000"
    ;; Tag without content
    "c0"
    ;; Indefinite-length strings without a break
    "5f4100" "7f6100"
    ;; Indefinite-length maps and arrays without a break
    "9f" "9f0102" "bf" "bf01020102" "819f" "9f8000" "9f9f9f9f9fffffffff"
    "9f819f819f9fffffff"
    ;; Reserved additional information
    "1c" "1d" "1e" "3c" "3d" "3e" "5c" "5d" "5e" "7c" "7d" "7e" "9c" "9d"
    "9e" "bc" "bd" "be" "dc" "dd" "de" "fc" "fd" "fe"
    ;; Reserved two-byte simple values
    "f800" "f801" "f818" "f81f"
    ;; Indefinite-length string chunks of the wrong type
    "5f00ff" "5f21ff" "5f6100ff" "5f80ff" "5fa0ff" "5fc000ff" "5fe0ff" "7f4100ff"
    ;; Indefinite-length string chunks that are not definite
    "5f5f4100ffff" "7f7f6100ffff"
    ;; Break outside an indefinite-length item
    "ff"
    ;; Break inside a definite-length array, map or tag
    "81ff" "8200ff" "a1ff" "a1ff00" "a100ff" "a20000ff" "9f81ff" "9f829f819f9fffffffff"
    ;; Break in the value position of an indefinite-length map
    "bf00ff" "bf000000ff"
    ;; Major types 0, 1 and 6 with additional information 31
    "1f" "3f" "df"))

(test-group "RFC 8949 Appendix F"
  (for-each
   (lambda (hex)
     (test-assert (string-append "term rejects " hex)
                  (cbor-error? (lambda () (decode-term-exactly (hex->bytevector hex)))))
     (test-assert (string-append "value rejects " hex)
                  (cbor-error? (lambda () (bytevector->cbor (hex->bytevector hex))))))
   not-well-formed))

;;; RFC 8746 typed arrays

(define (random-bytevector n) (random-bytes (make-blob n)))

(test-group "RFC 8746 typed arrays"
  (let ((specials (f64vector -0.0 +inf.0 -inf.0 +nan.0 4.9e-324 1.0 -1.0)))
    (test-assert "f64 specials" (value=? specials (roundtrip specials))))
  (let ((specials (f32vector -0.0 +inf.0 -inf.0 +nan.0 1.4e-45 1.0 -1.0)))
    (test-assert "f32 specials" (value=? specials (roundtrip specials))))
  (for-each
   (lambda (spec)
     (let* ((name (car spec)) (size (cadr spec)) (make (caddr spec))
            (v (make (random-bytevector (* size 1001)))))
       (test-assert (sprintf "~A random round trip" name) (value=? v (roundtrip v)))
       (test-assert (sprintf "~A file round trip" name) (value=? v (file-roundtrip v)))))
   (list (list 'f32 4 blob->f32vector/shared)
         (list 'f64 8 blob->f64vector/shared)
         (list 's8 1 blob->s8vector/shared)
         (list 'u16 2 blob->u16vector/shared)
         (list 's16 2 blob->s16vector/shared)
         (list 'u32 4 blob->u32vector/shared)
         (list 's32 4 blob->s32vector/shared)
         (list 'u64 8 blob->u64vector/shared)
         (list 's64 8 blob->s64vector/shared)))
  (test "f32 is written with tag 85" "d85548" (substring (bytevector->hex (cbor->bytevector (f32vector 1.5 2.0))) 0 6))
  (test-assert "f32 big-endian tag 81"
               (value=? (f32vector 1.5) (bytevector->cbor (hex->bytevector "d851443fc00000"))))
  (test-assert "f64 big-endian tag 82"
               (value=? (f64vector 1.5) (bytevector->cbor (hex->bytevector "d852483ff8000000000000"))))
  (test-assert "u16 big-endian tag 65"
               (value=? (u16vector 258 1) (bytevector->cbor (hex->bytevector "d8414401020001"))))
  (test-assert "f16 little-endian tag 84 becomes an f32vector"
               (value=? (f32vector 1.0 -2.0) (bytevector->cbor (hex->bytevector "d85444003c00c0"))))
  (test-assert "uint8 tag 64 becomes a bytevector"
               (equal? (bytevector 1 2) (bytevector->cbor (hex->bytevector "d840420102"))))
  (test-assert "128-bit floats stay tagged"
               (cbor-tagged? (bytevector->cbor (hex->bytevector "d8574100"))))
  (test-assert "typed array with a partial element is rejected"
               (cbor-error? (lambda () (bytevector->cbor (hex->bytevector "d85543000000"))))))

;;; Scheme values

(define-record-type point (make-point x y) point? (x point-x) (y point-y))

(test-group "Scheme values"
  (for-each
   (lambda (x)
     (test-assert (sprintf "round trip ~S" x) (value=? x (roundtrip x)))
     (test-assert (sprintf "file round trip ~S" x) (value=? x (file-roundtrip x))))
   (list 0 1 -1 23 24 255 256 65535 65536 4294967296 18446744073709551615
         18446744073709551616 -18446744073709551617 (expt 7 100) (- (expt 3 90))
         1.5 -0.0 1.0e300 5e-324 +inf.0 -inf.0 3/7 -22/7
         "" "hello" (string #\a (integer->char 0) #\b) "\x3bb;\x6c34;\x10151;"
         'sym '|with space| foo: #\a #\x3bb; (integer->char 0)
         #t #f '() (void) cbor-null
         '(1 2 3) '(1 . 2) '(1 2 . 3) '((dtype . f32) (shape 2 3) (requires-grad . #t))
         (vector) (vector 1 "a" 'b) (vector (vector 1) '(2 . 3))
         (bytevector) (bytevector 0 1 2 255)
         (make-cbor-tagged 1000 "x") (make-cbor-simple 16) (make-cbor-simple 255)
         (list (f32vector 1.0 2.0) (f64vector 3.0) (s32vector -1 2))))
  (let ((ht (make-hash-table equal?)))
    (hash-table-set! ht "a" 1)
    (hash-table-set! ht 'b (list 2 3))
    (hash-table-set! ht 7 (f64vector 1.0))
    (test-assert "hash table round trip" (value=? ht (roundtrip ht))))
  (test "flonums are doubles by default" "fb3ff8000000000000" (bytevector->hex (cbor->bytevector 1.5)))
  (test "preferred floats" "f93e00"
        (bytevector->hex (parameterize ((cbor-preferred-floats #t)) (cbor->bytevector 1.5))))
  (test "symbols use tag 39" "d82763666f6f" (bytevector->hex (cbor->bytevector 'foo)))
  (test "lists are arrays" "83010203" (bytevector->hex (cbor->bytevector '(1 2 3))))
  (test-assert "circular lists are rejected"
               (condition-case (let ((l (list 1 2))) (set-cdr! (cdr l) l) (cbor->bytevector l) #f)
                 ((exn) #t)))
  (test-assert "cyclic vectors are rejected"
               (condition-case (let ((v (vector 1))) (vector-set! v 0 v) (cbor->bytevector v) #f)
                 ((exn) #t)))
  (test-assert "unknown values are rejected"
               (condition-case (begin (cbor->bytevector (make-point 1 2)) #f)
                 ((exn) #t)))
  (test-assert "trailing bytes are rejected"
               (cbor-error? (lambda () (bytevector->cbor (bytevector 1 2))))))

(test-group "random values"
  (define (random-value depth)
    (let ((k (pseudo-random-integer (if (> depth 3) 8 12))))
      (case k
        ((0) (- (pseudo-random-integer 2000000) 1000000))
        ((1) (* (pseudo-random-real) 1e6))
        ((2) (list->string (map (lambda (i) (integer->char (+ 32 (pseudo-random-integer 900))))
                                (iota (pseudo-random-integer 8)))))
        ((3) (string->symbol (string-append "s" (number->string (pseudo-random-integer 100)))))
        ((4) (= 0 (pseudo-random-integer 2)))
        ((5) (random-bytevector (pseudo-random-integer 10)))
        ((6) (blob->f32vector/shared (random-bytevector (* 4 (pseudo-random-integer 10)))))
        ((7) (expt 2 (+ 60 (pseudo-random-integer 100))))
        ((8 9) (map (lambda (i) (random-value (+ depth 1))) (iota (pseudo-random-integer 5))))
        ((10) (cons (random-value (+ depth 1)) (random-value (+ depth 1))))
        (else (list->vector (map (lambda (i) (random-value (+ depth 1)))
                                 (iota (pseudo-random-integer 4))))))))
  (define (iota n) (let loop ((i (- n 1)) (acc '())) (if (< i 0) acc (loop (- i 1) (cons i acc)))))
  (test-assert "200 random values round trip"
               (let loop ((i 0))
                 (or (= i 200)
                     (let ((x (random-value 0)))
                       (and (value=? x (roundtrip x)) (loop (+ i 1)))))))
  (test-assert "200 random values round trip as terms"
               (let loop ((i 0))
                 (or (= i 200)
                     (let* ((bv (cbor->bytevector (random-value 0)))
                            (t (decode-term-exactly bv)))
                       (and (equal? bv (term->bytevector t))
                            (term=? t (decode-term-exactly (term->bytevector t)))
                            (loop (+ i 1))))))))

;;; Limits

(test-group "limits"
  (define (nested n)
    (let ((p (open-output-bytevector)))
      (do ((i 0 (+ i 1))) ((= i n)) (write-u8 #x81 p))
      (write-u8 0 p)
      (get-output-bytevector p)))
  (test-assert "1000 levels decode" (begin (bytevector->cbor (nested 1000)) #t))
  (test-assert "2000 levels exceed the default depth"
               (cbor-error? (lambda () (bytevector->cbor (nested 2000)))))
  (test-assert "depth limit applies to terms"
               (cbor-error? (lambda () (decode-term-exactly (nested 2000)))))
  (test-assert "forged 4 GB length fails at end of input"
               (cbor-error? (lambda () (bytevector->cbor (hex->bytevector "5b00000001000000000102")))))
  (test-assert "length limit"
               (cbor-error? (lambda () (parameterize ((cbor-max-length 4))
                                         (bytevector->cbor (cbor->bytevector "hello"))))))
  (test-assert "item count limit"
               (cbor-error? (lambda () (parameterize ((cbor-max-length 2))
                                         (bytevector->cbor (cbor->bytevector '(1 2 3)))))))
  (test "error reports reason" 'eof
        (condition-case (bytevector->cbor (hex->bytevector "1901"))
          (e (exn cbor) ((condition-property-accessor 'cbor 'reason) e)))))

;;; Files and sequences

(test-group "files and sequences"
  (write-cbor-file tmp-file (list 1 2))
  (test "file starts with the self-describe tag" "d9d9f7"
        (bytevector->hex (call-with-input-file tmp-file
                           (lambda (p) (read-bytevector 3 p)))))
  (test-assert "self-described file reads back" (value=? (list 1 2) (read-cbor-file tmp-file)))
  (delete-file* tmp-file)
  (let ((p (open-output-bytevector)))
    (write-cbor-sequence (list 1 "two" '(3)) p)
    (test-assert "sequence round trip"
                 (value=? (list 1 "two" '(3))
                          (read-cbor-sequence (open-input-bytevector (get-output-bytevector p))))))
  (test-assert "read-cbor returns eof at the end"
               (eof-object? (read-cbor (open-input-bytevector (bytevector))))))

;;; Codecs

(test-group "codecs"
  (parameterize ((cbor-codecs
                  (list (make-cbor-codec
                         #x5C4E80 point?
                         (lambda (p emit port) (emit (list (point-x p) (point-y p))))
                         (lambda (content) (make-point (car content) (cadr content)))))))
    (let ((p (roundtrip (list (make-point 1 2.5)))))
      (test-assert "codec value decodes" (point? (car p)))
      (test "codec fields" '(1 2.5) (list (point-x (car p)) (point-y (car p))))))
  (test-assert "without the codec, the tag is kept"
               (cbor-tagged? (bytevector->cbor (hex->bytevector "da005c4e808201f93e00"))))
  (test-assert "registered codec"
               (begin
                 (register-cbor-codec!
                  (make-cbor-codec #x5C4E81 point?
                                   (lambda (p emit port) (emit (point-x p)))
                                   (lambda (content) (make-point content 0))))
                 (point? (roundtrip (make-point 5 0))))))

;;; Compression

(test-group "compression"
  (define (structured n)
    (let ((v (make-f32vector n)))
      (do ((i 0 (+ i 1))) ((= i n) v) (f32vector-set! v i (exact->inexact (modulo i 100))))))
  (for-each
   (lambda (spec)
     (let ((name (car spec)) (x (cadr spec)))
       (test-assert (sprintf "~A round trip" name) (value=? x (roundtrip (cbor-deflated x))))
       (test-assert (sprintf "~A file round trip" name)
                    (value=? x (file-roundtrip (cbor-deflated x))))))
   (list (list "1 KB structured" (structured 256))
         (list "1 KB random" (blob->f32vector/shared (random-bytevector 1024)))
         (list "1 MB structured" (structured 262144))
         (list "10 MB random" (blob->f64vector/shared (random-bytevector (* 10 1024 1024))))
         (list "nested value" (list 'a "b" (vector 1 2) (f64vector 1.0 2.0)))))
  (let* ((x (structured 262144))
         (plain (cbor->bytevector x))
         (packed (cbor->bytevector (cbor-deflated x))))
    (test-assert "structured data shrinks" (< (* 10 (bytevector-length packed)) (bytevector-length plain))))
  (test-assert "compressed item inside a list"
               (value=? (list 1 (f32vector 1.0) 2)
                        (roundtrip (list 1 (cbor-deflated (f32vector 1.0)) 2))))
  (test-assert "write-cbor/compressed"
               (value=? "hello"
                        (let ((p (open-output-bytevector)))
                          (write-cbor/compressed "hello" p)
                          (read-cbor (open-input-bytevector (get-output-bytevector p))))))
  (test-assert "deflate-bytevector and inflate-bytevector"
               (let ((b (random-bytevector 5000)))
                 (equal? b (inflate-bytevector (deflate-bytevector b) 5000))))
  ;; Returns the raw content [1, length, data] of the compressed item
  ;; for X, read with the token-level decoder.
  (define (packed-content x)
    (let ((d (decoder-for (cbor->bytevector (cbor-deflated x)))))
      (decode-tag d)
      (decode-list-len d)
      (let* ((algorithm (decode-uint d))
             (length (decode-uint d))
             (data (decode-bytes d)))
        (list algorithm length data))))
  (define (unpack content)
    (bytevector->cbor (cbor->bytevector (make-cbor-tagged tag-compressed content))))
  (test-assert "wrong length is rejected"
               (let ((c (packed-content "hello world")))
                 (cbor-error? (lambda () (unpack (list 1 (+ 1 (cadr c)) (caddr c)))))))
  (test-assert "corrupt data is rejected"
               (let* ((c (packed-content (structured 1000)))
                      (data (bytevector-copy (caddr c))))
                 (bytevector-u8-set! data 10 (bitwise-xor 255 (bytevector-u8-ref data 10)))
                 (cbor-error? (lambda () (unpack (list 1 (cadr c) data))))))
  (test-assert "truncated data is rejected"
               (let ((c (packed-content (structured 1000))))
                 (cbor-error? (lambda () (unpack (list 1 (cadr c)
                                                       (bytevector-copy (caddr c) 0 20)))))))
  (test-assert "impossible length is rejected"
               (cbor-error? (lambda () (unpack (list 1 100000000 (bytevector 1 2 3))))))
  (test-assert "unknown algorithm is rejected"
               (cbor-error? (lambda () (unpack (list 2 3 (bytevector 1 2 3)))))))

;;; Tokens

(test-group "tokens"
  (test "read tokens" '((tk-list-begin) (tk-uint 1) (tk-string "a") (tk-break))
        (map token->list (read-tokens (open-input-bytevector (hex->bytevector "9f016161ff")))))
  (test "write tokens" "9f016161ff"
        (bytevector->hex
         (let ((p (open-output-bytevector)))
           (write-tokens (list (tk-list-begin) (tk-uint 1) (tk-string "a") (tk-break)) p)
           (get-output-bytevector p)))))

(test-exit)
