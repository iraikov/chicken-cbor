;;;; cbor-term.scm - generic CBOR data items
;;;
;;; A term is a tree that mirrors a CBOR data item exactly: it keeps
;;; the width of floats and the difference between definite and
;;; indefinite lengths, so decoding and re-encoding a term reproduces
;;; the original bytes whenever they were in preferred form.  The
;;; module also prints terms in the diagnostic notation of RFC 8949
;;; section 8.

(module cbor-term

  (term term?
   term-int term-bytes term-bytes-indef term-string term-string-indef
   term-list term-list-indef term-map term-map-indef term-tagged
   term-bool term-null term-undefined term-simple
   term-half term-float term-double
   encode-term decode-term
   term->diagnostic term=?)

  (import scheme
          (scheme base)
          (chicken base)
          (chicken format)
          (chicken string)
          (srfi 4)
          datatype
          cbor-core)

  (define (list-of pred)
    (lambda (x) (and (list? x) (every? pred x))))

  (define (every? pred lst)
    (or (null? lst) (and (pred (car lst)) (every? pred (cdr lst)))))

  (define (term-pair? x) (and (pair? x) (term? (car x)) (term? (cdr x))))

  (define-datatype term term?
    (term-int (n exact-integer?))                 ; includes bignums
    (term-bytes (bv bytevector?))
    (term-bytes-indef (chunks (list-of bytevector?)))
    (term-string (s string?))
    (term-string-indef (chunks (list-of string?)))
    (term-list (items (list-of term?)))
    (term-list-indef (items (list-of term?)))
    (term-map (pairs (list-of term-pair?)))      ; list of (key . value)
    (term-map-indef (pairs (list-of term-pair?)))
    (term-tagged (tag exact-integer?) (value term?))
    (term-bool (b boolean?))
    (term-null)
    (term-undefined)
    (term-simple (n exact-integer?))
    (term-half (x flonum?))
    (term-float (x flonum?))
    (term-double (x flonum?)))

  ;;; Encoding

  ;; Writes the term T to PORT.  Integers beyond 64 bits are written
  ;; as bignums (tags 2 and 3).
  (define (encode-term t #!optional (port (current-output-port)))
    (let enc ((t t))
      (cases term t
        (term-int (n) (encode-int n port))
        (term-bytes (bv) (encode-bytes bv port))
        (term-bytes-indef (chunks)
          (encode-bytes-begin port)
          (for-each (lambda (c) (encode-bytes c port)) chunks)
          (encode-break port))
        (term-string (s) (encode-string s port))
        (term-string-indef (chunks)
          (encode-string-begin port)
          (for-each (lambda (c) (encode-string c port)) chunks)
          (encode-break port))
        (term-list (items)
          (encode-list-len (length items) port)
          (for-each enc items))
        (term-list-indef (items)
          (encode-list-begin port)
          (for-each enc items)
          (encode-break port))
        (term-map (pairs)
          (encode-map-len (length pairs) port)
          (for-each (lambda (p) (enc (car p)) (enc (cdr p))) pairs))
        (term-map-indef (pairs)
          (encode-map-begin port)
          (for-each (lambda (p) (enc (car p)) (enc (cdr p))) pairs)
          (encode-break port))
        (term-tagged (tag value) (encode-tag tag port) (enc value))
        (term-bool (b) (encode-bool b port))
        (term-null () (encode-null port))
        (term-undefined () (encode-undefined port))
        (term-simple (n) (encode-simple n port))
        (term-half (x) (encode-float16 x port))
        (term-float (x) (encode-float32 x port))
        (term-double (x) (encode-float64 x port)))))

  ;;; Decoding

  ;; Reads one data item with the decoder D and returns it as a term,
  ;; or returns an eof object if the input is already at its end.
  (define (decode-term d)
    (if (eq? (peek-token-type d) 'eof)
        #!eof
        (decode-item d)))

  (define (decode-items d n)
    (decoder-enter! d)
    (let ((items (if n
                     (let loop ((i 0) (acc '()))
                       (if (= i n) (reverse acc) (loop (+ i 1) (cons (decode-item d) acc))))
                     (let loop ((acc '()))
                       (if (decode-break-or d)
                           (reverse acc)
                           (loop (cons (decode-item d) acc)))))))
      (decoder-leave! d)
      items))

  (define (decode-pairs d n)
    (decoder-enter! d)
    (let ((pairs (if n
                     (let loop ((i 0) (acc '()))
                       (if (= i n)
                           (reverse acc)
                           (let* ((k (decode-item d)) (v (decode-item d)))
                             (loop (+ i 1) (cons (cons k v) acc)))))
                     (let loop ((acc '()))
                       (if (decode-break-or d)
                           (reverse acc)
                           (let* ((k (decode-item d)) (v (decode-item d)))
                             (loop (cons (cons k v) acc))))))))
      (decoder-leave! d)
      pairs))

  ;; Reads the chunks of an indefinite-length string with the chunk
  ;; reader READ-CHUNK, up to the closing break.
  (define (decode-chunks d read-chunk expected)
    (let loop ((acc '()))
      (if (decode-break-or d)
          (reverse acc)
          (begin
            (unless (eq? (peek-token-type d) expected)
              (cbor-error d 'malformed "chunk of type ~A inside an indefinite ~A"
                          (peek-token-type d) expected))
            (loop (cons (read-chunk d) acc))))))

  (define (decode-item d)
    (case (peek-token-type d)
      ((uint nint) (term-int (decode-int d)))
      ((bytes) (term-bytes (decode-bytes d)))
      ((bytes-indef)
       (decode-bytes-indef d)
       (term-bytes-indef (decode-chunks d decode-bytes 'bytes)))
      ((string) (term-string (decode-string d)))
      ((string-indef)
       (decode-string-indef d)
       (term-string-indef (decode-chunks d decode-string 'string)))
      ((list-len list-len-indef)
       (let ((n (decode-list-len-or-indef d)))
         (if n (term-list (decode-items d n)) (term-list-indef (decode-items d #f)))))
      ((map-len map-len-indef)
       (let ((n (decode-map-len-or-indef d)))
         (if n (term-map (decode-pairs d n)) (term-map-indef (decode-pairs d #f)))))
      ((tag)
       (let ((tag (decode-tag d)))
         (decoder-enter! d)
         (let ((value (decode-item d)))
           (decoder-leave! d)
           (term-tagged tag value))))
      ((bool) (term-bool (decode-bool d)))
      ((null) (decode-null d) (term-null))
      ((undefined) (decode-undefined d) (term-undefined))
      ((simple) (term-simple (decode-simple d)))
      ((float16 float32 float64)
       (let-values (((x width) (decode-float-token d)))
         (case width
           ((16) (term-half x))
           ((32) (term-float x))
           (else (term-double x)))))
      ((break) (cbor-error d 'malformed "break outside an indefinite-length item"))
      ((eof) (cbor-error d 'eof "unexpected end of input"))
      (else (cbor-error d 'malformed "invalid initial byte"))))

  ;;; Diagnostic notation

  (define (hex-string bv)
    (let ((digits "0123456789abcdef"))
      (let loop ((i (- (bytevector-length bv) 1)) (acc '()))
        (if (< i 0)
            (list->string acc)
            (let ((b (bytevector-u8-ref bv i)))
              (loop (- i 1)
                    (cons (string-ref digits (quotient b 16))
                          (cons (string-ref digits (remainder b 16)) acc))))))))

  ;; Quotes a string in the JSON style that diagnostic notation uses.
  (define (quote-string s)
    (let ((out (open-output-string)))
      (write-char #\" out)
      (for-each
       (lambda (c)
         (cond ((char=? c #\") (write-string "\\\"" out))
               ((char=? c #\\) (write-string "\\\\" out))
               ((char<? c #\space)
                (let ((h (number->string (char->integer c) 16)))
                  (write-string "\\u" out)
                  (write-string (make-string (- 4 (string-length h)) #\0) out)
                  (write-string h out)))
               (else (write-char c out))))
       (string->list s))
      (write-char #\" out)
      (get-output-string out)))

  (define (float->diagnostic x)
    (cond ((nan? x) "NaN")
          ((infinite? x) (if (> x 0) "Infinity" "-Infinity"))
          (else
           (let ((s (number->string x)))
             (if (or (string-index s #\.) (string-index s #\e))
                 s
                 (string-append s ".0"))))))

  (define (string-index s c)
    (let loop ((i 0))
      (cond ((= i (string-length s)) #f)
            ((char=? (string-ref s i) c) i)
            (else (loop (+ i 1))))))

  (define (join-items strings)
    (string-intersperse strings ", "))

  ;; Returns the RFC 8949 diagnostic notation of the term T, for
  ;; example "[1, [2, 3]]" or "{_ \"a\": 1}".
  (define (term->diagnostic t)
    (let diag ((t t))
      (define (pairs->strings pairs)
        (map (lambda (p) (string-append (diag (car p)) ": " (diag (cdr p)))) pairs))
      (cases term t
        (term-int (n) (number->string n))
        (term-bytes (bv) (string-append "h'" (hex-string bv) "'"))
        (term-bytes-indef (chunks)
          (string-append "(_ " (join-items (map (lambda (c) (string-append "h'" (hex-string c) "'"))
                                          chunks))
                         ")"))
        (term-string (s) (quote-string s))
        (term-string-indef (chunks)
          (string-append "(_ " (join-items (map quote-string chunks)) ")"))
        (term-list (items) (string-append "[" (join-items (map diag items)) "]"))
        (term-list-indef (items) (string-append "[_ " (join-items (map diag items)) "]"))
        (term-map (pairs) (string-append "{" (join-items (pairs->strings pairs)) "}"))
        (term-map-indef (pairs) (string-append "{_ " (join-items (pairs->strings pairs)) "}"))
        (term-tagged (tag value) (sprintf "~A(~A)" tag (diag value)))
        (term-bool (b) (if b "true" "false"))
        (term-null () "null")
        (term-undefined () "undefined")
        (term-simple (n) (sprintf "simple(~A)" n))
        (term-half (x) (float->diagnostic x))
        (term-float (x) (float->diagnostic x))
        (term-double (x) (float->diagnostic x)))))

  ;;; Equality

  ;; Floats are equal when their values are identical, with NaN equal
  ;; to NaN and -0.0 different from 0.0.
  (define (float=? a b)
    (or (eqv? a b) (and (nan? a) (nan? b))))

  ;; Compares two terms structurally.  Unlike equal?, it treats NaN as
  ;; equal to itself, so any decoded term compares equal to itself.
  (define (term=? a b)
    (define (list=? xs ys)
      (and (= (length xs) (length ys)) (every2 term=? xs ys)))
    (define (pairs=? xs ys)
      (and (= (length xs) (length ys))
           (every2 (lambda (p q) (and (term=? (car p) (car q)) (term=? (cdr p) (cdr q))))
                   xs ys)))
    (cases term a
      (term-list (xs) (cases term b (term-list (ys) (list=? xs ys)) (else #f)))
      (term-list-indef (xs) (cases term b (term-list-indef (ys) (list=? xs ys)) (else #f)))
      (term-map (xs) (cases term b (term-map (ys) (pairs=? xs ys)) (else #f)))
      (term-map-indef (xs) (cases term b (term-map-indef (ys) (pairs=? xs ys)) (else #f)))
      (term-tagged (t x)
        (cases term b (term-tagged (u y) (and (= t u) (term=? x y))) (else #f)))
      (term-half (x) (cases term b (term-half (y) (float=? x y)) (else #f)))
      (term-float (x) (cases term b (term-float (y) (float=? x y)) (else #f)))
      (term-double (x) (cases term b (term-double (y) (float=? x y)) (else #f)))
      (else (equal? a b))))

  (define (every2 pred xs ys)
    (or (null? xs) (and (pred (car xs) (car ys)) (every2 pred (cdr xs) (cdr ys)))))

  )
