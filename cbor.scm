;;;; cbor.scm - Scheme values to and from CBOR (RFC 8949)
;;;
;;; Maps Scheme data to CBOR data items and back.  Numbers, strings,
;;; symbols, lists, vectors, hash tables and SRFI-4 vectors are handled
;;; directly; SRFI-4 vectors become RFC 8746 typed arrays, written as
;;; one block of bytes.  Other types can be added through a registry of
;;; codecs, each tied to a CBOR tag.

(module cbor

  (;; Reading and writing values
   write-cbor read-cbor
   cbor->bytevector bytevector->cbor
   write-cbor-sequence read-cbor-sequence
   write-cbor-file read-cbor-file
   call-with-cbor-output-file call-with-cbor-input-file
   encode-value decode-value
   cbor-preferred-floats

   ;; Values without a direct Scheme counterpart
   cbor-null cbor-null?
   make-cbor-tagged cbor-tagged? cbor-tagged-tag cbor-tagged-value
   make-cbor-simple cbor-simple? cbor-simple-value

   ;; Codec registry
   make-cbor-codec cbor-codec? cbor-codec-tag cbor-codec-predicate
   cbor-codec-encoder cbor-codec-decoder
   register-cbor-codec! cbor-codecs

   ;; Tag numbers
   tag-self-describe tag-identifier tag-rational
   tag-scheme-pair tag-scheme-vector tag-scheme-char tag-scheme-keyword

   ;; Re-exported from cbor-core
   cbor-max-depth cbor-max-length)

  (import scheme
          (scheme base)
          (scheme file)
          (chicken base)
          (chicken bitwise)
          (chicken blob)
          (chicken keyword)
          (only (chicken memory) move-memory!)
          (srfi 4)
          (srfi 69)
          cbor-core)

  ;;; ================================================================
  ;;; Tag numbers
  ;;; ================================================================

  (define tag-self-describe 55799)      ; RFC 8949 section 3.4.6
  (define tag-identifier 39)            ; IANA registry: identifier
  (define tag-rational 30)              ; IANA registry: rational number

  ;; Tags for Scheme types that CBOR has no standard form for.  The
  ;; numbers lie in the first-come-first-served range of the IANA tag
  ;; registry.
  (define tag-scheme-pair    #x5C4E00)  ; improper list: [items..., tail]
  (define tag-scheme-vector  #x5C4E01)  ; vector: array
  (define tag-scheme-char    #x5C4E02)  ; character: code point
  (define tag-scheme-keyword #x5C4E03)  ; keyword: text string

  ;;; ================================================================
  ;;; Values without a direct Scheme counterpart
  ;;; ================================================================

  ;; The CBOR null value.  CBOR undefined maps to the Scheme void
  ;; value instead.
  (define-record-type cbor-null-type (make-cbor-null) cbor-null?)
  (define cbor-null (make-cbor-null))

  ;; A tagged item whose tag has no codec.
  (define-record-type cbor-tagged
    (make-cbor-tagged tag value)
    cbor-tagged?
    (tag cbor-tagged-tag)
    (value cbor-tagged-value))

  ;; A simple value other than false, true, null and undefined.
  (define-record-type cbor-simple
    (make-cbor-simple value)
    cbor-simple?
    (value cbor-simple-value))

  ;;; ================================================================
  ;;; Codec registry
  ;;; ================================================================

  ;; A codec ties a Scheme type to a CBOR tag.  PREDICATE recognizes
  ;; values of the type.  ENCODER is called with three arguments: the
  ;; value, a procedure that writes a nested Scheme value, and the
  ;; output port, for writing tokens directly with cbor-core.  It must
  ;; write exactly one data item, the content of the tag, which is
  ;; written before it.  DECODER is called with the decoded content
  ;; and returns the value.
  (define-record-type cbor-codec
    (make-cbor-codec tag predicate encoder decoder)
    cbor-codec?
    (tag cbor-codec-tag)
    (predicate cbor-codec-predicate)
    (encoder cbor-codec-encoder)
    (decoder cbor-codec-decoder))

  ;; Codecs registered with register-cbor-codec!, most recent first.
  (define registered-codecs '())

  ;; Adds CODEC to the global registry, ahead of earlier codecs.
  (define (register-cbor-codec! codec)
    (set! registered-codecs (cons codec registered-codecs)))

  ;; A list of codecs that take priority over the global registry, for
  ;; use with parameterize.  Built-in types are always handled before
  ;; any codec is consulted.
  (define cbor-codecs (make-parameter '()))

  (define (find-codec match?)
    (define (search cs)
      (cond ((null? cs) #f)
            ((match? (car cs)) (car cs))
            (else (search (cdr cs)))))
    (or (search (cbor-codecs)) (search registered-codecs)))

  (define (codec-for-value obj)
    (find-codec (lambda (c) ((cbor-codec-predicate c) obj))))

  (define (codec-for-tag tag)
    (find-codec (lambda (c) (= tag (cbor-codec-tag c)))))

  ;;; ================================================================
  ;;; Typed arrays (RFC 8746)
  ;;; ================================================================

  ;; A typed-array tag is 64 + f*16 + s*8 + e*4 + ll, where f marks
  ;; floats, s signed integers, e little-endian byte order, and ll the
  ;; element size (1, 2, 4 or 8 bytes for integers; 2, 4, 8 or 16 for
  ;; floats).  Vectors are written in host byte order.
  (define host-little-endian?
    (cond-expand (little-endian #t) (else #f)))

  (define (typed-array-tag float? signed? size-code)
    (+ 64
       (if float? 16 0)
       (if signed? 8 0)
       (if host-little-endian? 4 0)
       size-code))

  ;; Returns the tag and the shared bytes of the SRFI-4 vector OBJ, or
  ;; #f if OBJ is not such a vector.  u8vectors are bytevectors and are
  ;; written as plain byte strings, so they are not handled here.
  (define (typed-array-parts obj)
    (cond ((f32vector? obj) (values (typed-array-tag #t #f 1) (f32vector->blob/shared obj)))
          ((f64vector? obj) (values (typed-array-tag #t #f 2) (f64vector->blob/shared obj)))
          ((s8vector? obj) (values 72 (s8vector->blob/shared obj)))
          ((u16vector? obj) (values (typed-array-tag #f #f 1) (u16vector->blob/shared obj)))
          ((s16vector? obj) (values (typed-array-tag #f #t 1) (s16vector->blob/shared obj)))
          ((u32vector? obj) (values (typed-array-tag #f #f 2) (u32vector-bytes obj)))
          ((s32vector? obj) (values (typed-array-tag #f #t 2) (s32vector->blob/shared obj)))
          ((u64vector? obj) (values (typed-array-tag #f #f 3) (u64vector->blob/shared obj)))
          ((s64vector? obj) (values (typed-array-tag #f #t 3) (s64vector->blob/shared obj)))
          (else (values #f #f))))

  ;; Returns a copy of the bytes of the u32vector V.  The standard
  ;; srfi-4 module of some CHICKEN 6 builds lacks the shared and copying
  ;; u32vector-to-blob conversions, so the bytes are copied directly.
  (define (u32vector-bytes v)
    (let ((b (make-blob (* 4 (u32vector-length v)))))
      (move-memory! v b)
      b))

  (define (typed-array-tag? tag)
    (and (>= tag 64) (<= tag 87)))

  ;; Reverses, in place, the byte order of each SIZE-byte element of
  ;; the bytevector BV.
  (define (swap-bytes! bv size)
    (let ((n (bytevector-length bv)))
      (do ((i 0 (+ i size))) ((>= i n))
        (do ((a i (+ a 1)) (b (+ i size -1) (- b 1))) ((>= a b))
          (let ((t (bytevector-u8-ref bv a)))
            (bytevector-u8-set! bv a (bytevector-u8-ref bv b))
            (bytevector-u8-set! bv b t))))
      bv))

  ;; True for the typed-array tags that have a Scheme counterpart:
  ;; all except the 128-bit float tags 83 and 87 and the reserved
  ;; tag 76.
  (define (typed-array-supported? tag)
    (not (memv tag '(76 83 87))))

  ;; Builds the Scheme value for the supported typed-array TAG from
  ;; the bytevector BV, which is taken over without copying.
  (define (typed-array-value d tag bv)
    (let* ((code (- tag 64))
           (float? (= 1 (bitwise-and (arithmetic-shift code -4) 1)))
           (signed? (= 1 (bitwise-and (arithmetic-shift code -3) 1)))
           (little? (= 1 (bitwise-and (arithmetic-shift code -2) 1)))
           (ll (bitwise-and code 3))
           (size (if float? (expt 2 (+ ll 1)) (expt 2 ll))))
      (unless (= 0 (remainder (bytevector-length bv) size))
        (cbor-error d 'malformed "typed array of ~A bytes is not a multiple of ~A"
                    (bytevector-length bv) size))
      (when (and (> size 1) (not (eq? little? host-little-endian?)))
        (swap-bytes! bv size))
      (cond
       ((and float? (= ll 0)) (half-array->f32vector bv))
       (float? (case ll
                 ((1) (blob->f32vector/shared bv))
                 (else (blob->f64vector/shared bv))))
       ((= ll 0) (if signed? (blob->s8vector/shared bv) bv)) ; 64, 68 (clamped), 72
       (signed? (case ll
                  ((1) (blob->s16vector/shared bv))
                  ((2) (blob->s32vector/shared bv))
                  (else (blob->s64vector/shared bv))))
       (else (case ll
               ((1) (blob->u16vector/shared bv))
               ((2) (blob->u32vector/shared bv))
               (else (blob->u64vector/shared bv)))))))

  ;; Converts host-order half-precision elements to an f32vector.
  (define (half-array->f32vector bv)
    (let* ((n (quotient (bytevector-length bv) 2))
           (halves (blob->u16vector/shared bv))
           (out (make-f32vector n)))
      (do ((i 0 (+ i 1))) ((= i n) out)
        (f32vector-set! out i (half-bits->flonum (u16vector-ref halves i))))))

  ;;; ================================================================
  ;;; Encoding
  ;;; ================================================================

  ;; When true, flonums are written in the shortest exact float width
  ;; (RFC 8949 preferred serialization); otherwise they are always
  ;; written as doubles.
  (define cbor-preferred-floats (make-parameter #f))

  ;; Walks the list X and returns two values: the number of pairs and
  ;; the final cdr.  Signals an error for a circular list.
  (define (list-shape x)
    (let loop ((slow x) (fast x) (n 0))
      (cond ((not (pair? fast)) (values n fast))
            ((not (pair? (cdr fast))) (values (+ n 1) (cdr fast)))
            (else
             (let ((slow (cdr slow)) (fast (cddr fast)))
               (when (eq? slow fast)
                 (error 'write-cbor "circular list cannot be encoded"))
               (loop slow fast (+ n 2)))))))

  ;; Writes the Scheme value OBJ to PORT as one CBOR data item.
  (define (encode-value obj port)
    (let ((max-depth (cbor-max-depth))
          (preferred (cbor-preferred-floats)))
      (let enc ((obj obj) (depth 0))
        (define (nested x) (enc x (+ depth 1)))
        (when (> depth max-depth)
          (error 'write-cbor "nesting deeper than cbor-max-depth; the value may be cyclic"))
        (cond
         ((exact-integer? obj) (encode-int obj port))
         ((flonum? obj) (if preferred (encode-float obj port) (encode-float64 obj port)))
         ((string? obj) (encode-string obj port))
         ((keyword? obj)
          (encode-tag tag-scheme-keyword port)
          (encode-string (keyword->string obj) port))
         ((symbol? obj)
          (encode-tag tag-identifier port)
          (encode-string (symbol->string obj) port))
         ((null? obj) (encode-list-len 0 port))
         ((pair? obj)
          (let-values (((n tail) (list-shape obj)))
            (if (null? tail)
                (encode-list-len n port)
                (begin (encode-tag tag-scheme-pair port)
                       (encode-list-len (+ n 1) port)))
            (let loop ((x obj))
              (if (pair? x)
                  (begin (nested (car x)) (loop (cdr x)))
                  (unless (null? x) (nested x))))))
         ((boolean? obj) (encode-bool obj port))
         ((bytevector? obj) (encode-bytes obj port))
         ((vector? obj)
          (encode-tag tag-scheme-vector port)
          (encode-list-len (vector-length obj) port)
          (do ((i 0 (+ i 1))) ((= i (vector-length obj)))
            (nested (vector-ref obj i))))
         ((hash-table? obj)
          (encode-map-len (hash-table-size obj) port)
          (hash-table-walk obj (lambda (k v) (nested k) (nested v))))
         ((char? obj)
          (encode-tag tag-scheme-char port)
          (encode-uint (char->integer obj) port))
         ((eq? obj (void)) (encode-undefined port))
         ((cbor-null? obj) (encode-null port))
         ((cbor-tagged? obj)
          (encode-tag (cbor-tagged-tag obj) port)
          (nested (cbor-tagged-value obj)))
         ((cbor-simple? obj) (encode-simple (cbor-simple-value obj) port))
         ((and (number? obj) (exact? obj) (rational? obj))
          (encode-tag tag-rational port)
          (encode-list-len 2 port)
          (encode-int (numerator obj) port)
          (encode-int (denominator obj) port))
         (else
          (let-values (((tag bytes) (typed-array-parts obj)))
            (if tag
                (begin (encode-tag tag port) (encode-bytes bytes port))
                (let ((codec (codec-for-value obj)))
                  (unless codec
                    (error 'write-cbor "no CBOR encoding for value" obj))
                  (encode-tag (cbor-codec-tag codec) port)
                  ((cbor-codec-encoder codec) obj nested port)))))))))

  ;; Writes OBJ to PORT as one CBOR data item.
  (define (write-cbor obj #!optional (port (current-output-port)))
    (encode-value obj port))

  ;; Returns the CBOR encoding of OBJ as a bytevector.
  (define (cbor->bytevector obj)
    (let ((port (open-output-bytevector)))
      (encode-value obj port)
      (get-output-bytevector port)))

  ;; Writes each element of the list OBJS as a separate data item,
  ;; forming a CBOR sequence (RFC 8742).
  (define (write-cbor-sequence objs #!optional (port (current-output-port)))
    (for-each (lambda (obj) (encode-value obj port)) objs))

  ;;; ================================================================
  ;;; Decoding
  ;;; ================================================================

  (define (decode-array d n)
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

  (define (decode-map d n)
    (decoder-enter! d)
    (let ((table (make-hash-table equal?)))
      (if n
          (do ((i 0 (+ i 1))) ((= i n))
            (let* ((k (decode-item d)) (v (decode-item d)))
              (hash-table-set! table k v)))
          (let loop ()
            (unless (decode-break-or d)
              (let* ((k (decode-item d)) (v (decode-item d)))
                (hash-table-set! table k v)
                (loop)))))
      (decoder-leave! d)
      table))

  ;; Decodes the content of tag TAG.
  (define (decode-tagged d tag)
    (decoder-enter! d)
    (let ((value
           (cond
            ((and (typed-array-tag? tag)
                  (memq (peek-token-type d) '(bytes bytes-indef)))
             (let ((bv (decode-bytes d)))
               (if (typed-array-supported? tag)
                   (typed-array-value d tag bv)
                   (make-cbor-tagged tag bv))))
            ((= tag tag-self-describe) (decode-item d))
            ((or (= tag 2) (= tag 3))
             (let* ((bv (decode-bytes d))
                    (mag (let loop ((i 0) (v 0))
                           (if (= i (bytevector-length bv))
                               v
                               (loop (+ i 1) (bitwise-ior (arithmetic-shift v 8)
                                                          (bytevector-u8-ref bv i)))))))
               (if (= tag 2) mag (- -1 mag))))
            (else
             (let ((content (decode-item d))
                   (codec (codec-for-tag tag)))
               (cond
                (codec ((cbor-codec-decoder codec) content))
                ((and (= tag tag-identifier) (string? content)) (string->symbol content))
                ((and (= tag tag-scheme-keyword) (string? content)) (string->keyword content))
                ((and (= tag tag-scheme-char) (exact-integer? content)) (integer->char content))
                ((and (= tag tag-scheme-vector) (list? content)) (list->vector content))
                ((and (= tag tag-scheme-pair) (list? content) (>= (length content) 2))
                 (let loop ((items content))
                   (if (null? (cdr items))
                       (car items)
                       (cons (car items) (loop (cdr items))))))
                ((and (= tag tag-rational) (list? content) (= (length content) 2)
                      (exact-integer? (car content)) (exact-integer? (cadr content))
                      (> (cadr content) 0))
                 (/ (car content) (cadr content)))
                (else (make-cbor-tagged tag content))))))))
      (decoder-leave! d)
      value))

  (define (decode-item d)
    (case (peek-token-type d)
      ((uint nint) (decode-int d))
      ((bytes bytes-indef) (decode-bytes d))
      ((string string-indef) (decode-string d))
      ((list-len list-len-indef) (decode-array d (decode-list-len-or-indef d)))
      ((map-len map-len-indef) (decode-map d (decode-map-len-or-indef d)))
      ((tag) (decode-tagged d (decode-tag d)))
      ((bool) (decode-bool d))
      ((null) (decode-null d) cbor-null)
      ((undefined) (decode-undefined d) (void))
      ((simple) (make-cbor-simple (decode-simple d)))
      ((float16 float32 float64) (decode-float d))
      ((break) (cbor-error d 'malformed "break outside an indefinite-length item"))
      ((eof) (cbor-error d 'eof "unexpected end of input"))
      (else (cbor-error d 'malformed "invalid initial byte"))))

  ;; Reads one data item with the decoder D and returns it as a Scheme
  ;; value, or returns an eof object at the end of input.
  (define (decode-value d)
    (if (eq? (peek-token-type d) 'eof)
        #!eof
        (decode-item d)))

  ;; Reads one data item from PORT, or returns an eof object if PORT
  ;; is at its end.
  (define (read-cbor #!optional (port (current-input-port)))
    (decode-value (make-decoder port)))

  ;; Decodes the bytevector BV, which must hold exactly one data item.
  (define (bytevector->cbor bv)
    (let* ((d (make-decoder (open-input-bytevector bv)))
           (value (decode-item d)))
      (unless (eq? (peek-token-type d) 'eof)
        (cbor-error d 'malformed "extra bytes after the data item"))
      value))

  ;; Reads every data item of a CBOR sequence from PORT and returns
  ;; them as a list.
  (define (read-cbor-sequence #!optional (port (current-input-port)))
    (let ((d (make-decoder port)))
      (let loop ((acc '()))
        (let ((v (decode-value d)))
          (if (eof-object? v) (reverse acc) (loop (cons v acc)))))))

  ;;; ================================================================
  ;;; Files
  ;;; ================================================================

  ;; Opens PATH for binary output, writes the self-describe tag (the
  ;; bytes d9 d9 f7, which mark the file as CBOR), and calls PROC with
  ;; the port.  PROC must write exactly one data item.
  (define (call-with-cbor-output-file path proc)
    (let ((port (open-binary-output-file path)))
      (dynamic-wind
          (lambda () #f)
          (lambda ()
            (encode-tag tag-self-describe port)
            (proc port))
          (lambda () (close-output-port port)))))

  ;; Opens PATH for binary input and calls PROC with the port.  A
  ;; leading self-describe tag is skipped by read-cbor.
  (define (call-with-cbor-input-file path proc)
    (let ((port (open-binary-input-file path)))
      (dynamic-wind
          (lambda () #f)
          (lambda () (proc port))
          (lambda () (close-input-port port)))))

  ;; Writes OBJ to the file PATH as one self-described data item.
  (define (write-cbor-file path obj)
    (call-with-cbor-output-file path (lambda (port) (encode-value obj port))))

  ;; Reads the single data item in the file PATH.
  (define (read-cbor-file path)
    (call-with-cbor-input-file
     path
     (lambda (port)
       (let* ((d (make-decoder port))
              (value (decode-value d)))
         (when (eof-object? value)
           (cbor-error d 'eof "file ~A holds no data item" path))
         value))))

  )
