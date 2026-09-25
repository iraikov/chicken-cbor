;;;; cbor-token.scm - CBOR as a flat stream of tokens
;;;
;;; Represents encoded CBOR as a list of tokens, one per initial byte:
;;; the header of an array is a token, and so is each of its items.
;;; The token stream is independent of the binary layout, which makes
;;; it convenient for testing encoders and for inspecting data.

(module cbor-token

  (token token?
   tk-uint tk-nint tk-bytes tk-bytes-begin tk-string tk-string-begin
   tk-list-len tk-list-begin tk-map-len tk-map-begin tk-tag
   tk-bool tk-null tk-undefined tk-simple
   tk-float16 tk-float32 tk-float64 tk-break
   token->list
   write-token write-tokens read-token read-tokens)

  (import scheme
          (scheme base)
          (chicken base)
          (srfi 4)
          datatype
          cbor-core)

  (define (byte? n) (and (exact-integer? n) (<= 0 n 255)))

  (define-datatype token token?
    (tk-uint (n exact-integer?))
    (tk-nint (n exact-integer?))        ; the negative value itself
    (tk-bytes (bv bytevector?))
    (tk-bytes-begin)
    (tk-string (s string?))
    (tk-string-begin)
    (tk-list-len (n exact-integer?))
    (tk-list-begin)
    (tk-map-len (n exact-integer?))
    (tk-map-begin)
    (tk-tag (n exact-integer?))
    (tk-bool (b boolean?))
    (tk-null)
    (tk-undefined)
    (tk-simple (n byte?))
    (tk-float16 (x flonum?))
    (tk-float32 (x flonum?))
    (tk-float64 (x flonum?))
    (tk-break))

  ;; Returns TOK as a list of its variant name and fields, for example
  ;; (tk-uint 5), which is easy to print and compare.
  (define (token->list tok)
    (cases token tok
      (tk-uint (n) (list 'tk-uint n))
      (tk-nint (n) (list 'tk-nint n))
      (tk-bytes (bv) (list 'tk-bytes bv))
      (tk-bytes-begin () '(tk-bytes-begin))
      (tk-string (s) (list 'tk-string s))
      (tk-string-begin () '(tk-string-begin))
      (tk-list-len (n) (list 'tk-list-len n))
      (tk-list-begin () '(tk-list-begin))
      (tk-map-len (n) (list 'tk-map-len n))
      (tk-map-begin () '(tk-map-begin))
      (tk-tag (n) (list 'tk-tag n))
      (tk-bool (b) (list 'tk-bool b))
      (tk-null () '(tk-null))
      (tk-undefined () '(tk-undefined))
      (tk-simple (n) (list 'tk-simple n))
      (tk-float16 (x) (list 'tk-float16 x))
      (tk-float32 (x) (list 'tk-float32 x))
      (tk-float64 (x) (list 'tk-float64 x))
      (tk-break () '(tk-break))))

  ;; Writes one token to PORT.
  (define (write-token tok #!optional (port (current-output-port)))
    (cases token tok
      (tk-uint (n) (encode-uint n port))
      (tk-nint (n) (encode-nint n port))
      (tk-bytes (bv) (encode-bytes bv port))
      (tk-bytes-begin () (encode-bytes-begin port))
      (tk-string (s) (encode-string s port))
      (tk-string-begin () (encode-string-begin port))
      (tk-list-len (n) (encode-list-len n port))
      (tk-list-begin () (encode-list-begin port))
      (tk-map-len (n) (encode-map-len n port))
      (tk-map-begin () (encode-map-begin port))
      (tk-tag (n) (encode-tag n port))
      (tk-bool (b) (encode-bool b port))
      (tk-null () (encode-null port))
      (tk-undefined () (encode-undefined port))
      (tk-simple (n) (encode-simple n port))
      (tk-float16 (x) (encode-float16 x port))
      (tk-float32 (x) (encode-float32 x port))
      (tk-float64 (x) (encode-float64 x port))
      (tk-break () (encode-break port))))

  ;; Writes each token of the list TOKENS to PORT.
  (define (write-tokens tokens #!optional (port (current-output-port)))
    (for-each (lambda (tok) (write-token tok port)) tokens))

  ;; Reads one token with the decoder D, or returns an eof object at
  ;; the end of input.  Byte and text strings are read whole, so a
  ;; definite string is a single token.
  (define (read-token d)
    (case (peek-token-type d)
      ((eof) #!eof)
      ((uint) (tk-uint (decode-uint d)))
      ((nint) (tk-nint (decode-nint d)))
      ((bytes) (tk-bytes (decode-bytes d)))
      ((bytes-indef) (decode-bytes-indef d) (tk-bytes-begin))
      ((string) (tk-string (decode-string d)))
      ((string-indef) (decode-string-indef d) (tk-string-begin))
      ((list-len) (tk-list-len (decode-list-len d)))
      ((list-len-indef) (decode-list-len-indef d) (tk-list-begin))
      ((map-len) (tk-map-len (decode-map-len d)))
      ((map-len-indef) (decode-map-len-indef d) (tk-map-begin))
      ((tag) (tk-tag (decode-tag d)))
      ((bool) (tk-bool (decode-bool d)))
      ((null) (decode-null d) (tk-null))
      ((undefined) (decode-undefined d) (tk-undefined))
      ((simple) (tk-simple (decode-simple d)))
      ((float16 float32 float64)
       (let-values (((x width) (decode-float-token d)))
         (case width
           ((16) (tk-float16 x))
           ((32) (tk-float32 x))
           (else (tk-float64 x)))))
      ((break) (decode-break d) (tk-break))
      (else (cbor-error d 'malformed "invalid initial byte"))))

  ;; Reads every remaining token from PORT and returns them as a list.
  (define (read-tokens #!optional (port (current-input-port)))
    (let ((d (make-decoder port)))
      (let loop ((acc '()))
        (let ((tok (read-token d)))
          (if (eof-object? tok)
              (reverse acc)
              (loop (cons tok acc)))))))

  )
