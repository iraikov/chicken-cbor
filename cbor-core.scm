;;;; cbor-core.scm - low-level CBOR encoder and decoder (RFC 8949)
;;;
;;; Writes and reads the individual tokens of the Concise Binary
;;; Object Representation on binary ports: integers, byte and text
;;; strings, array and map headers, tags, simple values and floats.
;;; Higher layers (cbor-term, cbor) build complete data items from
;;; these tokens.

(module cbor-core

  (;; Encoding
   encode-head
   encode-uint encode-nint encode-int
   encode-bytes encode-bytes-begin
   encode-string encode-string-begin
   encode-list-len encode-list-begin
   encode-map-len encode-map-begin
   encode-tag
   encode-bool encode-null encode-undefined encode-simple
   encode-float16 encode-float32 encode-float64 encode-float
   encode-break encode-encoded write-block

   ;; Half-precision conversion
   flonum->half-bits half-bits->flonum

   ;; Decoding
   make-decoder decoder? decoder-port decoder-offset
   peek-token-type
   decode-uint decode-nint decode-int decode-integer
   decode-bytes decode-string decode-bytes-indef decode-string-indef
   decode-list-len decode-list-len-indef decode-list-len-or-indef
   decode-map-len decode-map-len-indef decode-map-len-or-indef
   decode-tag
   decode-bool decode-null decode-undefined decode-simple
   decode-float decode-float-token
   decode-break decode-break-or
   decode-skip
   decoder-enter! decoder-leave!
   cbor-error

   ;; Limits
   cbor-max-depth cbor-max-length)

  (import scheme
          (scheme base)
          (chicken base)
          (chicken bitwise)
          (chicken blob)
          (chicken condition)
          (chicken format)
          (srfi 4))

  ;;; ================================================================
  ;;; Limits
  ;;; ================================================================

  ;; Deepest nesting of arrays, maps and tags that decoders accept.
  (define cbor-max-depth (make-parameter 1024))

  ;; Largest byte string, text string, array or map (in bytes or
  ;; items) that decoders accept, or #f for no limit.
  (define cbor-max-length (make-parameter #f))

  ;; Byte strings longer than this are read in two steps, so that a
  ;; forged length cannot force a huge allocation before the data
  ;; has actually arrived.
  (define large-payload-threshold (* 64 1024 1024))

  ;;; ================================================================
  ;;; Encoding
  ;;; ================================================================

  ;; Writes the N low-order bytes of the exact integer V to PORT, most
  ;; significant byte first.
  (define (write-be-bytes v n port)
    (let loop ((shift (* 8 (- n 1))))
      (when (>= shift 0)
        (write-u8 (bitwise-and #xff (arithmetic-shift v (- shift))) port)
        (loop (- shift 8)))))

  ;; Writes an initial byte and argument: MAJOR is the major type
  ;; (0-7) and ARG an unsigned integer below 2^64.  The argument is
  ;; written in the shortest form, as RFC 8949 preferred serialization
  ;; requires.
  (define (encode-head major arg port)
    (let ((mt (arithmetic-shift major 5)))
      (cond ((< arg 24) (write-u8 (bitwise-ior mt arg) port))
            ((< arg #x100)
             (write-u8 (bitwise-ior mt 24) port)
             (write-u8 arg port))
            ((< arg #x10000)
             (write-u8 (bitwise-ior mt 25) port)
             (write-be-bytes arg 2 port))
            ((< arg #x100000000)
             (write-u8 (bitwise-ior mt 26) port)
             (write-be-bytes arg 4 port))
            ((< arg #x10000000000000000)
             (write-u8 (bitwise-ior mt 27) port)
             (write-be-bytes arg 8 port))
            (else (error 'encode-head "argument does not fit in 64 bits" arg)))))

  ;; Writes a non-negative integer below 2^64 (major type 0).
  (define (encode-uint n #!optional (port (current-output-port)))
    (unless (and (exact-integer? n) (>= n 0))
      (error 'encode-uint "not a non-negative exact integer" n))
    (encode-head 0 n port))

  ;; Writes a negative integer no smaller than -2^64 (major type 1).
  (define (encode-nint n #!optional (port (current-output-port)))
    (unless (and (exact-integer? n) (< n 0))
      (error 'encode-nint "not a negative exact integer" n))
    (encode-head 1 (- -1 n) port))

  ;; Writes any exact integer.  Integers outside the 64-bit range of
  ;; major types 0 and 1 become bignums: tag 2 or 3 applied to the
  ;; big-endian bytes of the magnitude.
  (define (encode-int n #!optional (port (current-output-port)))
    (cond ((not (exact-integer? n))
           (error 'encode-int "not an exact integer" n))
          ((and (>= n 0) (< n #x10000000000000000)) (encode-head 0 n port))
          ((and (< n 0) (>= n (- #x10000000000000000))) (encode-head 1 (- -1 n) port))
          (else
           (let* ((neg (< n 0))
                  (mag (if neg (- -1 n) n))
                  (len (quotient (+ (integer-length mag) 7) 8))
                  (bv (make-bytevector len 0)))
             (let loop ((i (- len 1)) (v mag))
               (when (>= i 0)
                 (bytevector-u8-set! bv i (bitwise-and v #xff))
                 (loop (- i 1) (arithmetic-shift v -8))))
             (encode-head 6 (if neg 3 2) port)
             (encode-bytes bv port)))))

  ;; Largest block handed to write-bytevector in one call.  Some
  ;; CHICKEN 6 port types mishandle a single write that is larger than
  ;; their internal buffer; pieces of this size are safe on every port
  ;; type and add little cost on file ports.
  (define write-piece-size 256)

  ;; Writes bytes START to END of the bytevector BV to PORT.
  (define (write-block bv port start end)
    (let loop ((i start))
      (when (< i end)
        (let ((j (min end (+ i write-piece-size))))
          (write-bytevector bv port i j)
          (loop j)))))

  ;; Writes a byte string (major type 2) holding bytes START to END of
  ;; the bytevector BV.
  (define (encode-bytes bv #!optional (port (current-output-port))
                        (start 0) (end (bytevector-length bv)))
    (encode-head 2 (- end start) port)
    (write-block bv port start end))

  ;; Starts an indefinite-length byte string.  Each chunk is then
  ;; written with encode-bytes, and encode-break ends the string.
  (define (encode-bytes-begin #!optional (port (current-output-port)))
    (write-u8 #x5f port))

  ;; Writes a text string (major type 3) as its UTF-8 bytes.
  (define (encode-string s #!optional (port (current-output-port)))
    (let ((bv (string->utf8 s)))
      (encode-head 3 (bytevector-length bv) port)
      (write-block bv port 0 (bytevector-length bv))))

  ;; Starts an indefinite-length text string made of encode-string
  ;; chunks and ended by encode-break.
  (define (encode-string-begin #!optional (port (current-output-port)))
    (write-u8 #x7f port))

  ;; Writes the header of an array of N items (major type 4).
  (define (encode-list-len n #!optional (port (current-output-port)))
    (encode-head 4 n port))

  ;; Starts an indefinite-length array, ended by encode-break.
  (define (encode-list-begin #!optional (port (current-output-port)))
    (write-u8 #x9f port))

  ;; Writes the header of a map of N key/value pairs (major type 5).
  (define (encode-map-len n #!optional (port (current-output-port)))
    (encode-head 5 n port))

  ;; Starts an indefinite-length map, ended by encode-break.
  (define (encode-map-begin #!optional (port (current-output-port)))
    (write-u8 #xbf port))

  ;; Writes tag number N (major type 6); the tagged item follows.
  (define (encode-tag n #!optional (port (current-output-port)))
    (encode-head 6 n port))

  (define (encode-bool b #!optional (port (current-output-port)))
    (write-u8 (if b #xf5 #xf4) port))

  (define (encode-null #!optional (port (current-output-port)))
    (write-u8 #xf6 port))

  (define (encode-undefined #!optional (port (current-output-port)))
    (write-u8 #xf7 port))

  ;; Writes simple value N.  Values 0-23 use one byte; 32-255 use two.
  ;; Values 24-31 are reserved and cannot be written.
  (define (encode-simple n #!optional (port (current-output-port)))
    (cond ((and (>= n 0) (< n 24)) (write-u8 (bitwise-ior #xe0 n) port))
          ((and (>= n 32) (< n 256)) (write-u8 #xf8 port) (write-u8 n port))
          (else (error 'encode-simple "invalid simple value" n))))

  ;; Ends an indefinite-length item.
  (define (encode-break #!optional (port (current-output-port)))
    (write-u8 #xff port))

  ;; Writes the bytevector BV unchanged; it must already hold one or
  ;; more complete encoded CBOR items.
  (define (encode-encoded bv #!optional (port (current-output-port)))
    (write-block bv port 0 (bytevector-length bv)))

  ;; Writes the bytes of the native-order blob B to PORT in network
  ;; (big-endian) order.
  (define (write-blob-be b port)
    (let ((n (bytevector-length b)))
      (cond-expand
        (little-endian
         (let loop ((i (- n 1)))
           (when (>= i 0)
             (write-u8 (bytevector-u8-ref b i) port)
             (loop (- i 1)))))
        (else (write-bytevector b port)))))

  ;; Writes X as an IEEE 754 double (initial byte #xfb).
  (define (encode-float64 x #!optional (port (current-output-port)))
    (let ((v (make-f64vector 1 (exact->inexact x))))
      (write-u8 #xfb port)
      (write-blob-be (f64vector->blob/shared v) port)))

  ;; Writes X as an IEEE 754 single (initial byte #xfa), rounding it
  ;; to single precision.
  (define (encode-float32 x #!optional (port (current-output-port)))
    (let ((v (make-f32vector 1 (exact->inexact x))))
      (write-u8 #xfa port)
      (write-blob-be (f32vector->blob/shared v) port)))

  ;; Writes X as an IEEE 754 half (initial byte #xf9), rounding it to
  ;; half precision.
  (define (encode-float16 x #!optional (port (current-output-port)))
    (write-u8 #xf9 port)
    (write-be-bytes (flonum->half-bits x) 2 port))

  ;; Writes X in the shortest of half, single and double precision
  ;; that represents it exactly (RFC 8949 preferred serialization).
  ;; NaN is written as the canonical half-precision NaN.
  (define (encode-float x #!optional (port (current-output-port)))
    (let ((x (exact->inexact x)))
      (cond ((nan? x) (write-u8 #xf9 port) (write-be-bytes #x7e00 2 port))
            ((half-exact? x) (encode-float16 x port))
            ((single-exact? x) (encode-float32 x port))
            (else (encode-float64 x port)))))

  (define (negative-flonum? x)
    (or (< x 0.0) (eqv? x -0.0)))

  (define (single-exact? x)
    (let ((v (make-f32vector 1 x)))
      (= (f32vector-ref v 0) x)))

  (define (half-exact? x)
    (let ((y (half-bits->flonum (flonum->half-bits x))))
      (and (= x y) (eq? (negative-flonum? x) (negative-flonum? y)))))

  ;; Converts the flonum X to the bit pattern of the nearest IEEE 754
  ;; half-precision value, rounding ties to even.  Returns an exact
  ;; integer in 0..65535.
  (define (flonum->half-bits x)
    (let* ((x (exact->inexact x))
           (sign (if (negative-flonum? x) #x8000 0)))
      (cond ((nan? x) #x7e00)
            ((infinite? x) (bitwise-ior sign #x7c00))
            ((zero? x) sign)
            (else
             (let ((q (exact (abs x))))
               (cond
                ((>= q 65520) (bitwise-ior sign #x7c00))
                ;; Subnormal range: the value is a multiple of 2^-24.
                ;; A result of 1024 is the smallest normal number,
                ;; whose bit pattern is the same integer.
                ((< q 1/16384)
                 (bitwise-ior sign (round (* q 16777216))))
                (else
                 (let* ((e0 (- (integer-length (numerator q))
                               (integer-length (denominator q))))
                        (e (if (< q (expt 2 e0)) (- e0 1) e0))
                        (m (round (/ q (expt 2 (- e 10)))))
                        (e (if (= m 2048) (+ e 1) e))
                        (m (if (= m 2048) 1024 m)))
                   (if (>= (+ e 15) 31)
                       (bitwise-ior sign #x7c00)
                       (bitwise-ior sign
                                    (arithmetic-shift (+ e 15) 10)
                                    (- m 1024)))))))))))

  ;; Converts a half-precision bit pattern to a flonum (RFC 8949
  ;; Appendix D).
  (define (half-bits->flonum h)
    (let* ((exp (bitwise-and (arithmetic-shift h -10) #x1f))
           (mant (bitwise-and h #x3ff))
           (val (cond ((= exp 0) (* (exact->inexact mant) (expt 2.0 -24)))
                      ((= exp 31) (if (= mant 0) +inf.0 +nan.0))
                      (else (* (exact->inexact (+ mant 1024))
                               (expt 2.0 (- exp 25)))))))
      (if (= 0 (bitwise-and h #x8000)) val (- val))))

  ;;; ================================================================
  ;;; Decoding
  ;;; ================================================================

  ;; A decoder reads tokens from a binary input port.  It keeps its own
  ;; one-byte lookahead instead of peeking the port, counts the bytes
  ;; consumed so that errors can report their position, and tracks the
  ;; nesting depth of the item being decoded.
  (define-record-type decoder
    (%make-decoder port lookahead offset depth)
    decoder?
    (port decoder-port)
    (lookahead decoder-lookahead decoder-lookahead-set!)
    (offset decoder-offset decoder-offset-set!)
    (depth decoder-depth decoder-depth-set!))

  ;; Returns a decoder reading from PORT.
  (define (make-decoder #!optional (port (current-input-port)))
    (%make-decoder port #f 0 0))

  ;; Signals a condition of kinds exn and cbor.  REASON is a symbol
  ;; naming the class of error (eof, malformed, type, limit, utf8);
  ;; the cbor part carries the reason and the byte offset in D.
  (define (cbor-error d reason fmt . args)
    (abort
     (make-composite-condition
      (make-property-condition
       'exn
       'message (sprintf "CBOR ~A at byte ~A: ~A"
                         reason (if d (decoder-offset d) "?")
                         (apply sprintf fmt args))
       'arguments '()
       'location 'cbor)
      (make-property-condition
       'cbor
       'reason reason
       'offset (and d (decoder-offset d))))))

  ;; Returns the next byte, consuming it; signals an error at end of
  ;; input.
  (define (next-byte d)
    (let ((b (decoder-lookahead d)))
      (if b
          (begin (decoder-lookahead-set! d #f) b)
          (let ((b (read-u8 (decoder-port d))))
            (when (eof-object? b)
              (cbor-error d 'eof "unexpected end of input"))
            (decoder-offset-set! d (+ 1 (decoder-offset d)))
            b))))

  ;; Returns the next byte without consuming it, or an eof object at
  ;; the end of input.
  (define (peek-byte d)
    (or (decoder-lookahead d)
        (let ((b (read-u8 (decoder-port d))))
          (unless (eof-object? b)
            (decoder-offset-set! d (+ 1 (decoder-offset d)))
            (decoder-lookahead-set! d b))
          b)))

  ;; Reads an N-byte big-endian unsigned integer.
  (define (read-be-uint d n)
    (let loop ((i 0) (v 0))
      (if (= i n)
          v
          (loop (+ i 1) (bitwise-ior (arithmetic-shift v 8) (next-byte d))))))

  ;; Reads the argument that follows an initial byte with additional
  ;; information AI.  Returns the argument, or the symbol indefinite
  ;; for AI = 31.
  (define (read-argument d ai)
    (cond ((< ai 24) ai)
          ((= ai 24) (next-byte d))
          ((= ai 25) (read-be-uint d 2))
          ((= ai 26) (read-be-uint d 4))
          ((= ai 27) (read-be-uint d 8))
          ((= ai 31) 'indefinite)
          (else (cbor-error d 'malformed "reserved additional information ~A" ai))))

  ;; Names the token that starts with initial byte B, following the
  ;; token types of cborg: uint, nint, bytes, bytes-indef, string,
  ;; string-indef, list-len, list-len-indef, map-len, map-len-indef,
  ;; tag, bool, null, undefined, simple, float16, float32, float64,
  ;; break, invalid.
  (define (initial-byte-type b)
    (let ((major (arithmetic-shift b -5))
          (ai (bitwise-and b #x1f)))
      (if (and (>= ai 28) (< ai 31))
          'invalid
          (case major
            ((0) (if (= ai 31) 'invalid 'uint))
            ((1) (if (= ai 31) 'invalid 'nint))
            ((2) (if (= ai 31) 'bytes-indef 'bytes))
            ((3) (if (= ai 31) 'string-indef 'string))
            ((4) (if (= ai 31) 'list-len-indef 'list-len))
            ((5) (if (= ai 31) 'map-len-indef 'map-len))
            ((6) (if (= ai 31) 'invalid 'tag))
            (else
             (cond ((or (= ai 20) (= ai 21)) 'bool)
                   ((= ai 22) 'null)
                   ((= ai 23) 'undefined)
                   ((or (< ai 20) (= ai 24)) 'simple)
                   ((= ai 25) 'float16)
                   ((= ai 26) 'float32)
                   ((= ai 27) 'float64)
                   (else 'break)))))))

  ;; Returns the type of the next token (see initial-byte-type)
  ;; without consuming it, or the symbol eof at the end of input.
  (define (peek-token-type d)
    (let ((b (peek-byte d)))
      (if (eof-object? b) 'eof (initial-byte-type b))))

  ;; Signals a type error for initial byte B when EXPECTED was wanted.
  (define (type-error d b expected)
    (cbor-error d 'type "expected ~A, found ~A" expected (initial-byte-type b)))

  ;; Consumes an initial byte of major type MAJOR and returns its
  ;; argument (or indefinite).  WHAT names the expected token in
  ;; error messages.
  (define (read-head d major what)
    (let ((b (next-byte d)))
      (unless (= (arithmetic-shift b -5) major)
        (type-error d b what))
      (read-argument d (bitwise-and b #x1f))))

  (define (definite-argument d arg what)
    (when (eq? arg 'indefinite)
      (cbor-error d 'malformed "indefinite length not allowed for ~A" what))
    arg)

  (define (check-length d n what)
    (let ((limit (cbor-max-length)))
      (when (and limit (> n limit))
        (cbor-error d 'limit "~A of length ~A exceeds the limit of ~A" what n limit))
      n))

  ;; Decodes a non-negative integer (major type 0).
  (define (decode-uint d)
    (definite-argument d (read-head d 0 "unsigned integer") "an integer"))

  ;; Decodes a negative integer (major type 1).
  (define (decode-nint d)
    (- -1 (definite-argument d (read-head d 1 "negative integer") "an integer")))

  ;; Decodes an integer of major type 0 or 1.
  (define (decode-int d)
    (let ((b (next-byte d)))
      (case (arithmetic-shift b -5)
        ((0) (definite-argument d (read-argument d (bitwise-and b #x1f)) "an integer"))
        ((1) (- -1 (definite-argument d (read-argument d (bitwise-and b #x1f)) "an integer")))
        (else (type-error d b "integer")))))

  ;; Decodes an integer of major type 0 or 1, or a bignum (tag 2 or 3
  ;; applied to a byte string).
  (define (decode-integer d)
    (if (eq? (peek-token-type d) 'tag)
        (let ((tag (decode-tag d)))
          (unless (memv tag '(2 3))
            (cbor-error d 'type "expected bignum tag 2 or 3, found tag ~A" tag))
          (let* ((bv (decode-bytes d))
                 (n (bytevector-length bv))
                 (mag (let loop ((i 0) (v 0))
                        (if (= i n)
                            v
                            (loop (+ i 1)
                                  (bitwise-ior (arithmetic-shift v 8)
                                               (bytevector-u8-ref bv i)))))))
            (if (= tag 2) mag (- -1 mag))))
        (decode-int d)))

  ;; Reads exactly N payload bytes into a fresh bytevector.
  (define (read-payload d n)
    (let ((port (decoder-port d)))
      (define (fill! bv start)
        (let loop ((start start))
          (when (< start (bytevector-length bv))
            (let ((k (read-bytevector! bv port start)))
              (when (or (eof-object? k) (= k 0))
                (cbor-error d 'eof "unexpected end of input in a ~A-byte string" n))
              (decoder-offset-set! d (+ k (decoder-offset d)))
              (loop (+ start k))))))
      (if (<= n large-payload-threshold)
          (let ((bv (make-bytevector n)))
            (fill! bv 0)
            bv)
          (let ((head (make-bytevector large-payload-threshold)))
            (fill! head 0)
            (let ((bv (make-bytevector n)))
              (bytevector-copy! bv 0 head)
              (fill! bv large-payload-threshold)
              bv)))))

  ;; Concatenates a list of bytevectors.
  (define (concatenate-bytevectors chunks)
    (let* ((total (let loop ((c chunks) (n 0))
                    (if (null? c) n (loop (cdr c) (+ n (bytevector-length (car c)))))))
           (out (make-bytevector total)))
      (let loop ((c chunks) (i 0))
        (unless (null? c)
          (bytevector-copy! out i (car c))
          (loop (cdr c) (+ i (bytevector-length (car c))))))
      out))

  ;; Reads a definite or indefinite string of major type MAJOR (2 or
  ;; 3) and returns its bytes.  The chunks of an indefinite string
  ;; must be definite strings of the same major type.
  (define (read-string-bytes d major what)
    (let ((arg (read-head d major what)))
      (if (eq? arg 'indefinite)
          (let loop ((chunks '()) (total 0))
            (if (decode-break-or d)
                (concatenate-bytevectors (reverse chunks))
                (let ((b (next-byte d)))
                  (unless (= (arithmetic-shift b -5) major)
                    (cbor-error d 'malformed "chunk of type ~A inside an indefinite ~A"
                                (initial-byte-type b) what))
                  (let ((n (read-argument d (bitwise-and b #x1f))))
                    (when (eq? n 'indefinite)
                      (cbor-error d 'malformed "nested indefinite ~A" what))
                    (check-length d (+ total n) what)
                    (loop (cons (read-payload d n) chunks) (+ total n))))))
          (read-payload d (check-length d arg what)))))

  ;; Decodes a byte string (major type 2), definite or indefinite,
  ;; and returns a fresh bytevector.
  (define (decode-bytes d)
    (read-string-bytes d 2 "byte string"))

  ;; Decodes a text string (major type 3).  Signals an error if the
  ;; bytes are not valid UTF-8.
  (define (decode-string d)
    (let ((bv (read-string-bytes d 3 "text string")))
      (condition-case (utf8->string bv)
        ((exn) (cbor-error d 'utf8 "text string is not valid UTF-8")))))

  ;; Consumes the header of an indefinite-length byte string.  Its
  ;; chunks and the closing break follow as separate tokens.
  (define (decode-bytes-indef d)
    (let ((b (next-byte d)))
      (unless (= b #x5f) (type-error d b "indefinite byte string"))))

  ;; Consumes the header of an indefinite-length text string.
  (define (decode-string-indef d)
    (let ((b (next-byte d)))
      (unless (= b #x7f) (type-error d b "indefinite text string"))))

  (define (read-count d major what)
    (let ((arg (read-head d major what)))
      (if (eq? arg 'indefinite) #f (check-length d arg what))))

  ;; Decodes the header of a definite-length array and returns its
  ;; item count.
  (define (decode-list-len d)
    (or (read-count d 4 "array")
        (cbor-error d 'type "expected a definite-length array")))

  ;; Decodes the header of an indefinite-length array.
  (define (decode-list-len-indef d)
    (when (read-count d 4 "array")
      (cbor-error d 'type "expected an indefinite-length array")))

  ;; Decodes an array header; returns the item count, or #f for an
  ;; indefinite-length array.
  (define (decode-list-len-or-indef d)
    (read-count d 4 "array"))

  (define (decode-map-len d)
    (or (read-count d 5 "map")
        (cbor-error d 'type "expected a definite-length map")))

  (define (decode-map-len-indef d)
    (when (read-count d 5 "map")
      (cbor-error d 'type "expected an indefinite-length map")))

  ;; Decodes a map header; returns the pair count, or #f for an
  ;; indefinite-length map.
  (define (decode-map-len-or-indef d)
    (read-count d 5 "map"))

  ;; Decodes a tag and returns its number; the tagged item follows.
  (define (decode-tag d)
    (definite-argument d (read-head d 6 "tag") "a tag"))

  (define (decode-bool d)
    (let ((b (next-byte d)))
      (case b
        ((#xf4) #f)
        ((#xf5) #t)
        (else (type-error d b "bool")))))

  (define (decode-null d)
    (let ((b (next-byte d)))
      (unless (= b #xf6) (type-error d b "null"))))

  (define (decode-undefined d)
    (let ((b (next-byte d)))
      (unless (= b #xf7) (type-error d b "undefined"))))

  ;; Decodes any simple value, including false, true, null and
  ;; undefined, and returns its number (0-255).
  (define (decode-simple d)
    (let* ((b (next-byte d))
           (ai (bitwise-and b #x1f)))
      (unless (= (arithmetic-shift b -5) 7) (type-error d b "simple value"))
      (cond ((< ai 24) ai)
            ((= ai 24)
             (let ((n (next-byte d)))
               (when (< n 32)
                 (cbor-error d 'malformed "two-byte simple value ~A below 32" n))
               n))
            (else (type-error d b "simple value")))))

  ;; Reads N network-order bytes into a fresh native-order blob.
  (define (read-blob-be d n)
    (let ((b (make-bytevector n)))
      (cond-expand
        (little-endian
         (let loop ((i (- n 1)))
           (when (>= i 0)
             (bytevector-u8-set! b i (next-byte d))
             (loop (- i 1)))))
        (else
         (let loop ((i 0))
           (when (< i n)
             (bytevector-u8-set! b i (next-byte d))
             (loop (+ i 1))))))
      b))

  ;; Decodes a half, single or double float and returns it as a
  ;; flonum together with its width in bits, as two values.
  (define (decode-float-token d)
    (let ((b (next-byte d)))
      (case b
        ((#xf9) (values (half-bits->flonum (read-be-uint d 2)) 16))
        ((#xfa) (values (f32vector-ref (blob->f32vector/shared (read-blob-be d 4)) 0) 32))
        ((#xfb) (values (f64vector-ref (blob->f64vector/shared (read-blob-be d 8)) 0) 64))
        (else (type-error d b "float")))))

  ;; Decodes a half, single or double float and returns it as a
  ;; flonum.
  (define (decode-float d)
    (let-values (((x width) (decode-float-token d))) x))

  ;; Consumes the break that ends an indefinite-length item.
  (define (decode-break d)
    (let ((b (next-byte d)))
      (unless (= b #xff) (type-error d b "break"))))

  ;; Consumes a break and returns #t if one comes next; otherwise
  ;; returns #f and consumes nothing.
  (define (decode-break-or d)
    (let ((b (peek-byte d)))
      (cond ((eof-object? b) (cbor-error d 'eof "unexpected end of input, expected break"))
            ((= b #xff) (decoder-lookahead-set! d #f) #t)
            (else #f))))

  ;; Enters one level of nesting, signalling an error beyond
  ;; cbor-max-depth.  Every call is paired with decoder-leave!.
  (define (decoder-enter! d)
    (let ((depth (+ 1 (decoder-depth d))))
      (when (> depth (cbor-max-depth))
        (cbor-error d 'limit "nesting deeper than ~A" (cbor-max-depth)))
      (decoder-depth-set! d depth)))

  (define (decoder-leave! d)
    (decoder-depth-set! d (- (decoder-depth d) 1)))

  ;; Consumes one complete data item without building it.
  (define (decode-skip d)
    (let ((type (peek-token-type d)))
      (case type
        ((uint nint) (decode-int d))
        ((bytes bytes-indef) (decode-bytes d))
        ((string string-indef) (read-string-bytes d 3 "text string"))
        ((list-len list-len-indef map-len map-len-indef)
         (let* ((map? (memq type '(map-len map-len-indef)))
                (n (if map? (decode-map-len-or-indef d) (decode-list-len-or-indef d)))
                (per (if map? 2 1)))
           (decoder-enter! d)
           (if n
               (do ((i 0 (+ i 1))) ((= i (* per n))) (decode-skip d))
               (let loop ()
                 (unless (decode-break-or d)
                   (decode-skip d)
                   (loop))))
           (decoder-leave! d)))
        ((tag)
         (decode-tag d)
         (decoder-enter! d)
         (decode-skip d)
         (decoder-leave! d))
        ((bool null undefined simple) (decode-simple d))
        ((float16 float32 float64) (decode-float d))
        ((break) (cbor-error d 'malformed "break outside an indefinite-length item"))
        ((eof) (cbor-error d 'eof "unexpected end of input"))
        (else (cbor-error d 'malformed "invalid initial byte ~A" (peek-byte d))))))

  )
