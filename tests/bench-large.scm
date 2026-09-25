;;;; bench-large.scm - large-payload benchmark for the cbor egg
;;;
;;; Measures write time, read time and output size when serializing
;;; large SRFI-4 vectors and checkpoint-like structures, and checks
;;; that every element round-trips bit for bit.  One case runs per
;;; process, so that the peak memory of each case can be measured from
;;; outside (see bench-large.sh).
;;;
;;; Usage:
;;;   csi -s bench-large.scm KIND N MODE COMPRESS PATH
;;;
;;;   KIND      f64, f32 or model
;;;   N         number of elements (for model, total parameter count)
;;;   MODE      file, memory, baseline or truncate
;;;               file      write to PATH with write-cbor-file and read
;;;                         it back with read-cbor-file
;;;               memory    use cbor->bytevector / bytevector->cbor
;;;               baseline  build the data only, to measure its memory
;;;               truncate  write to PATH, cut the file in half, and
;;;                         check that reading it signals an error
;;;   COMPRESS  0 or 1 (compress with cbor-deflate)
;;;   PATH      output file for the file and truncate modes
;;;
;;; Prints one line of KEY=VALUE fields.

(import scheme
        (chicken base)
        (chicken blob)
        (chicken condition)
        (chicken file)
        (chicken file posix)
        (chicken format)
        (chicken gc)
        (scheme base)
        (chicken process-context)
        (chicken random)
        (scheme time)
        (srfi 4)
        cbor
        cbor-deflate)

;;; Test data

;; Fills a blob with random bytes and plants the special values -0.0,
;; +inf, -inf, NaN and a subnormal at the start, so that every byte
;; value and every class of floating point value occurs.
(define (make-random-f64vector n)
  (let* ((blob (random-bytes (make-blob (* 8 n))))
         (v (blob->f64vector/shared blob))
         (specials (list -0.0 +inf.0 -inf.0 +nan.0 4.9e-324 1.0 -1.0)))
    (let loop ((i 0) (s specials))
      (when (and (< i n) (pair? s))
        (f64vector-set! v i (car s))
        (loop (+ i 1) (cdr s))))
    v))

(define (make-random-f32vector n)
  (let* ((blob (random-bytes (make-blob (* 4 n))))
         (v (blob->f32vector/shared blob))
         (specials (list -0.0 +inf.0 -inf.0 +nan.0 1.4e-45 1.0 -1.0)))
    (let loop ((i 0) (s specials))
      (when (and (< i n) (pair? s))
        (f32vector-set! v i (car s))
        (loop (+ i 1) (cdr s))))
    v))

;; Builds a tensor record shaped like the output of
;; tensor->serializable in nanograd's layer.scm.
(define (tensor-record shape)
  (let ((n (apply * shape)))
    `((dtype . f32)
      (shape . ,shape)
      (requires-grad . #t)
      (data . ,(make-random-f32vector n)))))

;; Builds a sequential model of 25 dense layers, each with a weight
;; tensor and a bias tensor (50 tensors in all), whose parameter
;; count is close to N.  Layer widths alternate between two sizes so
;; that tensor shapes differ.
(define (make-model n)
  (let* ((layers 25)
         (per-layer (max 2 (quotient n layers)))
         (out (max 1 (inexact->exact (floor (sqrt per-layer)))))
         (in (max 1 (quotient (- per-layer out) out))))
    `((type . sequential)
      (name . "Model")
      (layers
       . ,(let loop ((i 0) (acc '()))
            (if (= i layers)
                (reverse acc)
                (let ((o (if (even? i) out (+ out 1)))
                      (k (if (even? i) in (- in 1))))
                  (loop (+ i 1)
                        (cons `((type . dense)
                                (name . ,(sprintf "Dense~A" i))
                                (input-size . ,k)
                                (output-size . ,o)
                                (activation (type . activation)
                                            (name . "ReLU"))
                                (weights . ,(tensor-record (list o k)))
                                (biases . ,(tensor-record (list o))))
                              acc)))))))))

(define (make-data kind n)
  (case kind
    ((f64) (make-random-f64vector n))
    ((f32) (make-random-f32vector n))
    ((model) (make-model n))
    (else (error 'bench-large "unknown kind" kind))))

;; Number of payload bytes held in SRFI-4 vectors inside X.
(define (payload-bytes x)
  (cond ((f64vector? x) (* 8 (f64vector-length x)))
        ((f32vector? x) (* 4 (f32vector-length x)))
        ((pair? x) (+ (payload-bytes (car x)) (payload-bytes (cdr x))))
        (else 0)))

;;; Bit-exact comparison

;; Compares two structures, treating SRFI-4 vectors as equal when
;; their bytes are identical.  This counts NaN and -0.0 as equal to
;; themselves, which numeric comparison would not.
(define (bits-equal? a b)
  (cond ((f64vector? a)
         (and (f64vector? b)
              (equal? (f64vector->blob/shared a) (f64vector->blob/shared b))))
        ((f32vector? a)
         (and (f32vector? b)
              (equal? (f32vector->blob/shared a) (f32vector->blob/shared b))))
        ((pair? a)
         (and (pair? b) (bits-equal? (car a) (car b)) (bits-equal? (cdr a) (cdr b))))
        (else (equal? a b))))

;;; Timing

;; Calls THUNK and returns its value and the elapsed wall-clock time in
;; milliseconds.
(define (timed thunk)
  (let* ((t0 (current-jiffy))
         (v (thunk))
         (t1 (current-jiffy)))
    (values v (quotient (* 1000 (- t1 t0)) (jiffies-per-second)))))

;;; Cases

(define (wrap obj compress) (if compress (cbor-deflated obj) obj))

(define (write-file obj path compress)
  (write-cbor-file path (wrap obj compress)))

(define (read-file path)
  (read-cbor-file path))

;; Cuts the file at PATH to half its size by copying its first half.
(define (truncate-file! path)
  (let* ((size (file-size path))
         (half (quotient size 2))
         (tmp (string-append path ".half")))
    (let ((bytes (with-input-from-file path
                   (lambda () (read-bytevector half (current-input-port))))))
      (with-output-to-file tmp
        (lambda () (write-bytevector bytes (current-output-port)))))
    (rename-file tmp path #t)))

(define (run kind n mode compress path)
  (let* ((data (make-data kind n))
         (payload (payload-bytes data)))
    (gc #t)
    (case mode
      ((baseline)
       (printf "kind=~A n=~A mode=baseline payload=~A~%" kind n payload))
      ((file)
       (let-values (((_ tw) (timed (lambda () (write-file data path compress)))))
         (let ((size (file-size path)))
           (let-values (((back tr) (timed (lambda () (read-file path)))))
             (printf "kind=~A n=~A mode=file compress=~A payload=~A size=~A ratio=~A write-ms=~A read-ms=~A exact=~A~%"
                     kind n compress payload size
                     (/ (round (* 1000.0 (/ size payload))) 1000.0)
                     tw tr (bits-equal? data back))
             (delete-file* path)))))
      ((memory)
       (let-values (((bv tw) (timed (lambda () (cbor->bytevector (wrap data compress))))))
         (let ((size (bytevector-length bv)))
           (let-values (((back tr) (timed (lambda () (bytevector->cbor bv)))))
             (printf "kind=~A n=~A mode=memory compress=~A payload=~A size=~A write-ms=~A read-ms=~A exact=~A~%"
                     kind n compress payload size tw tr (bits-equal? data back))))))
      ((truncate)
       (write-file data path compress)
       (truncate-file! path)
       (let ((result
              (condition-case (begin (read-file path) "no-error")
                (e (exn) (sprintf "error:~A"
                                ((condition-property-accessor 'exn 'message) e))))))
         (printf "kind=~A n=~A mode=truncate compress=~A result=~S~%"
                 kind n compress result)
         (delete-file* path)))
      (else (error 'bench-large "unknown mode" mode)))))

(let ((args (command-line-arguments)))
  (unless (= (length args) 5)
    (error 'bench-large "usage: KIND N MODE COMPRESS PATH"))
  (run (string->symbol (list-ref args 0))
       (string->number (list-ref args 1))
       (string->symbol (list-ref args 2))
       (string=? (list-ref args 3) "1")
       (list-ref args 4)))
