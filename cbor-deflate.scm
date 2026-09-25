;;;; cbor-deflate.scm - compressed CBOR data items
;;;
;;; Adds a tag for compressed content.  The content of the tag is the
;;; array [algorithm, length, data]: algorithm 1 means zlib (RFC 1950),
;;; length is the size in bytes of the uncompressed encoding, and data
;;; is a byte string holding the compressed encoding of exactly one
;;; CBOR data item.  Writers emit the data as an indefinite-length byte
;;; string of chunks, so compression runs in bounded memory; readers
;;; accept definite and indefinite byte strings.  Loading this module
;;; registers the tag, after which read-cbor expands compressed items
;;; transparently.  Compression uses the bundled miniz library.

(module cbor-deflate

  (tag-compressed
   cbor-deflated cbor-deflated? cbor-deflated-value
   cbor-deflate-level
   write-cbor/compressed
   deflate-bytevector inflate-bytevector)

  (import scheme
          (scheme base)
          (chicken base)
          (chicken condition)
          (chicken foreign)
          (srfi 4)
          cbor-core
          cbor)

  #>
  #include "miniz.c"

  /* A compression or decompression stream together with the outcome
     of its most recent step. */
  typedef struct {
    mz_stream s;
    long consumed;
    int status;
  } cbor_zstream;

  static void *cbor_deflate_new(int level) {
    cbor_zstream *z = calloc(1, sizeof(cbor_zstream));
    if (z == NULL) return NULL;
    if (mz_deflateInit(&z->s, level) != MZ_OK) { free(z); return NULL; }
    return z;
  }

  static void *cbor_inflate_new(void) {
    cbor_zstream *z = calloc(1, sizeof(cbor_zstream));
    if (z == NULL) return NULL;
    if (mz_inflateInit(&z->s) != MZ_OK) { free(z); return NULL; }
    return z;
  }

  /* Runs one step with SRC_LEN input bytes at SRC + SRC_OFF and room
     for DST_LEN output bytes at DST + DST_OFF.  Returns the number of
     bytes produced; the bytes consumed and the miniz status code are
     kept in the stream.  The buffers are Scheme objects that may move
     between calls, so the stream pointers are set afresh each time. */
  static long cbor_zstep(void *p, int deflating,
                         unsigned char *src, long src_off, long src_len,
                         unsigned char *dst, long dst_off, long dst_len) {
    cbor_zstream *z = (cbor_zstream *)p;
    int rc;
    z->s.next_in = src + src_off;
    z->s.avail_in = (unsigned int)src_len;
    z->s.next_out = dst + dst_off;
    z->s.avail_out = (unsigned int)dst_len;
    rc = deflating ? mz_deflate(&z->s, MZ_FINISH) : mz_inflate(&z->s, MZ_SYNC_FLUSH);
    z->consumed = src_len - (long)z->s.avail_in;
    z->status = rc;
    return dst_len - (long)z->s.avail_out;
  }

  static long cbor_zconsumed(void *p) { return ((cbor_zstream *)p)->consumed; }
  static int cbor_zstatus(void *p) { return ((cbor_zstream *)p)->status; }

  static void cbor_zfree(void *p, int deflating) {
    cbor_zstream *z = (cbor_zstream *)p;
    if (deflating) mz_deflateEnd(&z->s); else mz_inflateEnd(&z->s);
    free(z);
  }
  <#

  (define %deflate-new (foreign-lambda c-pointer "cbor_deflate_new" int))
  (define %inflate-new (foreign-lambda c-pointer "cbor_inflate_new"))
  (define %zstep
    (foreign-lambda long "cbor_zstep" c-pointer bool
                    nonnull-scheme-pointer long long
                    nonnull-scheme-pointer long long))
  (define %zconsumed (foreign-lambda long "cbor_zconsumed" c-pointer))
  (define %zstatus (foreign-lambda int "cbor_zstatus" c-pointer))
  (define %zfree (foreign-lambda void "cbor_zfree" c-pointer bool))

  (define mz-ok 0)
  (define mz-stream-end 1)
  (define mz-buf-error -5)

  ;; miniz counts buffer space in unsigned ints, so each step handles
  ;; at most this many bytes of input or output.
  (define max-step (* 1024 1024 1024))

  ;; Size of the compressed chunks that writers emit.
  (define chunk-size (* 1024 1024))

  ;; Deflate cannot expand data by more than this factor, which bounds
  ;; the uncompressed length that a reader accepts for a given amount
  ;; of compressed data.
  (define max-expansion 1032)

  ;; Tag of a compressed item, in the first-come-first-served range of
  ;; the IANA tag registry.
  (define tag-compressed #x5C4E10)

  ;; Compression level from 0 (store only) to 10 (best); 1 is fastest.
  (define cbor-deflate-level (make-parameter 6))

  ;; Wraps a value so that encoding writes it as a compressed item.
  ;; The wrapper may appear anywhere inside a larger value.
  (define-record-type cbor-deflated-type
    (cbor-deflated value)
    cbor-deflated?
    (value cbor-deflated-value))

  (define (with-stream make deflating proc)
    (let ((z (make)))
      (unless z (error 'cbor-deflate "cannot allocate a compression stream"))
      (dynamic-wind
          (lambda () #f)
          (lambda () (proc z))
          (lambda () (%zfree z deflating)))))

  ;; Compresses the bytevector SRC as one zlib stream and calls EMIT
  ;; with each output chunk: a bytevector and the number of bytes used
  ;; in it.  The chunk buffer is reused between calls.
  (define (deflate-chunks src emit)
    (let ((n (bytevector-length src))
          (buf (make-bytevector chunk-size)))
      (with-stream
       (lambda () (%deflate-new (cbor-deflate-level))) #t
       (lambda (z)
         (let loop ((off 0))
           (let* ((produced (%zstep z #t src off (min max-step (- n off)) buf 0 chunk-size))
                  (off (+ off (%zconsumed z)))
                  (status (%zstatus z)))
             (when (> produced 0) (emit buf produced))
             (cond ((= status mz-stream-end) #t)
                   ((and (or (= status mz-ok) (= status mz-buf-error))
                         (or (> produced 0) (> (%zconsumed z) 0)))
                    (loop off))
                   (else (error 'cbor-deflate "compression failed" status)))))))))

  ;; Returns the zlib compression of the bytevector SRC.
  (define (deflate-bytevector src)
    (let ((chunks '()))
      (deflate-chunks src (lambda (buf k)
                            (let ((c (make-bytevector k)))
                              (bytevector-copy! c 0 buf 0 k)
                              (set! chunks (cons c chunks)))))
      (apply bytevector-append (reverse chunks))))

  ;; Decompresses the zlib stream in the bytevector SRC, whose
  ;; uncompressed size must be exactly LENGTH bytes.
  (define (inflate-bytevector src length)
    (let ((n (bytevector-length src))
          (out (make-bytevector length)))
      (with-stream
       %inflate-new #f
       (lambda (z)
         (let loop ((in 0) (done 0))
           (let* ((produced (%zstep z #f src in (min max-step (- n in))
                                    out done (min max-step (- length done))))
                  (in (+ in (%zconsumed z)))
                  (done (+ done produced))
                  (status (%zstatus z)))
             (cond ((= status mz-stream-end)
                    (unless (= done length)
                      (cbor-error #f 'malformed "compressed item holds ~A bytes, not ~A"
                                  done length))
                    out)
                   ((and (or (= status mz-ok) (= status mz-buf-error))
                         (or (> produced 0) (> (%zconsumed z) 0)))
                    (loop in done))
                   ((= done length)
                    (cbor-error #f 'malformed "compressed item is longer than ~A bytes" length))
                   (else
                    (cbor-error #f 'malformed "corrupt or truncated compressed item")))))))))

  ;; Writes the compressed item for VALUE to PORT.
  (define (encode-deflated obj emit port)
    (let ((encoded (cbor->bytevector (cbor-deflated-value obj))))
      (encode-list-len 3 port)
      (encode-uint 1 port)
      (encode-uint (bytevector-length encoded) port)
      (encode-bytes-begin port)
      (deflate-chunks encoded (lambda (buf k) (encode-bytes buf port 0 k)))
      (encode-break port)))

  ;; Expands the content of a compressed item and decodes the value
  ;; inside it.
  (define (decode-deflated content)
    (unless (and (list? content) (= (length content) 3)
                 (exact-integer? (car content))
                 (exact-integer? (cadr content)) (>= (cadr content) 0)
                 (bytevector? (caddr content)))
      (cbor-error #f 'malformed "compressed item is not [algorithm, length, data]"))
    (let ((algorithm (car content))
          (length (cadr content))
          (data (caddr content)))
      (unless (= algorithm 1)
        (cbor-error #f 'malformed "unknown compression algorithm ~A" algorithm))
      (when (> length (* max-expansion (+ 1 (bytevector-length data))))
        (cbor-error #f 'malformed "compressed item claims an impossible length ~A" length))
      (let ((limit (cbor-max-length)))
        (when (and limit (> length limit))
          (cbor-error #f 'limit "compressed item of ~A bytes exceeds the limit of ~A"
                      length limit)))
      (bytevector->cbor (inflate-bytevector data length))))

  (register-cbor-codec!
   (make-cbor-codec tag-compressed cbor-deflated? encode-deflated decode-deflated))

  ;; Writes OBJ to PORT as one compressed data item.
  (define (write-cbor/compressed obj #!optional (port (current-output-port)))
    (write-cbor (cbor-deflated obj) port))

  )
