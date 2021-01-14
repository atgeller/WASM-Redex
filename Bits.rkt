#lang racket

(require redex/reduction-semantics
         bitsyntax
         "Syntax.rkt"
         "MachineOps.rkt")

(provide (all-defined-out))

(define endianess #f)
(define max-memory-size 8192) ;; Arbitrary

(define memory-size bit-string-byte-count)

(define (make-memory size)
  (bit-string (0 :: little-endian bytes size)))

(define (grow-memory mem newsize)
  (if (<= 0 newsize max-memory-size)
      (cons (bit-string-pack (bit-string-append mem (make-memory newsize)))
            (+ (memory-size mem) newsize))
      (cons mem -1)))

(define (type-width type)
  (match type
    [`i8 8]
    [`i16 16]
    [`i32 32]
    [`i64 64]
    [`f32 32]
    [`f64 64]))

(define (integer-type? t)
  (match t
    [`i32 #t]
    [`i64 #t]
    [`f32 #f]
    [`f64 #f]))

(define (floating-type? t)
  (match t
    [`i32 #f]
    [`i64 #f]
    [`f32 #t]
    [`f64 #t]))

;; TODO: I don't think we need bitsyntax, can just use Racket bytestrings
;; TODO: load/store floating point numbers

(define (valid-bit-index? mem bit-index width)
  (<= 0 bit-index (+ bit-index width) (bit-string-length mem)))

(define (wrapped-load-packed mem type align index tp sign)
  (let ([width (type-width tp)]
        [bit-index (* 8 index)])
    (if (valid-bit-index? mem bit-index width)
        (term (,type const ,(load mem bit-index width
                                  (match sign
                                    [`signed (compose (curry to-unsigned-sized (type-width type))
                                                      bit-string->signed-integer)]
                                    [`unsigned bit-string->unsigned-integer]))))
        (term (trap)))))

(define (wrapped-load mem type align index)
  (let ([width (type-width type)]
        [bit-index (* 8 index)])
    (if (valid-bit-index? mem bit-index width)
        (term (,type const ,(load mem bit-index width
                                  (match type
                                    [`i32 bit-string->unsigned-integer]
                                    [`i64 bit-string->unsigned-integer]
                                    [`f32 (lambda (val end) (real->single-flonum (real->floating-point-bytes val 4 end)))]
                                    [`f64 (lambda (val end) (real->floating-point-bytes val 8 end))]))))
        (term (trap)))))

;; Shouldn't really be provided, but useful for testing and debugging
(define (load mem offset width conversionfn)
  (conversionfn (sub-bit-string mem offset (+ offset width)) endianess))

(define (wrapped-store-packed mem type align index value tp)
  (let ([width (type-width tp)]
        [bit-index (* 8 index)])
    (if (valid-bit-index? mem bit-index width)
        (term (bits ,(store mem bit-index (integer->integer-bytes (modulo value (expt 2 width))))))
        (term (trap)))))

(define (wrapped-store mem type align index value)
  (let ([bit-index (* 8 index)])
    (if (valid-bit-index? mem bit-index (type-width type))
        (term (bits ,(store mem bit-index
                            (match type
                              [`i32 (integer->integer-bytes value 4 #f endianess)]
                              [`i64 (integer->integer-bytes value 8 #f endianess)]
                              [`f32 (real->floating-point-bytes value 4 endianess)]
                              [`f64 (real->floating-point-bytes value 8 endianess)]))))
        (term (trap)))))

;; Shouldn't really be provided, but useful for testing and debugging
(define (store mem offset bytes)
  (bit-string-pack (bit-string-append (bit-string-take mem offset)
                                      bytes
                                      (bit-string-drop mem (bit-string-length bytes)))))

;; For tests
(define (store-integer mem offset width value)
  (store mem offset (integer->integer-bytes value (/ width 8) #f endianess)))
