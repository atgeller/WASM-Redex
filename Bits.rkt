#lang racket

(require "MachineOps.rkt")

(provide (all-defined-out))

(define memory-page-size (make-parameter 65536))
(define max-memory-pages (make-parameter 32))

(define memory-size bytes-length)
(define (memory-pages mem)
  (/ (memory-size mem) (memory-page-size)))

(define (make-memory size)
  (make-bytes (* (memory-page-size) size) 0))

(define (grow-memory mem newsize)
  (if (<= (+ (memory-pages mem) newsize) (max-memory-pages))
      (values (bytes-append mem (make-memory newsize))
              (+ (memory-pages mem) newsize))
      (values mem #xFFFFFFFF)))

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

(define (valid-index? mem index width)
  (<= 0 index (+ index width) (bytes-length mem)))

(define (wrapped-load-packed mem type align index tp sign)
  (let ([width (/ (type-width tp) 8)])
    (if (valid-index? mem index width)
        (match sign
          [`signed (to-unsigned-sized (type-width type) (integer-bytes->integer mem #t #f index (+ index width)))]
          [`unsigned (integer-bytes->integer mem #f #f index (+ index width))])
        #f)))

(define (wrapped-load mem type align index)
  (if (valid-index? mem index (/ (type-width type) 8))
      (match type
        [`i32 (integer-bytes->integer mem #f #f index (+ index 4))]
        [`i64 (integer-bytes->integer mem #f #f index (+ index 8))]
        [`f32 (real->single-flonum (floating-point-bytes->real mem #f index (+ index 4)))]
        [`f64 (real->double-flonum (floating-point-bytes->real mem #f index (+ index 8)))])
      #f))

(define (wrapped-store-packed mem type align index value tp)
  (let* ([bit-width (type-width tp)]
         [width (/ bit-width 8)])
    (if (valid-index? mem index width)
        (integer->integer-bytes (modulo value (expt 2 bit-width)) width #f #f mem index)
        #f)))

(define (wrapped-store mem type align index value)
  (if (valid-index? mem index (/ (type-width type) 8))
      (match type
        [`i32 (integer->integer-bytes value 4 #f #f mem index)]
        [`i64 (integer->integer-bytes value 8 #f #f mem index)]
        [`f32 (real->floating-point-bytes value 4 #f mem index)]
        [`f64 (real->floating-point-bytes value 8 #f mem index)])
      #f))

;; Shouldn't really be provided, but useful for testing and debugging
#;(define (store mem offset bytes)
  (bytes-append (subbytes mem 0 offset)
                bytes
                (subbytes mem (+ offset (bytes-length bytes)))))

;; For tests
#;(define (store-integer mem offset width value)
  (store mem offset (integer->integer-bytes value (/ width 8) #f #f)))
