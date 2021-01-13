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

(define (type-width type (tp #f))
  ;; One of these is true by well-formedness
  (cond 
    [(redex-match? WASMrt i8 tp) 8]
    [(redex-match? WASMrt i16 tp) 16]
    [(redex-match? WASMrt i32 tp) 32]
    [(redex-match? WASMrt i32 type) 32]
    [(redex-match? WASMrt i64 type) 64]
    [(redex-match? WASMrt f32 type) 32]
    [(redex-match? WASMrt f64 type) 64]))

(define (wrapped-load mem type align index (tp_sx #f))
  (let* ([tp (and tp_sx (first tp_sx))]
         [sign (and tp_sx (second tp_sx))]
         [width (type-width type tp)]
         [conversionfn (if (redex-match? WASMrt signed sign)
                           (compose (curry to-unsigned-sized (type-width type))
                                    bit-string->signed-integer)
                           bit-string->unsigned-integer)])
    (let ([bit-index (* 8 index)])
      (if (<= 0 bit-index (+ bit-index width) (bit-string-length mem))
          (term (,type const ,(load mem bit-index width conversionfn)))
          (term (trap))))))

;; Shouldn't really be provided, but useful for testing and debugging
(define (load mem offset width conversionfn)
  (conversionfn (sub-bit-string mem offset (+ offset width)) endianess))

(define (wrapped-store mem type align index value (tp #f))
  (let* ([width (type-width type tp)])
    (let ([bit-index (* 8 index)])
      (if (<= 0 bit-index (+ bit-index width) (bit-string-length mem))
          (term (bits ,(store mem bit-index width value)))
          (term (trap))))))

;; Shouldn't really be provided, but useful for testing and debugging
(define (store mem offset width value)
  (define packed (integer->bit-string value width endianess))
  (bit-string-pack (bit-string-append (bit-string-take mem offset)
                                      packed
                                      (bit-string-drop mem (+ offset width)))))
