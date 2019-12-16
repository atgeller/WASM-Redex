#lang racket

(require redex/reduction-semantics
         bitsyntax
         "Syntax.rkt")

(provide (all-defined-out))

(define endianess #t)

(define (make-memory size)
  (bit-string (0 :: big-endian bits size)))

(define (wrapped-load mem type align index (tp_sx #f))
  (let* ([tp (and tp_sx (car tp_sx))]
         [sign (and tp_sx (cdr tp_sx))]
         ;; TODO: I don't really like handling all this here...
         ;; One of these is true by well-formedness
         [width (cond 
                  [(redex-match? WASMrt i8 tp) 8]
                  [(redex-match? WASMrt i16 tp) 16]
                  [(redex-match? WASMrt i32 tp) 32]
                  [(redex-match? WASMrt i32 type) 32]
                  [(redex-match? WASMrt i64 type) 64])]
         [conversionfn (if (redex-match? WASMrt signed sign)
                           bit-string->signed-integer
                           bit-string->unsigned-integer)])
    (if (< -1 index (+ index width) (bit-string-length mem))
        (term (,type const ,(load mem index width conversionfn)))
        (term (trap)))))

;; Shouldn't really be provided, but useful for testing and debugging
(define (load mem offset width conversionfn)
  (conversionfn (sub-bit-string mem offset (+ offset width)) endianess))

(define (wrapped-store mem type align index value (tp #f))
  (let* ([width (cond ;; One of these is true by well-formedness
                  [(redex-match? WASMrt i8 tp) 8]
                  [(redex-match? WASMrt i16 tp) 16]
                  [(redex-match? WASMrt i32 tp) 32]
                  [(redex-match? WASMrt i32 type) 32]
                  [(redex-match? WASMrt i64 type) 64])])
    (if (< -1 index (+ index width) (bit-string-length mem))
        (term (bits ,(store mem index width value)))
        (term (trap)))))

;; Shouldn't really be provided, but useful for testing and debugging
(define (store mem offset width value)
  (define packed (integer->bit-string value width endianess))
  (bit-string-pack! packed mem offset)
  (bit-string-pack mem))
