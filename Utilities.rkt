#lang racket

(require redex/reduction-semantics
         "Syntax.rkt")

(provide (all-defined-out))

(define-syntax where/not
  (syntax-rules ()
   [(where/not x y) (match (term y)
                      [x #f]
                      [_ #t])]))

;; NOTE: Useful for the very popular myriad index-based lookups
(define-metafunction WASM
  index : (any ...) j -> any
  [(index (any ...) j)
   ,(list-ref (term (any ...)) (term j))])

;; NOTE: Useful for the very popular myriad index-based lookups
(define-metafunction WASM
  with-index : (any ...) j any -> (any ...)
  [(with-index (any_1 ...) j any_2)
   ,(append (take (term (any_1 ...)) (term j))
            (term (any_2))
            (drop (term (any_1 ...)) (add1 (term j))))])

; Returns the number of bits required to represent values of a given type
; Equivalent to |t|
(define-metafunction WASM
  bit-width : t -> natural
  [(bit-width i32) 32]
  [(bit-width i64) 64]
  [(bit-width f32) 32]
  [(bit-width f64) 64])

; Returns the number of bits required to represent values of a given packed type
; Equivalent to |tp|
(define-metafunction WASM
  packed-bit-width : tp -> natural
  [(packed-bit-width i8) 8]
  [(packed-bit-width i16) 16]
  [(packed-bit-width i32) 32])
