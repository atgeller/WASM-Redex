#lang racket

(require redex/reduction-semantics
         "Syntax.rkt")

(provide (all-defined-out))

(define-syntax where/not
  (syntax-rules ()
   [(where/not x y) (match (term y)
                      [x #f]
                      [_ #t])]))

; Calculates the depth of a given execution context L
; This is equivalent to the inductive annotation n, in the Wasm paper
(define-metafunction WASMrt
  context-depth : L -> j
  [(context-depth hole) 0]
  [(context-depth (v ... (label n (e ...) L) e_2 ...)) ,(add1 (term (context-depth L)))])

;; NOTE: Useful for the very popular myriad index-based lookups
(define-metafunction WASMrt
  do-get : (any ...) j -> any
  [(do-get (any ...) j)
   ,(list-ref (term (any ...)) (term j))])

;; NOTE: Useful for the very popular myriad index-based lookups
(define-metafunction WASMrt
  do-set : (any ...) j any -> (any ...)
  [(do-set (any_1 ...) j any_2)
   ,(append (take (term (any_1 ...)) (term j))
            (term (any_2))
            (drop (term (any_1 ...)) (add1 (term j))))])

; Returns the number of bits required to represent values of a given type
; Equivalent to |t|
(define-metafunction WASMrt
  bit-width : t -> natural
  [(bit-width i32) 32]
  [(bit-width i64) 64]
  [(bit-width f32) 32]
  [(bit-width f64) 64])

; Returns the number of bits required to represent values of a given packed type
; Equivalent to |tp|
(define-metafunction WASMrt
  packed-bit-width : tp -> natural
  [(packed-bit-width i8) 8]
  [(packed-bit-width i16) 16]
  [(packed-bit-width i32) 32])
