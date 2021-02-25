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

;; closure accessors
(define-metafunction WASMrt
  cl-code : cl -> (func tf (local (t ...) (e ...)))
  [(cl-code (i (func tf (local (t ...) (e ...)))))
   (func tf (local (t ...) (e ...)))])

(define-metafunction WASMrt
  cl-code-opt : any -> any
  [(cl-code-opt (i (func tf (local (t ...) (e ...)))))
   (func tf (local (t ...) (e ...)))]
  [(cl-code-opt _) #f])

(define-metafunction WASMrt
  cl-inst : cl -> i
  [(cl-inst (i (func tf (local (t ...) (e ...)))))
   i])
