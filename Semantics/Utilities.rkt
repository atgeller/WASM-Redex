#lang racket

(require redex/reduction-semantics
         "RunTimeSyntax.rkt")

(provide context-depth)

; Calculates the depth of a given execution context L
; This is equivalent to the inductive annotation n, in the Wasm paper
(define-metafunction WASM-RunTime
  context-depth : L -> j
  [(context-depth hole) 0]
  [(context-depth (v ... (label n (e ...) L) e_2 ...)) ,(add1 (term (context-depth L)))])