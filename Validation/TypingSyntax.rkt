#lang racket

(require redex/reduction-semantics
         "../Syntax.rkt")

(provide WASM-Typing)

(define-extended-language WASM-Typing WASM
  (C ::= ((func tf ...) (global tg ...) (table j) (memory j) (local t ...) (label (t ...) ...) (return (t ...)))
     ((func tf ...) (global tg ...) (table j) (memory j) (local t ...) (label (t ...) ...) (return))
     ((func tf ...) (global tg ...) (table j) (memory) (local t ...) (label (t ...) ...) (return (t ...)))
     ((func tf ...) (global tg ...) (table j) (memory) (local t ...) (label (t ...) ...) (return))
     ((func tf ...) (global tg ...) (table) (memory j) (local t ...) (label (t ...) ...) (return (t ...)))
     ((func tf ...) (global tg ...) (table) (memory j) (local t ...) (label (t ...) ...) (return))
     ((func tf ...) (global tg ...) (table) (memory) (local t ...) (label (t ...) ...) (return (t ...)))
     ((func tf ...) (global tg ...) (table) (memory) (local t ...) (label (t ...) ...) (return)))

  (S ::= ((C ...) (j ...) (j ...))))
