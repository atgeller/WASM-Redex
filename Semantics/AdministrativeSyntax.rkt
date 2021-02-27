#lang racket

(require redex/reduction-semantics
         "../Syntax.rkt")

(provide WASM-Admin)

(define-extended-language WASM-Admin WASM
  (v ::= (t const c))

  (e ::= .... trap (call cl) (label n (e ...) (e ...))
     (local n (i (v ...)) (e ...)))
  (L ::= hole (v ... (label n (e ...) L) e ...))

  (s ::= ((inst ...) (tabinst ...) (meminst ...)))
  (cl ::= (i (func tf (local (t ...) (e ...)))))
  (inst ::= ((cl ...) (v ...) (i ...) (i ...)))

  (tabinst ::= (cl ...))
  (meminst ::= bstr)

  (bstr ::= (side-condition any_1 (bytes? (term any_1)))))