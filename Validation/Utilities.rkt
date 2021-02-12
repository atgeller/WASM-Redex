#lang racket

(require redex/reduction-semantics
         "../Syntax.rkt")

(provide (all-defined-out))

(define-extended-language WASMTyping WASM
  (C ::= ((func tf ...) (global tg ...) (table j ...) (memory j ...) (local t ...) (label (t ...) ...) (return (t ...) ...)))
  ;;        funcs    globals  table   memory  locals  labels        return
  #;(C ::= ((tf ...) (tg ...) (j ...) (j ...) (t ...) ((t ...) ...) ((t ...) ...)))

  (S ::= ((C ...) (j ...) (j ...))))

(define-metafunction WASMTyping
  reverse-get : (any ...) j -> any
  [(reverse-get (any ...) j)
   ,(list-ref (reverse (term (any ...))) (term j))])

(define-metafunction WASMTyping
  with-locals : C (t ...) -> C
  [(with-locals ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) _ (label (t_2 ...) ...) (return (t_3 ...) ...)) (t ...))
   ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local (t ...)) (label (t_2 ...) ...) (return (t_3 ...) ...))])

(define-metafunction WASMTyping
  in-label : C (t ...) -> C
  [(in-label ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local t_1 ...) (label (t_2 ...) ...) (return (t_3 ...) ...)) (t ...))
   ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local t_1 ...) (label (t_2 ...) ... (t ...)) (return (t_3 ...) ...))])

(define-metafunction WASMTyping
  with-return : C (t ...) -> C
  [(with-return ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local t_1 ...) (label (t_2 ...) ...) _) (t ...))
   ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local t_1 ...) (label (t_2 ...) ...) (return (t ...)))])

(define-metafunction WASMTyping
  context-labels : C -> ((t ...) ...)
  [(context-labels (_ _ _ _ _ (label (t ...) ...) _)) ((t ...) ...)])

(define-metafunction WASMTyping
  context-label : C i -> (t ...)
  [(context-label C i) (reverse-get (context-labels C) i)])

(define-metafunction WASMTyping
  context-return : C -> (t ...)
  [(context-return (_ _ _ _ _ _ (return (t ...)))) (t ...)])

(define-metafunction WASMTyping
  context-func : C i -> tf
  [(context-func ((func tf ...) _ _ _ _ _ _) i) (do-get (tf ...) i)])

(define-metafunction WASMTyping
  context-table : C -> n
  [(context-table (_ _ (table n) _ _ _ _)) n])

(define-metafunction WASMTyping
  context-memory : C -> n
  [(context-memory (_ _ _ (memory n) _ _ _)) n])

(define-metafunction WASMTyping
  context-local : C i -> t
  [(context-local (_ _ _ _ (local t ...) _ _) i) (do-get (t ...) i)])

(define-metafunction WASMTyping
  context-global : C i -> tg
  [(context-global (_ (global tg ...) _ _ _ _ _) i) (do-get (tg ...) i)])

(define-judgment-form WASMTyping
  #:contract (same-types ((t ...) ...) (t ...))
  #:mode (same-types I I)

  [-----------------------
   (same-types () (t ...))]

  [(same-types ((t_1 ...) ...) (t ...))
   --------------------------------------------
   (same-types ((t ...) (t_1 ...) ...) (t ...))])
