#lang racket

(require redex/reduction-semantics
         "../Utilities.rkt"
         "TypingSyntax.rkt")

(provide (all-defined-out))

(define (list-ref-right lst pos)
  (list-ref (reverse lst) pos))

(define-metafunction WASM-Typing
  reverse-get : (any ...) j -> any
  [(reverse-get (any ...) j)
   ,(list-ref-right (term (any ...)) (term j))])

; Below are various setters used to functionally update a field of the module type context (used in inductive typing rules, such as blocks)

;; Sets the local variable types, used in the typing rule for functions to set up the context to type check the function body
(define-metafunction WASM-Typing
  with-locals : C (t ...) -> C
  [(with-locals ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) _ (label (t_2 ...) ...) (return (t_3 ...) ...)) (t ...))
   ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local t ...) (label (t_2 ...) ...) (return (t_3 ...) ...))])

;; Adds a branch condition (the pre-condition of a branch instruction) onto the label stack.
;; Used in typing rules for block, loop, and if to append the branching condition of the block
(define-metafunction WASM-Typing
  add-label : C (t ...) -> C
  [(add-label ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local t_1 ...) (label (t_2 ...) ...) (return (t_3 ...) ...)) (t ...))
   ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local t_1 ...) (label (t_2 ...) ... (t ...)) (return (t_3 ...) ...))])

;; Sets the return condition, used in the typing rule for functions to set up the context to type check the function body
(define-metafunction WASM-Typing
  with-return : C (t ...) -> C
  [(with-return ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local t_1 ...) (label (t_2 ...) ...) _) (t ...))
   ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local t_1 ...) (label (t_2 ...) ...) (return (t ...)))])

; Below are various accessors used to extract a field from the module type context
(define-metafunction WASM-Typing
  context-funcs : C -> (tf ...)
  [(context-funcs ((func tf ...) _ _ _ _ _ _)) (tf ...)])

(define-metafunction WASM-Typing
  context-func : C i -> tf
  [(context-func C i) (index (context-funcs C) i)])

(define-metafunction WASM-Typing
  context-globals : C -> (tg ...)
  [(context-globals (_ (global tg ...) _ _ _ _ _)) (tg ...)])

(define-metafunction WASM-Typing
  context-global : C i -> tg
  [(context-global C i) (index (context-globals C) i)])

(define-metafunction WASM-Typing
  context-table : C -> n
  [(context-table (_ _ (table n) _ _ _ _)) n])

(define-metafunction WASM-Typing
  context-memory : C -> n
  [(context-memory (_ _ _ (memory n) _ _ _)) n])

(define-metafunction WASM-Typing
  context-local : C i -> t
  [(context-local (_ _ _ _ (local t ...) _ _) i) (index (t ...) i)])

(define-metafunction WASM-Typing
  context-labels : C -> ((t ...) ...)
  [(context-labels (_ _ _ _ _ (label (t ...) ...) _)) ((t ...) ...)])

(define-metafunction WASM-Typing
  context-label : C i -> (t ...)
  [(context-label C i) (reverse-get (context-labels C) i)])

(define-metafunction WASM-Typing
  context-return : C -> (t ...)
  [(context-return (_ _ _ _ _ _ (return (t ...)))) (t ...)])

;; returns true if everything in the list is the same as the second argument,
;; otherwise returns false
(define-metafunction WASM-Typing
  same : (any ...) any -> boolean
  [(same () any) #t]
  [(same (any_!_ any_rest ...) any_!_) #f]
  [(same (any any_rest ...) any)
   (same (any_rest ...) any)])
