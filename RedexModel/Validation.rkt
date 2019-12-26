#lang racket

(require redex/reduction-semantics
         "Syntax.rkt"
         "Utilities.rkt")

(define-extended-language WASMTyping WASM
    (C ::= ((func tf ...) (global tg ...) (table j) (memory j) (local t ...) (label ((t ...) ...)) (return t ...))
       ((func tf ...) (global tg ...) (table j) (memory j) (local t ...) (label ((t ...) ...)) (return))
       ((func tf ...) (global tg ...) (table j) (memory) (local t ...) (label ((t ...) ...)) (return t ...))
       ((func tf ...) (global tg ...) (table j) (memory) (local t ...) (label ((t ...) ...)) (return))
       ((func tf ...) (global tg ...) (table) (memory j) (local t ...) (label ((t ...) ...)) (return t ...))
       ((func tf ...) (global tg ...) (table) (memory j) (local t ...) (label ((t ...) ...)) (return))
       ((func tf ...) (global tg ...) (table) (memory) (local t ...) (label ((t ...) ...)) (return t ...))
       ((func tf ...) (global tg ...) (table) (memory) (local t ...) (label ((t ...) ...)) (return))
       ))

(define-judgment-form WASMTyping
  #:contract (⊢ C (e ...) tf)
  #:mode (⊢ I I O)
  
  [----------------------------------
   (⊢ C ((t const c)) (() -> (t)))]
  
  [--------------------------------
   (⊢ C ((t unop)) ((t) -> (t)))]
  
  [---------------------------------
   (⊢ C ((t binop)) ((t t) -> (t)))]

  [----------------------------------
   (⊢ C ((t testop)) ((t) -> (i32)))]

  [-----------------------------------
   (⊢ C ((t relop)) ((t t) -> (i32)))]

  [---------------------------------
   (⊢ C ((unreachable)) (() -> ()))] ;; TODO: need polymorphic function types...

  [-------------------------
   (⊢ C ((nop)) (() -> ()))]

  [--------------------------
   (⊢ C ((drop)) (() -> ()))] ;; TODO: need polymorphic function types...

  [(where t_2 (do-get (t ...) j))
   ------------------------------
   (⊢ ((_ _ _ _ (local t ...) _ _)) ((get-local j)) (() -> (t_2)))]

  [(where t_2 (do-get (t ...) j))
   ------------------------------
   (⊢ ((_ _ _ _ (local t ...) _ _)) ((set-local j)) ((t_2) -> ()))]

  [(where t_2 (do-get (t ...) j))
   ------------------------------
   (⊢ ((_ _ _ _ (local t ...) _ _)) ((tee-local j)) ((t_2) -> (t_2)))]

  [(where t (global-type (do-get (tg ...) j)))
   ------------------------------
   (⊢ ((_ (global tg ...) _ _ _ _)) ((get-global j)) (() -> (t)))]

  [(where (mut t) (do-get (tg ...) j))
   ------------------------------
   (⊢ ((_ (global tg ...) _ _ _ _)) ((set-global j)) ((t) -> ()))]
  )
