#lang racket

(require redex/reduction-semantics
         "../Utilities.rkt"
         "Utilities.rkt"
         "InstructionTyping.rkt")

(provide ⊢-module-func
         ⊢-module-global
         ⊢-module-table
         ⊢-module-memory
         ⊢-module
         extract-module-type)

;; Validates the function definition and returns all exports and the type of the function
(define-judgment-form WASMTyping
  #:contract (⊢-module-func C f ((ex ...) tf))

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (where C_2 (with-return (in-label (with-locals C (t_1 ... t ...)) (t_2 ...)) (t_2 ...)))
   (⊢ C_2 (e ...) (() -> (t_2 ...)))
   ----------------------------------------------------------------------------------------
   (⊢-module-func C ((ex ...) (func tf (local (t ...) (e ...)))) ((ex ...) tf))]

  [-------------------------------------------------------
   (⊢-module-func C ((ex ...) (func tf im)) ((ex ...) tf))])

;; Validates the global variable definition and returns all exports and the type of the global
(define-judgment-form WASMTyping
  #:contract (⊢-module-global C glob ((ex ...) tg))

  [(where (mut t) tg)
   (⊢ C (e ...) (() -> (t)))
   (side-condition ,(or (empty? (term (ex ...))) (equal? (term (mut t)) (term (const t)))))
   ----------------------------------------------------------------------------------------
   (⊢-module-global C ((ex ...) (global tg (e ...))) ((ex ...) tg))]

  ;; Imported globals are immutable
  [(where (const t) tg)
   -----------------------------------------------------------
   (⊢-module-global C ((ex ...) (global tg im)) ((ex ...) tg))])

;; Validates the table and returns all exports and the table size
(define-judgment-form WASMTyping
  #:contract (⊢-module-table C tab ((ex ...) i))

  [(where (tf ...) ((context-func C i) ...))
   (where n ,(length (term (i ...))))
   ------------------------------------------------------------
   (⊢-module-table C ((ex ...) (table n (i ...))) ((ex ...) n))]

  [-------------------------------------------------------
   (⊢-module-table C ((ex ...) (table n im)) ((ex ...) n))])

;; Returns all exports and the memory size
(define-judgment-form WASMTyping
  #:contract (⊢-module-memory C mem ((ex ...) n))

  [------------------------------------------------------
   (⊢-module-memory C ((ex ...) (memory n)) ((ex ...) n))]

  [---------------------------------------------------------
   (⊢-module-memory C ((ex ...) (memory n im)) ((ex ...) n))])

;; Validates all definitions in the module against the types declared in the module
(define-judgment-form WASMTyping
  #:contract (⊢-module mod C)

  [(⊢-module-func C_f f ((ex_f ...) tf)) ...
   (⊢-module-global C_g glob ((ex_g ...) tg)) ...
   (⊢-module-table C_t tab ((ex_t ...) n_t)) ...
   (⊢-module-memory C_m mem ((ex_m ...) n_m)) ...
   (side-condition ,(<= (length (term (n_t ...))) 1))
   (side-condition ,(<= (length (term (n_m ...))) 1))

   (where (C_g ...) (global-contexts (tg ...)))
   
   (where ((func tf ...) (global tg ...) (table n_t ...) (memory n_m ...) (local) (label) (return)) C)
   (side-condition (same (C_f ... C_t ... C_m ...) C))

   (side-condition (distinct (ex_f ... ... ex_g ... ... ex_t ... ... ex_m ... ...)))
   ---------------------------------------------------------------------------------------------------
   (⊢-module (module (f ...) (glob ...) (tab ...) (mem ...)) C)])

(define-metafunction WASMTyping
  global-contexts : (tg ...) -> (C ...)
  [(global-contexts ()) ()]
  [(global-contexts (tg_i-1 ... tg))
   (C ... ((func) (global tg_i-1 ...) (table) (memory) (local) (label) (return)))
   (where (C ...) (global-contexts (tg_i-1 ...)))])

(define-metafunction WASMTyping
  distinct : (any ...) -> boolean
  [(distinct ()) #t]
  [(distinct (any any_rest ...))
   (distinct (any_rest ...))
   (side-condition (not (member (term any) (term (any_rest ...)))))
   or
   #f])


(define-metafunction WASMTyping
  func-type : f -> tf
  [(func-type (_ (func tf _))) tf])

(define-metafunction WASMTyping
  glob-type : glob -> tg
  [(glob-type (_ (global tg _))) tg])

(define-metafunction WASMTyping
  tab-size : tab -> n
  [(tab-size (_ (table n _ ...))) n])

(define-metafunction WASMTyping
  mem-size : mem -> n
  [(mem-size (_ (memory n _ ...))) n])

;; Extracts the module type annotations into a context
(define-metafunction WASMTyping
  extract-module-type : mod -> C
  [(extract-module-type (module (f ...) (glob ...) (tab ...) (mem ...)))
   ((func (func-type f) ...)
    (global (glob-type glob) ...)
    (table (tab-size tab) ...)
    (memory (mem-size mem) ...)
    (local)
    (label)
    (return))])
