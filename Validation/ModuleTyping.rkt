#lang racket

(require redex/reduction-semantics
         "../Utilities.rkt"
         "Utilities.rkt"
         "InstructionTyping.rkt")

(provide ⊢-module-func ;; For store typing
         ⊢-module-global
         ⊢-module-func-list
         ⊢-module-global-list
         ⊢-module-table
         ⊢-module-mem
         ⊢-module
         valid-indexes
         extract-module-type)

;; Validates the function definition and returns all exports and the type of the function
(define-judgment-form WASMTyping
  #:contract (⊢-module-func C f ((ex ...) tf))

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (⊢ ((func (tf_1 ...)) (global (tg ...)) (table j_1 ...) (memory j_2 ...) (local (t_1 ... t ...)) (label ((t_2 ...))) (return (t_2 ...)))
      (e ...)
      (() -> (t_2 ...)))
   ---------------------
   (⊢-module-func ((func (tf_1 ...)) (global (tg ...)) (table j_1 ...) (memory j_2 ...) _ _ _)
                  (func (ex ...) tf (local (t ...) (e ...)))
                  ((ex ...) tf))]

  [--------------------------------------
   (⊢-module-func C
                  (func (ex ...) tf im)
                  ((ex ...) tf))]
  )

;; Helper judgement to ensure that function declarations/definitions are valid
;; Ensures each function in the list matches its respective type under a the module type consisting only of the preceeding global definitions in the list
(define-judgment-form WASMTyping
  #:contract (⊢-module-func-list C (f ...) (((ex ...) tf) ...))

  [-----------------------------
   (⊢-module-func-list C () ())]

  [(⊢-module-func C f_1 ((ex_1 ...) tf_1))
   (⊢-module-func-list C (f_2 ...) (((ex_2 ...) tf_2) ...))
   --------------------------------------------------------
   (⊢-module-func-list C (f_1 f_2 ...) (((ex_1 ...) tf_1) ((ex_2 ...) tf_2) ...))]
  )

;; Validates the global variable definition and returns all exports and the type of the global
(define-judgment-form WASMTyping
  #:contract (⊢-module-global C glob ((ex ...) tg))

  [(where (#f t) tg)
   (⊢ C (e ...) (() -> (t)))
   -------------------------
   (⊢-module-global C
                    (global (ex ...) tg (e ...))
                    ((ex ...) tg))]

  ;; Can't have exports if global is mutable
  [(where (#t t) tg)
   (where () (ex ...))
   (⊢ C (e ...) (() -> (t)))
   -------------------------
   (⊢-module-global C
                    (global (ex ...) tg (e ...))
                    ((ex ...) tg))]

  ;; Imported globals are immutable
  [(where (#f t) tg)
   ------------------
   (⊢-module-global C
                    (global (ex ...) tg im)
                    ((ex ...) tg))]
  )

;; Helper judgement to ensure that global variable definitions are valid
;; Ensures each function in the list matches its respective type under a the module type consisting only of the preceeding global definitions in the list
(define-judgment-form WASMTyping
  #:contract (⊢-module-global-list (glob ...) (((ex ...) tg) ...))

  [(⊢-module-global-list () ())]

  [(⊢-module-global ((func ()) (global (tg ...)) (table) (memory) (local ()) (label ()) (return)) glob_1 ((ex_1 ...) tg_1))
   (⊢-module-global-list (glob ...) (((ex ...) tg) ...))
   ------------------------------------------------------------------------------------------------------------------------
   (⊢-module-global-list (glob ... glob_1) (((ex ...) tg) ... ((ex_1 ...) tg_1)))]
  )

;; Helper function to ensure a table is well-formed
;; Checks that there are exactly `i` indices (j ...), and that each one points to a valid function
(define-metafunction WASMTyping
  valid-indexes : C (j ...) i -> boolean
  [(valid-indexes ((func (tf ...)) _ _ _ _ _ _) (j ...) i)
   ,(and (= (length (term (j ...))) (term i))
         (let ([bound (length (term (tf ...)))])
           (andmap
            (lambda (index) (< index bound))
            (term (j ...)))))])

;; Validates the table and returns all exports and the table size
(define-judgment-form WASMTyping
  #:contract (⊢-module-table C tab ((ex ...) i))

  [(where #t (valid-indexes C (j ...) i))
   ---------------------------------------------
   (⊢-module-table C
                   (table (ex ...) i (j ...))
                   ((ex ...) i))]

  [-----------------
   (⊢-module-table C
                   (table (ex ...) i im)
                   ((ex ...) i))]
  )

;; Returns all exports and the memory size
(define-judgment-form WASMTyping
  #:contract (⊢-module-mem C mem ((ex ...) i))

  [----------------------------------------------------
   (⊢-module-mem C (memory (ex ...) i) ((ex ...) i))]

  [------------------------------------------------------
   (⊢-module-mem C (memory (ex ...) i im) ((ex ...) i))]
  )

;; Validates all definitions in the module against the types declared in the module
(define-judgment-form WASMTyping
  #:contract (⊢-module mod C)

  [(where ((func (tf ...)) (global (tg ...)) (table i) (memory j) (local ()) (label ()) (return)) C)
   (⊢-module-func-list C (f ...) (((ex_1_ ...) tf) ...))
   (⊢-module-global-list (glob ...) (((ex_2_ ...) tg) ...))
   (⊢-module-table C tab ((ex_3_ ...) i))
   (⊢-module-mem C mem ((ex_4_ ...) j))
   ------------------------------------
   (⊢-module (module (f ...) (glob ...) (tab) (mem)) C)]

  [(where ((func (tf ...)) (global (tg ...)) (table i) (memory) (local ()) (label ()) (return)) C)
   (⊢-module-func-list C (f ...) (((ex_1_ ...) tf) ...))
   (⊢-module-global-list (glob ...) (((ex_2_ ...) tg) ...))
   (⊢-module-mem C tab ((ex_3_ ...) i))
   ------------------------------------
   (⊢-module (module (f ...) (glob ...) (tab) ()) C)]

  [(where ((func (tf ...)) (global (tg ...)) (table) (memory j) (local ()) (label ()) (return)) C)
   (⊢-module-func-list C (f ...) (((ex_1_ ...) tf) ...))
   (⊢-module-global-list (glob ...) (((ex_2_ ...) tg) ...))
   (⊢-module-mem C mem ((ex_4_ ...) j))
   ------------------------------------
   (⊢-module (module (f ...) (glob ...) () (mem)) C)]

  [(⊢-module-func-list C (f ...) (((ex_1_ ...) tf) ...))
   (⊢-module-global-list (glob ...) (((ex_2_ ...) tg) ...))
   (where C ((func (tf ...)) (global (tg ...)) (table) (memory) (local ()) (label ()) (return)))
   ---------------------------------------------------------------------------------------------
   (⊢-module (module (f ...) (glob ...) () ()) C)]
  )

;; Helper metafunction to extract a function type declaration from the function definition
(define-metafunction WASMTyping
  extract-func-types : (f ...) -> (tf ...)
  [(extract-func-types ()) ()]
  [(extract-func-types ((_ (func tf _)) f_2 ...))
   (tf (extract-func-types (f_2 ...)))])

;; Helper metafunction to extract a global variable's type from the global variable definition
(define-metafunction WASMTyping
  extract-global-types : (glob ...) -> (tg ...)
  [(extract-global-types ()) ()]
  [(extract-global-types ((_ (global tg _)) glob_2 ...))
   (tg (extract-global-types (glob_2 ...)))])

;; Extracts the declared module type (consisting of all declared function and global types in that module, as well as the size of table and memory if applicable)
;; Eventually may be useful for deriving module types
(define-metafunction WASMTyping
  extract-module-type : mod -> C
  [(extract-module-type (module (f ...) (glob ...) ((table i _)) ((memory j))))
   ((extract-func-types (f ...)) (extract-global-types (glob ...)) (table i) (memory j) (local ()) (label ()) (return))]
  [(extract-module-type (module (f ...) (glob ...) ((table i _)) ()))
   ((extract-func-types (f ...)) (extract-global-types (glob ...)) (table i) (memory) (local ()) (label ()) (return))]
  [(extract-module-type (module (f ...) (glob ...) () ((memory j))))
   ((extract-func-types (f ...)) (extract-global-types (glob ...)) (table) (memory j) (local ()) (label ()) (return))]
  [(extract-module-type (module (f ...) (glob ...) () ()))
   ((extract-func-types (f ...)) (extract-global-types (glob ...)) (table) (memory) (local ()) (label ()) (return))])
