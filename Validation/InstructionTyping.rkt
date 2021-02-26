#lang racket

(require redex/reduction-semantics
         "../Utilities.rkt"
         "Utilities.rkt"
         "TypingSyntax.rkt")

(provide ⊢)

(define-metafunction WASM-Typing
  inn? : t -> boolean
  [(inn? inn) #t]
  [(inn? fnn) #f])

(define-metafunction WASM-Typing
  fnn? : t -> boolean
  [(fnn? inn) #f]
  [(fnn? fnn) #t])

(define-judgment-form WASM-Typing
  #:contract (⊢ C (e ...) tf)
  
  [------------------------------
  (⊢ C ((t const c)) (() -> (t)))]
  
  [-----------------------------
   (⊢ C ((t unop)) ((t) -> (t)))]
  
  [--------------------------------
   (⊢ C ((t binop)) ((t t) -> (t)))]

  [---------------------------------
   (⊢ C ((t testop)) ((t) -> (i32)))]

  [----------------------------------
   (⊢ C ((t relop)) ((t t) -> (i32)))]

  [(where (t_!_1 t_!_1) (t_1 t_2))
   (side-condition ,(or (and (term (inn? t_1)) (term (inn? t_2))
                             (< (term (bit-width t_1)) (term (bit-width t_2))))
                        (and (term (fnn? t_1)) (term (fnn? t_2)))))
   ----------------------------------------------------------------------------
   (⊢ C ((t_1 convert t_2)) ((t_2) -> (t_1)))]

  [(where (t_!_1 t_!_1) (t_1 t_2))
   (side-condition ,(nor (and (term (inn? t_1)) (term (inn? t_2))
                              (< (term (bit-width t_1)) (term (bit-width t_2))))
                         (and (term (fnn? t_1)) (term (fnn? t_2)))))
   -----------------------------------------------------------------------------
   (⊢ C ((t_1 convert t_2 sx)) ((t_2) -> (t_1)))]

  [(where (t_!_1 t_!_1) (t_1 t_2))
   (side-condition ,(= (term (bit-width t_1)) (term (bit-width t_2))))
   -------------------------------------------------------------------
   (⊢ C ((t_1 reinterpret t_2)) ((t_2) -> (t_1)))]

  [--------------------------------------------
   (⊢ C (unreachable) ((t_1 ...) -> (t_2 ...)))]

  [----------------------
   (⊢ C (nop) (() -> ()))]

  [------------------------
   (⊢ C (drop) ((t) -> ()))]

  [---------------------------------
   (⊢ C (select) ((t t i32) -> (t)))]

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (where C_2 (add-label C (t_2 ...)))
   (⊢ C_2 (e ...) tf)
   -----------------------------------
   (⊢ C ((block tf (e ...))) tf)]

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (where C_2 (add-label C (t_1 ...)))
   (⊢ C_2 (e ...) tf)
   -----------------------------------
   (⊢ C ((loop tf (e ...))) tf)]

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (where C_2 (add-label C (t_2 ...)))
   (⊢ C_2 (e_1 ...) tf)
   (⊢ C_2 (e_2 ...) tf)
   ---------------------------------------------------------------------
   (⊢ C ((if tf (e_1 ...) else (e_2 ...))) ((t_1 ... i32) -> (t_2 ...)))]

  [(where (t ...) (context-label C i))
   ---------------------------------------------
   (⊢ C ((br i)) ((t_1 ... t ...) -> (t_2 ...)))]

  [(where (t ...) (context-label C i))
   ------------------------------------------
   (⊢ C ((br-if i)) ((t ... i32) -> (t ...)))]

  [(where (t ...) (context-label C i))
   (where ((t_3 ...) ...) ((context-label C i_1) ...))
   (side-condition (same ((t_3 ...) ...) (t ...)))
   ---------------------------------------------------------------
   (⊢ C ((br-table i i_1 ...)) ((t_1 ... t ... i32) -> (t_2 ...)))]

  [(where (t ...) (context-return C))
   ---------------------------------------------
   (⊢ C (return) ((t_1 ... t ...) -> (t_2 ...)))]

  [(where tf (context-func C i))
   -----------------------------
   (⊢ C ((call i)) tf)]

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (where n (context-table C))
   -------------------------------------------------------
   (⊢ C ((call-indirect tf)) ((t_1 ... i32) -> (t_2 ...)))]

  [(where t (context-local C i))
   ---------------------------------
   (⊢ C ((get-local i)) (() -> (t)))]

  [(where t (context-local C i))
   ---------------------------------
   (⊢ C ((set-local i)) ((t) -> ()))]

  [(where t (context-local C i))
   ----------------------------------
   (⊢ C ((tee-local i)) ((t) -> (t)))]

  [(where (_ t) (context-global C i))
   ----------------------------------
   (⊢ C ((get-global i)) (() -> (t)))]

  [(where (var t) (context-global C i))
   ------------------------------------
   (⊢ C ((set-global i)) ((t) -> ()))]

  [(where n (context-memory C))
   (side-condition ,(<= (expt 2 (term a)) (/ (term (bit-width t)) 8)))
   -------------------------------------------------------------
   (⊢ C ((t load a o)) ((i32) -> (t)))]

  [(where n (context-memory C))
   (side-condition ,(<= (expt 2 (term a)) (/ (term (packed-bit-width tp)) 8)))
   (side-condition ,(< (term (packed-bit-width tp)) (term (bit-width t))))
   (where inn t)
   -----------------------------------------------------------------------
   (⊢ C ((t load (tp sx) a o)) ((i32) -> (t)))]

  [(where n (context-memory C))
   (side-condition ,(<= (expt 2 (term a)) (/ (term (bit-width t)) 8)))
   -------------------------------------------------------------
   (⊢ C ((t store a o)) ((i32 t) -> ()))]

  [(where n (context-memory C))
   (side-condition ,(<= (expt 2 (term a)) (/ (term (packed-bit-width tp)) 8)))
   (side-condition ,(< (term (packed-bit-width tp)) (term (bit-width t))))
   (where inn t)
   -----------------------------------------------------------------------
   (⊢ C ((t store tp a o)) ((i32 t) -> ()))]

  [(where n (context-memory C))
   --------------------------------------
   (⊢ C (current-memory) (() -> (i32)))]

  [(where n (context-memory C))
   --------------------------------------
   (⊢ C (grow-memory) ((i32) -> (i32)))]

  [-------------------
   (⊢ C () (() -> ()))]

  [(⊢ C (e_1 ...) ((t_1 ...) -> (t_2 ...)))
   (⊢ C (e_2) ((t_2 ...) -> (t_3 ...)))
   --------------------------------------------
   (⊢ C (e_1 ... e_2) ((t_1 ...) -> (t_3 ...)))]

  [(⊢ C (e ...) ((t_1 ...) -> (t_2 ...)))
   --------------------------------------------------
   (⊢ C (e ...) ((t ... t_1 ...) -> (t ... t_2 ...)))]
  )