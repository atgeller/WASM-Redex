#lang racket

(require redex/reduction-semantics
         "../Utilities.rkt"
         "../Bits.rkt"
         "Utilities.rkt")

(provide ⊢)

(define-judgment-form WASMTyping
  #:contract (⊢ C (e ...) tf)
  
  [-------------------------------
  (⊢ C ((t const c)) (() -> (t)))]
  
  [------------------------------
   (⊢ C ((t unop)) ((t) -> (t)))]
  
  [---------------------------------
   (⊢ C ((t binop)) ((t t) -> (t)))]

  [----------------------------------
   (⊢ C ((t testop)) ((t) -> (i32)))]

  [-----------------------------------
   (⊢ C ((t relop)) ((t t) -> (i32)))]

  [(where (t_!_1 t_!_1) (t_1 t_2))
   (side-condition ,(or (and (integer-type? (term t_1))
                             (integer-type? (term t_2))
                             (< (type-width (term t_1)) (type-width (term t_2))))
                        (and (floating-type? (term t_1))
                             (floating-type? (term t_2)))))
   ------------------------------------------
   (⊢ C ((t_1 convert t_2)) ((t_2) -> (t_1)))]

  [(where (t_!_1 t_!_1) (t_1 t_2))
   (side-condition ,(nor (and (integer-type? (term t_1))
                              (integer-type? (term t_2))
                              (< (type-width (term t_1)) (type-width (term t_2))))
                         (and (floating-type? (term t_1))
                              (floating-type? (term t_2)))))
   ---------------------------------------------
   (⊢ C ((t_1 convert t_2 sx)) ((t_2) -> (t_1)))]

  [(where (t_!_1 t_!_1) (t_1 t_2))
   (side-condition ,(= (type-width (term t_1)) (type-width (term t_2))))
   ----------------------------------------------
   (⊢ C ((t_1 reinterpret t_2)) ((t_2) -> (t_1)))]

  [----------------------------------------------
   (⊢ C ((unreachable)) ((t_1 ...) -> (t_2 ...)))]

  [-------------------------
   (⊢ C ((nop)) (() -> ()))]

  [---------------------------
   (⊢ C ((drop)) ((t) -> ()))]

  [------------------------------------
   (⊢ C ((select)) ((t t i32) -> (t)))]

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (where C_2 (in-label C (t_2 ...))) ;; TODO: bug workaround, not needed if fixed
   (⊢ C_2 (e ...) tf)
   -------------------------------------
   (⊢ C ((block tf (e ...))) tf)]

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (⊢ (in-label C (t_1 ...)) (e ...) tf)
   -------------------------------------
   (⊢ C ((loop tf (e ...))) tf)]

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (⊢ (in-label C (t_2 ...)) (e_1 ...) tf)
   (⊢ (in-label C (t_2 ...)) (e_2 ...) tf)
   ---------------------------------------
   (⊢ C ((if tf (e_1 ...) (e_2 ...))) ((t_1 ... i32) -> (t_2 ...)))]

  [(label-types ((t ...) ...) (j) (t_3 ...))
   -----------------------------------------
   (⊢ (_ _ _ _ (label ((t ...) ...)) _) ((br j)) ((t_1 ... t_3 ...) -> (t_2 ...)))]

  [(label-types ((t ...) ...) (j) (t_1 ...))
   -----------------------------------------
   (⊢ (_ _ _ _ (label ((t ...) ...)) _) ((br-if j)) ((t_1 ... i32) -> (t_1 ...)))]

  [(label-types ((t ...) ...) (j ...) (t_3 ...))
   ---------------------------------------------
   (⊢ ((_ _ _ _ (label ((t ...) ...)) _ _)) ((br-table (j ...))) ((t_1 ... t_3 ... i32) -> (t_2 ...)))]

  [(where (_ _ _ _ _ _ (return (t ...))) C)
   ----------------------------------------
   (⊢ C ((return)) ((t_1 ... t ...) -> (t_2 ...)))]

  [(where tf_2 (do-get (tf ...) j))
   --------------------------------
   (⊢ ((func (tf ...)) _ _ _ _ _ _) ((call j)) tf_2)]

  [(where ((t_1 ...) -> (t_2 ...)) tf)
   (where (_ _ (table j) _ _ _ _) C)
   ---------------------------------
   (⊢ C ((call-indirect tf)) ((t_1 ... i32) -> (t_2 ...)))]

  [(where t_2 (do-get (t ...) j))
   ------------------------------
   (⊢ (_ _ _ _ (local (t ...)) _ _) ((get-local j)) (() -> (t_2)))]

  [(where t_2 (do-get (t ...) j))
   ------------------------------
   (⊢ (_ _ _ _ (local (t ...)) _ _) ((set-local j)) ((t_2) -> ()))]

  [(where t_2 (do-get (t ...) j))
   ------------------------------
   (⊢ (_ _ _ _ (local (t ...)) _ _) ((tee-local j)) ((t_2) -> (t_2)))]

  [(where (_ t) (do-get (tg ...) j))
   -------------------------------------------
   (⊢ (_ (global (tg ...)) _ _ _ _ _) ((get-global j)) (() -> (t)))]

  [(where (#t t) (do-get (tg ...) j))
   ----------------------------------
   (⊢ ((_ (global (tg ...)) _ _ _ _)) ((set-global j)) ((t) -> ()))]

  [(where (_ _ _ (memory j) _ _ _) C)
   (side-condition ,(< (expt 2 (term a))
                       (type-width (term t))))
   ------------------------------------------
   (⊢ C ((t load a _)) ((i32) -> (t)))]

  ;; TODO: no floats yet so not included in side-condition
  [(where (_ _ _ (memory j) _ _ _) C)
   (side-condition ,(<= (expt 2 (term a))
                       (type-width (term tp))))
   (side-condition ,(< (type-width (term tp)) (type-width (term t))))
   (side-condition ,(integer-type? (term t)))
   -----------------------------------------------------
   (⊢ C ((t load (tp sz) a _)) ((i32) -> (t)))]

  [(where (_ _ _ (memory j) _ _ _) C)
   (side-condition ,(< (expt 2 (term j_1))
                       (type-width (term t))))
   -------------------------------------------
   (⊢ C ((t store j_1 _)) ((i32 t) -> ()))]

  ;; TODO: no floats yet so not included in side-condition
  [(where (_ _ _ (memory j) _ _ _) C)
   (side-condition ,(<= (expt 2 (term j_1))
                       (type-width (term tp))))
   (side-condition ,(< (type-width (term tp)) (type-width (term t))))
   (side-condition ,(integer-type? (term t)))
   -----------------------------------------------------
   (⊢ C ((t store (tp) j_1 _)) ((i32 t) -> ()))]

  [(where (_ _ _ (memory j) _ _ _) C)
   ------------------------------------
   (⊢ C ((current-memory)) (() -> (i32)))]

  [(where (_ _ _ (memory j) _ _ _) C)
   ----------------------------------
   (⊢ C ((grow-memory)) ((i32) -> (i32)))]

  [--------------------
   (⊢ C () (() -> ()))]

  [(⊢ C (e_1 ...) ((t_1 ...) -> (t_2 ...)))
   (⊢ C (e_2) ((t_2 ...) -> (t_3 ...)))
   ------------------------------------
   (⊢ C (e_1 ... e_2) ((t_1 ...) -> (t_3 ...)))]

  [(⊢ C (e ...) ((t_1 ...) -> (t_2 ...)))
   ----------------------------------------
   (⊢ C (e ...) ((t ... t_1 ...) -> (t ... t_2 ...)))]
  )