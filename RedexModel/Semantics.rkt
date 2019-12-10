#lang racket

(require redex/reduction-semantics
         "Syntax.rkt"
         "Utilities.rkt")

(provide ->)

;; TL;DR about stack: the stack is implicit in the stream of instructions being processed.
;; This is because v \subset e, so although we say (e ...) it ends up looking like (v ... e ...).
;; Thus, the next instruction to execute is the head of e ..., and the stack is v ...
(define ->
    (reduction-relation
     WASMrt
     #:domain (s j (v ...) (e ...))
     #:codomain (s j (v ...) (e ...))
     ;; (s j (v ...) (e ...)) -> (s j (v ...) (e ...))
     ;; s = Store
     ;; j = Current instance (runtime module representation) index
     ;; (v ...) = Local variables
     ;; (e ...) = Code (with implicit stack)

     ;; Every instruction is operating inside of an execution environment.
     ;; We need these contexts to keep track of the stack and next instructions at each level,
     ;; and to handle branching/returning from levels.
     ;; Much of the instructions don't need to know about the environment they are executing in, but some do.
     ;; Todo: It would be nice to reduce the boilerplate for those that don't.
     
     ;; Due to validation we can be sure we are returning the proper number of values
     ;; TODOING: Add store.
     (--> (s j (v ...) (in-hole L (v_1 ... (t const c_1) (t const c_2) (t binop) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (eval-binop binop c_1 c_2 t) e ...))))
     
     (--> (s j (v ...) (in-hole L (v_1 ... (t const c) (t testop) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (eval-testop testop c t) e ...))))
     
     (--> (s j (v ...) (in-hole L (v_1 ... (t const c_1) (t const c_2) (t relop) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (eval-relop relop c_1 c_2 t) e ...))))
          
     (--> (s j (v ...) (in-hole L (v_1 ... (nop) e ...)))
          (s j (v ...) (in-hole L (v_1 ... e ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... (unreachable) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (trap) e ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... v_2 v_3 (i32 const 0) (select) e ...)))
          (s j (v ...) (in-hole L (v_1 ... v_3 e ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... v_2 v_3 (i32 const c) (select) e ...)))
          (s j (v ...) (in-hole L (v_1 ... v_2 e ...)))
          (side-condition (> (term c) 0)))
     
     (--> (s j (v ...) (in-hole L (v_1 ... v_2 (drop) e ...)))
          (s j (v ...) (in-hole L (v_1 ... e ...))))
     
     (--> (s j (v ...) (in-hole L (v_1 ... (i32 const 0) (if tf (e_1 ...) else (e_2 ...)) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... (i32 const c) (if tf (e_1 ...) else (e_2 ...)) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e ...)))
          (side-condition (> (term c) 0)))
     
     (--> (s j (v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e_2 ...)))
          (s j (v ...) (in-hole L (v_1 ... (label () (e_1 ...)) e_2 ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... (label () (v_2 ... (trap) e ...) e_2 ...))))
          (s j (v ...) (in-hole L ((trap)))))

     ; Knowing about contexts is necessary for this (so can't shortcut the rest :/)!
     (--> (s j (v ...) (in-hole L (v_1 ... (br j_1) e ...)))
          (s j (v ...) (decompose L j_1 (v_1 ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... (i32 const 0) (br-if j_1) e ...)))
          (s j (v ...) (in-hole L (v_1 ... e ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... (i32 const c) (br-if j_1) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (br j_1) e ...)))
          (side-condition (> (term c) 0)))

     (--> (s j (v ...) (in-hole L (v_1 ... (i32 const c) (br-table (j_1 ...)) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (br (do-get (j_1 ...) (term c))) e ...)))
          (side-condition (<= (term c) (length (term (j_1 ...))))))

     (--> (s j (v ...) (in-hole L (v_1 ... (i32 const c) (br-table (j_1 ... j_2)) e ...)))
          (s j (v ...) (in-hole L (v_1 ... v_2 (br j_2) e ...)))
          (side-condition (> (term c) (length (term (j_1 ...))))))

     (--> (s j (v ...) (in-hole L (v_1 ... (label () (v_2 ...)) e ...)))
          (s j (v ...) (v_1 ... v_2 ... e ...)))

     ;; Locals!
     (--> (s j (v ...) (in-hole L (v_1 ... (get-local j_1) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (do-get (v ...) j_1) e ...))))
     
     (--> (s j (v ...) (in-hole L (v_1 ... v_2 (set-local j) e ...)))
          (s j (do-set (v ...) j v_2) (in-hole L (v_1 ... e ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... v_2 (tee-local j) e ...)))
          (s j (v ...) (in-hole L (v_1 ... v_2 v_2 (set-local j) e ...))))

     ;; Store stuff!
     (--> ((inst ...) j (v ...) (in-hole L (v_1 ... (get-global j_1) e ...)))
          ((inst ...) j (v ...) (in-hole L (v_1 ... (do-global-get (inst ...) j j_1) e ...))))

     (--> ((inst ...) j (v ...) (in-hole L (v_1 ... v_2 (set-global j_1) e ...)))
          ((do-global-set (inst ...) j j_1 v_2) j (v ...) (in-hole L (v_1 ... e ...))))

     ; Function calls
     (--> ((inst ...) j (v ...) (in-hole L (v_1 ... (call j_1) e ...)))
          ((inst ...) j (v ...) (in-hole L (v_1 ... (call (function-lookup (inst ...) j j_1)) e ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... (call cl) e ...)))
          (s j (v ...) (in-hole L (setup-call (v_1 ...) cl (e ...)))))

     ; Stuff inside functions calls
     ;; TODO: Can we assume v_3 ... is the right arity and types to return?
     ;; NOTE: This is fun decomposition...
     (--> (s j (v ...) (in-hole L (v_1 ... (local (j_1 (v_2 ...)) (in-hole L_2 (v_3 ... (return) e ...)) e_2 ...))))
          (s j (v ...) (in-hole L (v_1 ... v_3 ... e_2 ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... (local (j_1 (v_2 ...)) (v_3 ... (trap) e ...)) e_2 ...)))
          (s j (v ...) (in-hole L ((trap)))))

     ;; TODO: Can we assume v_3 ... has the right arity and types to return?
     (--> (s j (v ...) (in-hole L (v_1 ... (local (j_1 (v_2 ...)) (v_3 ...)) e_2 ...)))
          (s j (v ...) (in-hole L (v_1 ... v_3 ... e_2 ...))))

     (--> (s j (v ...) (in-hole L (v_1 ... (local (j_1 (v_2 ...)) (e ...)) e_2 ...)))
          (s_new j (v ...) (in-hole L (v_1 ... (local (j_1 (v_2new ...)) (e_new ...)) e_2 ...)))
          (where ((s_new j_1 (v_2new ...) (e_new ...)))
                 ,(apply-reduction-relation -> (term (s j_1 (v_2 ...) (e ...))))))

     (--> (s j (v ...) (in-hole L (v_1 ... (i32 const j_1) (call-indirect tf) e ...)))
          (s j (v ...) (in-hole L (v_1 ... (handle-call-indirect s j j_1 tf)))))
     ))