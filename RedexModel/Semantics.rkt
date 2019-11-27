#lang racket

(require redex/reduction-semantics
         redex/gui
         "Syntax.rkt"
         "Utilities.rkt")

(provide ->)

;; TL;DR about stack: the stack is implicit in the stream of instructions being processed.
;; This is because v \subset e, so although we say (e ...) it ends up looking like (v ... e ...).
;; Thus, the next instruction to execute is the head of e ..., and the stack is v ...
(define ->
    (reduction-relation
     WASMrt
     #:domain ((v ...) (e ...))
     #:codomain ((v ...) (e ...))
     ;; ((v ...) (e ...)) -> ((v ...) (e ...))
     ;; Every instruction is operating inside of an execution environment.
     ;; We need these contexts to keep track of the stack and next instructions at each level,
     ;; and to handle branching/returning from levels.
     ;; Much of the instructions don't need to know about the environment they are executing in, but some do.
     ;; Todo: It would be nice to reduce the boilerplate for those that don't.
     
     ;; Due to validation we can be sure we are returning the proper number of values
     ;; Todo: Add store.
     (--> ((v ...) (in-hole L (v_1 ... (t const c_1) (t const c_2) (t binop) e ...)))
          ((v ...) (in-hole L (v_1 ... (eval-binop binop c_1 c_2 t) e ...))))
     
     (--> ((v ...) (in-hole L (v_1 ... (t const c) (t testop) e ...)))
          ((v ...) (in-hole L (v_1 ... (eval-testop testop c t) e ...))))
     
     (--> ((v ...) (in-hole L (v_1 ... (t const c_1) (t const c_2) (t relop) e ...)))
          ((v ...) (in-hole L (v_1 ... (eval-relop relop c_1 c_2 t) e ...))))
          
     (--> ((v ...) (in-hole L (v_1 ... (nop) e ...)))
          ((v ...) (in-hole L (v_1 ... e ...))))

     (--> ((v ...) (in-hole L (v_1 ... (unreachable) e ...)))
          ((v ...) (in-hole L (v_1 ... (trap) e ...))))

     (--> ((v ...) (in-hole L (v_1 ... v_2 v_3 (i32 const 0) (select) e ...)))
          ((v ...) (in-hole L (v_1 ... v_3 e ...))))

     (--> ((v ...) (in-hole L (v_1 ... v_2 v_3 (i32 const c) (select) e ...)))
          ((v ...) (in-hole L (v_1 ... v_2 e ...)))
          (side-condition (> (term c) 0)))
     
     (--> ((v ...) (in-hole L (v_1 ... v_2 (drop) e ...)))
          ((v ...) (in-hole L (v_1 ... e ...))))
     
     (--> ((v ...) (in-hole L (v_1 ... (i32 const 0) (if tf (e_1 ...) else (e_2 ...)) e ...)))
          ((v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e ...))))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const c) (if tf (e_1 ...) else (e_2 ...)) e ...)))
          ((v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e ...)))
          (side-condition (> (term c) 0)))
     
     (--> ((v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e_2 ...)))
          ((v ...) (in-hole L (v_1 ... (label () (e_1 ...)) e_2 ...))))

     (--> ((v ...) (in-hole L (v_1 ... (get-local j) e ...)))
          ((v ...) (in-hole L (v_1 ... ,(car (drop (term (v ...)) (term j))) e ...))))
     
     (--> ((v ...) (in-hole L (v_1 ... v_2 (set-local j) e ...)))
          (,(append (take (term (v ...)) (term j)) (term (v_2)) (drop (term (v ...)) (add1 (term j))))
           (in-hole L (v_1 ... e ...))))

     (--> ((v ...) (in-hole L (v_1 ... (label () (v_2 ... (trap) e ...) e_2 ...))))
          ((v ...) (in-hole L ((trap)))))

     ;; Knowing about contexts is necessary for this (so can't shortcut the rest :/)!
     (--> ((v ...) (in-hole L (v_1 ... (br j) e ...)))
          ((v ...) (decompose L j (v_1 ...))))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const 0) (br-if j) e ...)))
          ((v ...) (in-hole L (v_1 ... e ...))))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const c) (br-if j) e ...)))
          ((v ...) (in-hole L (v_1 ... (br j) e ...)))
          (side-condition (> (term c) 0)))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const c) (br-table (j ...)) e ...)))
          ((v ...) (in-hole L (v_1 ... v_2 (br ,(car (drop (term (j ...)) (term c)))) e ...)))
          (side-condition (<= (term c) (length (term (j ...))))))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const c) (br-table (j ...)) e ...)))
          ((v ...) (in-hole L (v_1 ... v_2 (br ,(last (term (j ...)))) e ...)))
          (side-condition (> (term c) (length (term (j ...))))))

     (--> ((v ...) (in-hole L (v_1 ... (label () (v_2 ...)) e ...)))
          ((v ...) (v_1 ... v_2 ... e ...)))
     ))