#lang racket

(require redex/reduction-semantics
         redex/gui
         "../Syntax.rkt"
         "../Utilities.rkt"
         "../Semantics.rkt")

;; Tests of br
(test-->>∃ ->
           (term (() ((block (() -> ()) ((block (() -> ()) ((i32 const 0) (i32 const 1) (add i32) (br-if 1))) (unreachable))))))
           (term (() ())))

(test-->>∃ ->
           (term (() ((block (() -> ()) ((block (() -> ()) ((br 0))) (unreachable))))))
           (term (() ((trap)))))