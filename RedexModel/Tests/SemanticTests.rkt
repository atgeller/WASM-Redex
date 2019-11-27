#lang racket

(module+ test
  (require redex/reduction-semantics
           redex/gui
           "../Syntax.rkt"
           "../Utilities.rkt"
           "../Semantics.rkt"
           rackunit)

  ;; Tests of simple binops
  (test-->>E -> ;; add
             (term (() ((i32 const 2) (i32 const 1) (i32 add))))
             (term (() ((i32 const 3)))))
  (test-->>E -> ;; sub
             (term (() ((i32 const 2) (i32 const 1) (i32 sub))))
             (term (() ((i32 const 1)))))
  (test-->>E -> ;; mul
             (term (() ((i32 const 2) (i32 const 1) (i32 mul))))
             (term (() ((i32 const 2)))))
  (test-->>E -> ;; div
             (term (() ((i32 const 2) (i32 const 1) (i32 div))))
             (term (() ((i32 const 2)))))
  (test-->>E -> ;; div 0
             (term (() ((i32 const 1) (i32 const 0) (i32 div))))
             (term (() ((trap)))))
  (test-->>E -> ;; rem
             (term (() ((i32 const 3) (i32 const 2) (i32 rem))))
             (term (() ((i32 const 1)))))
  (test-->>E -> ;; rem 0
             (term (() ((i32 const 1) (i32 const 0) (i32 rem))))
             (term (() ((trap)))))
  (test-->>E -> ;; and
             (term (() ((i32 const 3) (i32 const 2) (i32 and))))
             (term (() ((i32 const 2)))))
  (test-->>E -> ;; or
             (term (() ((i32 const 3) (i32 const 6) (i32 or))))
             (term (() ((i32 const 7)))))
  (test-->>E -> ;; xor
             (term (() ((i32 const 3) (i32 const 6) (i32 xor))))
             (term (() ((i32 const 5)))))
  #;(test-->>E -> ;; shl OMITTED
             (term (() ((i32 const 2) (i32 const 1) (i32 shl))))
             (term (() ((i32 const 4)))))
  #;(test-->>E -> ;; shr OMITTED
             (term (() ((i32 const 4) (i32 const 1) (i32 shr))))
             (term (() ((i32 const 2)))))

  ;; Tests of br (and unreachable)
  (test-->>E -> ;; jump out
             (term (() ((block (() -> ()) ((block (() -> ()) ((br 0))) (unreachable))))))
             (term (() ((trap)))))
  (test-->>E -> ;; jump over
             (term (() ((block (() -> ()) ((block (() -> ()) ((br 1))) (unreachable))))))
             (term (() ())))
  (test-->>E -> ;; br-if true
             (term (() ((block (() -> ()) ((i32 const 1) (br-if 0) (unreachable))))))
             (term (() ())))
  (test-->>E -> ;; br-if false
             (term (() ((block (() -> ()) ((i32 const 0) (br-if 0) (unreachable))))))
             (term (() ((trap)))))
  )