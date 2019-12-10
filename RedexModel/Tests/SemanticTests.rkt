#lang racket

(module+ test
  (require redex/reduction-semantics
           "../Semantics.rkt"
           rackunit)

  ;; Tests of simple binops
  (test-->>E -> ;; add
             (term (() 0 () ((i32 const 2) (i32 const 1) (i32 add))))
             (term (() 0 () ((i32 const 3)))))
  (test-->>E -> ;; sub
             (term (() 0 () ((i32 const 2) (i32 const 1) (i32 sub))))
             (term (() 0 () ((i32 const 1)))))
  (test-->>E -> ;; mul
             (term (() 0 () ((i32 const 2) (i32 const 1) (i32 mul))))
             (term (() 0 () ((i32 const 2)))))
  (test-->>E -> ;; div
             (term (() 0 () ((i32 const 2) (i32 const 1) (i32 div))))
             (term (() 0 () ((i32 const 2)))))
  (test-->>E -> ;; div 0
             (term (() 0 () ((i32 const 1) (i32 const 0) (i32 div))))
             (term (() 0 () ((trap)))))
  (test-->>E -> ;; rem
             (term (() 0 () ((i32 const 3) (i32 const 2) (i32 rem))))
             (term (() 0 () ((i32 const 1)))))
  (test-->>E -> ;; rem 0
             (term (() 0 () ((i32 const 1) (i32 const 0) (i32 rem))))
             (term (() 0 () ((trap)))))
  (test-->>E -> ;; and
             (term (() 0 () ((i32 const 3) (i32 const 2) (i32 and))))
             (term (() 0 () ((i32 const 2)))))
  (test-->>E -> ;; or
             (term (() 0 () ((i32 const 3) (i32 const 6) (i32 or))))
             (term (() 0 () ((i32 const 7)))))
  (test-->>E -> ;; xor
             (term (() 0 () ((i32 const 3) (i32 const 6) (i32 xor))))
             (term (() 0 () ((i32 const 5)))))
  #;(test-->>E -> ;; shl OMITTED
             (term (() 0 () ((i32 const 2) (i32 const 1) (i32 shl))))
             (term (() 0 () ((i32 const 4)))))
  #;(test-->>E -> ;; shr OMITTED
             (term (() 0 () ((i32 const 4) (i32 const 1) (i32 shr))))
             (term (() 0 () ((i32 const 2)))))

  ;; Tests of br (and unreachable)
  (test-->>E -> ;; jump out
             (term (() 0 () ((block (() -> ()) ((block (() -> ()) ((br 0))) (unreachable))))))
             (term (() 0 () ((trap)))))
  (test-->>E -> ;; jump over
             (term (() 0 () ((block (() -> ()) ((block (() -> ()) ((br 1))) (unreachable))))))
             (term (() 0 () ())))
  (test-->>E -> ;; br-if true
             (term (() 0 () ((block (() -> ()) ((i32 const 1) (br-if 0) (unreachable))))))
             (term (() 0 () ())))
  (test-->>E -> ;; br-if false
             (term (() 0 () ((block (() -> ()) ((i32 const 0) (br-if 0) (unreachable))))))
             (term (() 0 () ((trap)))))

  ;; Tests of function calls
  (test-->>E -> ;; call j, call cl, get-local, return
             (term (((() ())
                     (((0 (func ((i32) -> ()) (local () ((get-local 0) (return)))))
                       (1 (func ((i32 i32) -> ()) (local () ((get-local 1) (return)))))
                       (2 (func ((i32 i32 i32) -> ()) (local () ((get-local 2) (return)))))) ())
                     (() ())) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 1) (i32 const 2) (call 1)))) ; e stream
             (term (((() ())
                     (((0 (func ((i32) -> ()) (local () ((get-local 0) (return)))))
                       (1 (func ((i32 i32) -> ()) (local () ((get-local 1) (return)))))
                       (2 (func ((i32 i32 i32) -> ()) (local () ((get-local 2) (return)))))) ())
                     (() ())) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 2))))) ; e stream

  (test-->>E -> ;; call j, call cl, get-local, return
             (term (((() ())
                     (((0 (func ((i32) -> ()) (local () ((get-local 0)))))
                       (1 (func ((i32 i32) -> ()) (local () ((get-local 1)))))
                       (2 (func ((i32 i32 i32) -> ()) (local () ((get-local 2)))))) ())
                     (() ())) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 1) (i32 const 2) (call 1)))) ; e stream
             (term (((() ())
                     (((0 (func ((i32) -> ()) (local () ((get-local 0)))))
                       (1 (func ((i32 i32) -> ()) (local () ((get-local 1)))))
                       (2 (func ((i32 i32 i32) -> ()) (local () ((get-local 2)))))) ())
                     (() ())) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 2))))) ; e stream

  (test-->>E -> ;; call cl, trap
             (term (() ; store
                    0 ; inst
                    () ; locals
                    ((local (0 ()) ((unreachable)))))) ; e stream
             (term (() ; store
                    0 ; inst
                    () ; locals
                    ((trap)))))

  (test-->>E -> ;; call cl, br
             (term (() ; store
                    0 ; inst
                    () ; locals
                    ((call (0 (func (() -> ()) (local () ((br 0) (unreachable))))))))) ; e stream
             (term (() ; store
                    0 ; inst
                    () ; locals
                    ())))
  )