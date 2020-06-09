#lang racket

(module+ test
  (require redex/reduction-semantics
           "../Semantics.rkt"
           "../Bits.rkt"
           rackunit)

  ;; Tests of simple binops
  (test-->>E -> ;; add
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 add))))
             (term ((() () ()) 0 () ((i32 const 3)))))
  (test-->>E -> ;; sub
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 sub))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; mul
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 mul))))
             (term ((() () ()) 0 () ((i32 const 2)))))
  (test-->>E -> ;; div
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 div))))
             (term ((() () ()) 0 () ((i32 const 2)))))
  (test-->>E -> ;; div 0
             (term ((() () ()) 0 () ((i32 const 1) (i32 const 0) (i32 div))))
             (term ((() () ()) 0 () ((trap)))))
  (test-->>E -> ;; rem
             (term ((() () ()) 0 () ((i32 const 3) (i32 const 2) (i32 rem))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; rem 0
             (term ((() () ()) 0 () ((i32 const 1) (i32 const 0) (i32 rem))))
             (term ((() () ()) 0 () ((trap)))))
  (test-->>E -> ;; and
             (term ((() () ()) 0 () ((i32 const 3) (i32 const 2) (i32 and))))
             (term ((() () ()) 0 () ((i32 const 2)))))
  (test-->>E -> ;; or
             (term ((() () ()) 0 () ((i32 const 3) (i32 const 6) (i32 or))))
             (term ((() () ()) 0 () ((i32 const 7)))))
  (test-->>E -> ;; xor
             (term ((() () ()) 0 () ((i32 const 3) (i32 const 6) (i32 xor))))
             (term ((() () ()) 0 () ((i32 const 5)))))
  #;(test-->>E -> ;; shl OMITTED
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 shl))))
             (term ((() () ()) 0 () ((i32 const 4)))))
  #;(test-->>E -> ;; shr OMITTED
             (term ((() () ()) 0 () ((i32 const 4) (i32 const 1) (i32 shr))))
             (term ((() () ()) 0 () ((i32 const 2)))))

  ;; Tests of br (and unreachable)
  (test-->>E -> ;; jump out
             (term ((() () ()) 0 () ((block (() -> ()) ((block (() -> ()) ((br 0))) (unreachable))))))
             (term ((() () ()) 0 () ((trap)))))
  (test-->>E -> ;; jump over
             (term ((() () ()) 0 () ((block (() -> ()) ((block (() -> ()) ((br 1))) (unreachable))))))
             (term ((() () ()) 0 () ())))
  (test-->>E -> ;; br-if true
             (term ((() () ()) 0 () ((block (() -> ()) ((i32 const 1) (br-if 0) (unreachable))))))
             (term ((() () ()) 0 () ())))
  (test-->>E -> ;; br-if false
             (term ((() () ()) 0 () ((block (() -> ()) ((i32 const 0) (br-if 0) (unreachable))))))
             (term ((() () ()) 0 () ((trap)))))

  ;; Tests of function calls
  (test-->>E -> ;; call j, call cl, get-local, return
             (term ((((() () (table) (memory))
                      (((0 (() (func ((i32) -> ()) (local () ((get-local 0))))))
                        (1 (() (func ((i32 i32) -> ()) (local () ((get-local 1))))))
                        (2 (() (func ((i32 i32 i32) -> ()) (local () ((get-local 2)))))))
                       () (table) (memory))
                      (() ()  (table) (memory)))
                     ()
                     ()) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 1) (i32 const 2) (call 1)))) ; e stream
             (term ((((() () (table) (memory))
                      (((0 (() (func ((i32) -> ()) (local () ((get-local 0))))))
                        (1 (() (func ((i32 i32) -> ()) (local () ((get-local 1))))))
                        (2 (() (func ((i32 i32 i32) -> ()) (local () ((get-local 2)))))))
                       () (table) (memory))
                      (() ()  (table) (memory)))
                     ()
                     ()) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 2))))) ; e stream

  (test-->>E -> ;; call j, call cl, get-local, return
             (term ((((() () (table) (memory))
                     (((0 (() (func ((i32) -> ()) (local () ((get-local 0))))))
                       (1 (() (func ((i32 i32) -> ()) (local () ((get-local 1))))))
                       (2 (() (func ((i32 i32 i32) -> ()) (local () ((get-local 2)))))))
                      () (table) (memory))
                     (() ()  (table) (memory)))
                     ()
                     ()) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 1) (i32 const 2) (call 1)))) ; e stream
             (term ((((() () (table) (memory))
                     (((0 (() (func ((i32) -> ()) (local () ((get-local 0))))))
                       (1 (() (func ((i32 i32) -> ()) (local () ((get-local 1))))))
                       (2 (() (func ((i32 i32 i32) -> ()) (local () ((get-local 2)))))))
                      () (table) (memory))
                     (() ()  (table) (memory)))
                     ()
                     ()) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 2))))) ; e stream

  (test-->>E -> ;; call cl, trap
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ((local (0 ()) ((unreachable)))))) ; e stream
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ((trap)))))

  (test-->>E -> ;; call cl, br
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ((call (0 (() (func (() -> ()) (local () ((br 0) (unreachable)))))))))) ; e stream
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ())))

  (test-->>E -> ;; call_indirect
             (term ((((() () (table 1) (memory)))
                    (((0 (() (func ((i32) -> (i32)) (local () ((get-local 0) (return))))))
                      (1 (() (func ((i32 i32) -> (i32)) (local () ((get-local 1) (return))))))
                      (2 (() (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) (return)))))))
                     ((3 (() (func ((i32) -> (i32)) (local () ((get-local 0) (return))))))
                      (4 (() (func ((i32 i32) -> (i32)) (local () ((get-local 1) (return))))))
                      (5 (() (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) (return))))))))
                    ())
                    0
                    ()
                    ((i32 const 2) (i32 const 3) (i32 const 1) (call-indirect ((i32 i32) -> (i32))))))
             (term ((((() () (table 1) (memory)))
                    (((0 (() (func ((i32) -> (i32)) (local () ((get-local 0) (return))))))
                      (1 (() (func ((i32 i32) -> (i32)) (local () ((get-local 1) (return))))))
                      (2 (() (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) (return)))))))
                     ((3 (() (func ((i32) -> (i32)) (local () ((get-local 0) (return))))))
                      (4 (() (func ((i32 i32) -> (i32)) (local () ((get-local 1) (return))))))
                      (5 (() (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) (return))))))))
                    ())
                    0
                    ()
                    ((i32 const 3)))))

  (test-->>E -> ;; call_indirect wrong type
             (term ((((() () (table 1) (memory)))
                    (((0 (() (func ((i32) -> (i32)) (local () ((get-local 0) (return))))))
                      (1 (() (func ((i32 i32) -> (i32)) (local () ((get-local 1) (return))))))
                      (2 (() (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) (return)))))))
                     ((3 (() (func ((i32) -> (i32)) (local () ((get-local 0) (return))))))
                      (4 (() (func ((i32 i32) -> (i32)) (local () ((get-local 1) (return))))))
                      (5 (() (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) (return))))))))
                    ())
                    0
                    ()
                    ((i32 const 2) (i32 const 3) (i32 const 1) (call-indirect ((i64) -> (i64))))))
             (term ((((() () (table 1) (memory)))
                    (((0 (() (func ((i32) -> (i32)) (local () ((get-local 0) (return))))))
                      (1 (() (func ((i32 i32) -> (i32)) (local () ((get-local 1) (return))))))
                      (2 (() (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) (return)))))))
                     ((3 (() (func ((i32) -> (i32)) (local () ((get-local 0) (return))))))
                      (4 (() (func ((i32 i32) -> (i32)) (local () ((get-local 1) (return))))))
                      (5 (() (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) (return))))))))
                    ())
                    0
                    ()
                    ((trap)))))

  (test-->>E -> ;; store than load
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(make-memory 128))))
                    0
                    ()
                    ((i32 const 0) (i32 const 0) (i64 const 65) (i64 store 0 8) (i64 load 0 8))))
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(store (make-memory 128) 64 64 65))))
                    0
                    ()
                    ((i64 const 65)))))

  (test-->>E -> ;; store out-of-bounds than load
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(make-memory 128))))
                    0
                    ()
                    ((i32 const 0) (i32 const 0) (i64 const 65) (i64 store 0 200) (i64 load 0 8))))
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(make-memory 128))))
                    0
                    ()
                    ((trap)))))

  (test-->>E -> ;; store than load out-of-bounds
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(make-memory 128))))
                    0
                    ()
                    ((i32 const 0) (i32 const 0) (i64 const 65) (i64 store 0 8) (i64 load 0 200))))
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(store (make-memory 128) 64 64 65))))
                    0
                    ()
                    ((trap)))))

  (test-->>E -> ;; current-memory
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(make-memory 128))))
                    0
                    ()
                    ((current-memory))))
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(make-memory 128))))
                    0
                    ()
                    ((i32 const 128)))))

  (test-->>E -> ;; if-true
             (term ((() () ())
                    0
                    ()
                    ((i32 const 1)
                     (if (() -> (i32))
                         ((i32 const 2))
                         else
                         ((i32 const 3))))))
             (term ((() () ())
                    0
                    ()
                    ((i32 const 2)))))

    (test-->>E -> ;; if-false
             (term ((() () ())
                    0
                    ()
                    ((i32 const 0)
                     (if (() -> (i32))
                         ((i32 const 2))
                         else
                         ((i32 const 3))))))
             (term ((() () ())
                    0
                    ()
                    ((i32 const 3)))))
  

  (test-->>E -> ;; loop, if
               (term ((((((0 (() (func ((i32) -> (i32))
                                       (local (i32) ((loop (() -> (i32))
                                                           ((get-local 0)
                                                            (if (() -> (i32))
                                                                ((get-local 0)
                                                                 (get-local 1)
                                                                 (i32 add)
                                                                 (set-local 1)
                                                                 (get-local 0)
                                                                 (i32 const 1)
                                                                 (i32 sub)
                                                                 (set-local 0)
                                                                 (br 1))
                                                                else
                                                                ((get-local 1)))))))))))
                         () (table) (memory))) ; store
                       () ())
                      0
                      ()
                      ((i32 const 5) (call 0))))
             (term ((((((0 (() (func ((i32) -> (i32))
                                     (local (i32) ((loop (() -> (i32))
                                                         ((get-local 0)
                                                          (if (() -> (i32))
                                                              ((get-local 0)
                                                               (get-local 1)
                                                               (i32 add)
                                                               (set-local 1)
                                                               (get-local 0)
                                                               (i32 const 1)
                                                               (i32 sub)
                                                               (set-local 0)
                                                               (br 1))
                                                              else
                                                              ((get-local 1)))))))))))
                       () (table) (memory))) ; store
                     () ())
                    0
                    ()
                    ((i32 const 15)))))
  )