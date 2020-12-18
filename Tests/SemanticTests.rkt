#lang racket

(module+ test
  (require redex/reduction-semantics
           "../Semantics.rkt"
           "../Bits.rkt"
           rackunit)

  ;; Tests of unops
  (test-->>E -> ;; clz 0
             (term ((() () ()) 0 () ((i32 const 0) (i32 clz))))
             (term ((() () ()) 0 () ((i32 const 32)))))
  (test-->>E -> ;; clz #xFFFFFFFF
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 clz))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; clz single bit
             (term ((() () ()) 0 () ((i32 const #x00800000) (i32 clz))))
             (term ((() () ()) 0 () ((i32 const 8)))))
  (test-->>E -> ;; ctz 0
             (term ((() () ()) 0 () ((i32 const 0) (i32 ctz))))
             (term ((() () ()) 0 () ((i32 const 32)))))
  (test-->>E -> ;; ctz #xFFFFFFFF
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 ctz))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; ctz single bit
             (term ((() () ()) 0 () ((i32 const #x00800000) (i32 ctz))))
             (term ((() () ()) 0 () ((i32 const 23)))))
  (test-->>E -> ;; popcnt 0
             (term ((() () ()) 0 () ((i32 const 0) (i32 popcnt))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; popcnt #xFFFFFFFF
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 popcnt))))
             (term ((() () ()) 0 () ((i32 const 32)))))
  (test-->>E -> ;; popcnt #xDEADBEEF
             (term ((() () ()) 0 () ((i32 const #xDEADBEEF) (i32 popcnt))))
             (term ((() () ()) 0 () ((i32 const 24)))))

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
  (test-->>E -> ;; div-s
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 div-s))))
             (term ((() () ()) 0 () ((i32 const 2)))))
  (test-->>E -> ;; div-s -1
             (term ((() () ()) 0 () ((i32 const 2) (i32 const #xFFFFFFFF) (i32 div-s))))
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFE)))))
  (test-->>E -> ;; div-s 0
             (term ((() () ()) 0 () ((i32 const 1) (i32 const 0) (i32 div-s))))
             (term ((() () ()) 0 () ((trap)))))
  (test-->>E -> ;; div-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 div-u))))
             (term ((() () ()) 0 () ((i32 const 2)))))
  (test-->>E -> ;; div-u -1
             (term ((() () ()) 0 () ((i32 const 2) (i32 const #xFFFFFFFF) (i32 div-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; div-u 0
             (term ((() () ()) 0 () ((i32 const 1) (i32 const 0) (i32 div-u))))
             (term ((() () ()) 0 () ((trap)))))
  (test-->>E -> ;; rem-s
             (term ((() () ()) 0 () ((i32 const 3) (i32 const 2) (i32 rem-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; rem-s 3 -2
             (term ((() () ()) 0 () ((i32 const 1) (i32 const #xFFFFFFFE) (i32 rem-s))))
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF)))))
  (test-->>E -> ;; rem-s 0
             (term ((() () ()) 0 () ((i32 const 1) (i32 const 0) (i32 rem-s))))
             (term ((() () ()) 0 () ((trap)))))
  (test-->>E -> ;; rem-u
             (term ((() () ()) 0 () ((i32 const 3) (i32 const 2) (i32 rem-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; rem-u 1 -2
             (term ((() () ()) 0 () ((i32 const 3) (i32 const #xFFFFFFFE) (i32 rem-u))))
             (term ((() () ()) 0 () ((i32 const 3)))))
  (test-->>E -> ;; rem-u 0
             (term ((() () ()) 0 () ((i32 const 1) (i32 const 0) (i32 rem-u))))
             (term ((() () ()) 0 () ((trap)))))
  (test-->>E -> ;; and
             (term ((() () ()) 0 () ((i32 const 5) (i32 const 3) (i32 and))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; or
             (term ((() () ()) 0 () ((i32 const 5) (i32 const 3) (i32 or))))
             (term ((() () ()) 0 () ((i32 const 7)))))
  (test-->>E -> ;; xor
             (term ((() () ()) 0 () ((i32 const 5) (i32 const 3) (i32 xor))))
             (term ((() () ()) 0 () ((i32 const 6)))))
  (test-->>E -> ;; shl
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 shl))))
             (term ((() () ()) 0 () ((i32 const 4)))))
  (test-->>E -> ;; shl overflow
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 31) (i32 shl))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; shr-s
             (term ((() () ()) 0 () ((i32 const 4) (i32 const 1) (i32 shr-s))))
             (term ((() () ()) 0 () ((i32 const 2)))))
  (test-->>E -> ;; shr-s underflow
             (term ((() () ()) 0 () ((i32 const 4) (i32 const 3) (i32 shr-s))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; shr-s negative
             (term ((() () ()) 0 () ((i32 const #x80000000) (i32 const 31) (i32 shr-s))))
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF)))))
  (test-->>E -> ;; shr-u
             (term ((() () ()) 0 () ((i32 const 4) (i32 const 1) (i32 shr-u))))
             (term ((() () ()) 0 () ((i32 const 2)))))
  (test-->>E -> ;; shr-u underflow
             (term ((() () ()) 0 () ((i32 const 4) (i32 const 3) (i32 shr-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; shr-u negative
             (term ((() () ()) 0 () ((i32 const #x80000000) (i32 const 31) (i32 shr-u))))
             (term ((() () ()) 0 () ((i32 const #x00000001)))))
  (test-->>E -> ;; rotl
             (term ((() () ()) 0 () ((i32 const #xFF0000FF) (i32 const 8) (i32 rotl))))
             (term ((() () ()) 0 () ((i32 const #x0000FFFF)))))
  (test-->>E -> ;; rotl far
             (term ((() () ()) 0 () ((i32 const #xFF0000FF) (i32 const 40) (i32 rotl))))
             (term ((() () ()) 0 () ((i32 const #x0000FFFF)))))
  (test-->>E -> ;; rotr
             (term ((() () ()) 0 () ((i32 const #xFF0000FF) (i32 const 8) (i32 rotr))))
             (term ((() () ()) 0 () ((i32 const #xFFFF0000)))))
  (test-->>E -> ;; rotr far
             (term ((() () ()) 0 () ((i32 const #xFF0000FF) (i32 const 40) (i32 rotr))))
             (term ((() () ()) 0 () ((i32 const #xFFFF0000)))))
  
  (test-->>E -> ;; i64 math
             (term ((() () ()) 0 () ((i64 const 4294967295) (i64 const 2) (i64 mul))))
             (term ((() () ()) 0 () ((i64 const 8589934590)))))

  ;; TODO: testop, relop

  ;; cvtop
  (test-->>E -> ;; i64 1 -> i32 1
             (term ((() () ()) 0 () ((i64 const 1) (i32 wrap i64))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; i64 -1 -> i32 -1
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFFFFFFFFFF) (i32 wrap i64))))
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF)))))
  (test-->>E -> ;; i32 1 -> i64 1
             (term ((() () ()) 0 () ((i32 const 1) (i64 extend-s i32))))
             (term ((() () ()) 0 () ((i64 const 1)))))
  (test-->>E -> ;; i32 -1 -> i64 -1
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i64 extend-s i32))))
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFFFFFFFFFF)))))
  (test-->>E -> ;; i32 1 -> i64 1 unsigned
             (term ((() () ()) 0 () ((i32 const 1) (i64 extend-u i32))))
             (term ((() () ()) 0 () ((i64 const 1)))))
  (test-->>E -> ;; i32 -1 -> i64 -1 unsigned
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i64 extend-u i32))))
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFF)))))

  ;; nop, drop
  (test-->>E -> ;; nop
             (term ((() () ()) 0 () ((i32 const 0) (nop))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; drop
             (term ((() () ()) 0 () ((i32 const 0) (i32 const 1) (drop))))
             (term ((() () ()) 0 () ((i32 const 0)))))

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

  (test-->>E -> ;; call cl, return ensure branching with instructions after local
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ((call (0 (() (func (() -> (i32)) (local () ((i32 const 42) (return) (unreachable))))))) (i32 const 2) (i32 add))))
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ((i32 const 44)))))

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

  (test-->>E -> ;; store i32, load i8 (test of endianness)
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(make-memory 128))))
                    0
                    ()
                    ((i32 const 0)
                     (i32 const #x12345678)
                     (i32 store 0 4)
                     (i32 const 0)
                     (i32 load (i8 unsigned) 0 4))))
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(store (make-memory 128) 32 32 #x12345678))))
                    0
                    ()
                    ((i32 const #x78))))) ; would be #x12 if big-endian

  (test-->>E -> ;; store 255, load signed i8
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(make-memory 128))))
                    0
                    ()
                    ((i32 const 0)
                     (i32 const #xFF)
                     (i32 store 0 4)
                     (i32 const 0)
                     (i32 load (i8 signed) 0 4))))
             (term ((((() () (table) (memory 0)))
                     ()
                     ((bits ,(store (make-memory 128) 32 32 #xFF))))
                    0
                    ()
                    ((i32 const #xFFFFFFFF)))))

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

  ;; TODO: grow memory

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

  (test-->>E -> ;; loop with trap (trap inside label with instructions)
             (term ((() () ())
                    0
                    ()
                    ((loop (() -> ())
                           ((unreachable))))))
             (term ((() () ())
                    0
                    ()
                    ((trap)))))

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