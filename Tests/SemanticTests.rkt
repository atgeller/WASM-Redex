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

  (test-->>E -> ;; abs 3
             (term ((() () ()) 0 () ((f64 const 3.0) (f64 abs))))
             (term ((() () ()) 0 () ((f64 const 3.0)))))
  (test-->>E -> ;; abs -3
             (term ((() () ()) 0 () ((f64 const -3.0) (f64 abs))))
             (term ((() () ()) 0 () ((f64 const 3.0)))))
  (test-->>E -> ;; neg 3
             (term ((() () ()) 0 () ((f64 const 3.0) (f64 neg))))
             (term ((() () ()) 0 () ((f64 const -3.0)))))
  (test-->>E -> ;; neg -3
             (term ((() () ()) 0 () ((f64 const -3.0) (f64 neg))))
             (term ((() () ()) 0 () ((f64 const 3.0)))))
  (test-->>E -> ;; sqrt
             (term ((() () ()) 0 () ((f64 const 4.0) (f64 sqrt))))
             (term ((() () ()) 0 () ((f64 const 2.0)))))
  (test-->>E -> ;; sqrt
             (term ((() () ()) 0 () ((f64 const 4.0) (f64 sqrt) (f64 sqrt))))
             (term ((() () ()) 0 () ((f64 const 1.4142135623730951)))))
  (test-->>E -> ;; sqrt negative
             (term ((() () ()) 0 () ((f64 const -4.0) (f64 sqrt))))
             (term ((() () ()) 0 () ((f64 const +nan.0)))))
  (test-->>E -> ;; ceil
             (term ((() () ()) 0 () ((f64 const ,pi) (f64 ceil))))
             (term ((() () ()) 0 () ((f64 const 4.0)))))
  (test-->>E -> ;; floor
             (term ((() () ()) 0 () ((f64 const ,pi) (f64 floor))))
             (term ((() () ()) 0 () ((f64 const 3.0)))))
  (test-->>E -> ;; nearest
             (term ((() () ()) 0 () ((f64 const ,pi) (f64 nearest))))
             (term ((() () ()) 0 () ((f64 const 3.0)))))
  (test-->>E -> ;; nearest
             (term ((() () ()) 0 () ((f64 const 1.5) (f64 nearest))))
             (term ((() () ()) 0 () ((f64 const 2.0)))))

  (test-->>E -> ;; f32 sqrt
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum 2.0f0)) (f32 sqrt))))
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum 1.4142135f0))))))
  (test-->>E -> ;; f32 sqrt negative
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum -2.0f0)) (f32 sqrt))))
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum +nan.f))))))

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
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; div-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 div-u))))
             (term ((() () ()) 0 () ((i32 const 2)))))
  (test-->>E -> ;; div-u -1
             (term ((() () ()) 0 () ((i32 const 2) (i32 const #xFFFFFFFF) (i32 div-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; div-u 0
             (term ((() () ()) 0 () ((i32 const 1) (i32 const 0) (i32 div-u))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; rem-s
             (term ((() () ()) 0 () ((i32 const 3) (i32 const 2) (i32 rem-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; rem-s 3 -2
             (term ((() () ()) 0 () ((i32 const 1) (i32 const #xFFFFFFFE) (i32 rem-s))))
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF)))))
  (test-->>E -> ;; rem-s 0
             (term ((() () ()) 0 () ((i32 const 1) (i32 const 0) (i32 rem-s))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; rem-u
             (term ((() () ()) 0 () ((i32 const 3) (i32 const 2) (i32 rem-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; rem-u 1 -2
             (term ((() () ()) 0 () ((i32 const 3) (i32 const #xFFFFFFFE) (i32 rem-u))))
             (term ((() () ()) 0 () ((i32 const 3)))))
  (test-->>E -> ;; rem-u 0
             (term ((() () ()) 0 () ((i32 const 1) (i32 const 0) (i32 rem-u))))
             (term ((() () ()) 0 () (trap))))
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

  (test-->>E -> ;; f64 add
             (term ((() () ()) 0 () ((f64 const 0.1) (f64 const 0.2) (f64 add))))
             (term ((() () ()) 0 () ((f64 const 0.30000000000000004)))))
  (test-->>E -> ;; f64 sub
             (term ((() () ()) 0 () ((f64 const 0.3) (f64 const 0.2) (f64 sub))))
             (term ((() () ()) 0 () ((f64 const 0.09999999999999998)))))
  (test-->>E -> ;; f64 mul
             (term ((() () ()) 0 () ((f64 const 42.0) (f64 const 0.1) (f64 mul))))
             (term ((() () ()) 0 () ((f64 const 4.2)))))
  (test-->>E -> ;; f64 mul
             (term ((() () ()) 0 () ((f64 const 42.0) (f64 const 0.0) (f64 mul))))
             (term ((() () ()) 0 () ((f64 const 0.0)))))
  (test-->>E -> ;; f64 mul
             (term ((() () ()) 0 () ((f64 const -42.0) (f64 const 0.0) (f64 mul))))
             (term ((() () ()) 0 () ((f64 const -0.0)))))
  (test-->>E -> ;; f64 div
             (term ((() () ()) 0 () ((f64 const 5.0) (f64 const 2.5) (f64 div))))
             (term ((() () ()) 0 () ((f64 const 2.0)))))
  (test-->>E -> ;; f64 div
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const 0.0) (f64 div))))
             (term ((() () ()) 0 () ((f64 const +inf.0)))))
  (test-->>E -> ;; f64 div
             (term ((() () ()) 0 () ((f64 const -1.0) (f64 const 0.0) (f64 div))))
             (term ((() () ()) 0 () ((f64 const -inf.0)))))
  (test-->>E -> ;; f64 min
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const 0.1) (f64 min))))
             (term ((() () ()) 0 () ((f64 const 0.1)))))
  (test-->>E -> ;; f64 min
             (term ((() () ()) 0 () ((f64 const +inf.0) (f64 const -inf.0) (f64 min))))
             (term ((() () ()) 0 () ((f64 const -inf.0)))))
  (test-->>E -> ;; f64 max
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const 0.1) (f64 max))))
             (term ((() () ()) 0 () ((f64 const 1.0)))))
  (test-->>E -> ;; f64 max
             (term ((() () ()) 0 () ((f64 const +inf.0) (f64 const -inf.0) (f64 max))))
             (term ((() () ()) 0 () ((f64 const +inf.0)))))
  (test-->>E -> ;; f64 copysign
             (term ((() () ()) 0 () ((f64 const 2.0) (f64 const -3.0) (f64 copysign))))
             (term ((() () ()) 0 () ((f64 const -2.0)))))
  (test-->>E -> ;; f64 copysign
             (term ((() () ()) 0 () ((f64 const 2.0) (f64 const -0.0) (f64 copysign))))
             (term ((() () ()) 0 () ((f64 const -2.0)))))
  (test-->>E -> ;; f64 copysign
             (term ((() () ()) 0 () ((f64 const -3.0) (f64 const 2.0) (f64 copysign))))
             (term ((() () ()) 0 () ((f64 const 3.0)))))

  ;; Cursed becuase Racket has an understandable hatred for reading single precision floats
  (test-->>E -> ;; f32 add
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum 0.2f0)) (f32 const ,(real->single-flonum 0.1f0)) (f32 add))))
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum 0.3f0))))))

  (test-->>E -> ;; eqz false
             (term ((() () ()) 0 () ((i32 const 8) (i32 eqz))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; eqz true
             (term ((() () ()) 0 () ((i32 const 0) (i32 eqz))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  
  (test-->>E -> ;; eq true
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 eq))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; eq false
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 43) (i32 eq))))
             (term ((() () ()) 0 () ((i32 const 0)))))

  (test-->>E -> ;; eq float true
             (term ((() () ()) 0 () ((f64 const 42.0) (f64 const 42.0) (f64 eq))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; eq float true
             (term ((() () ()) 0 () ((f64 const 0.0) (f64 const -0.0) (f64 eq))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; eq float false
             (term ((() () ()) 0 () ((f64 const 0.0) (f64 const 42.0) (f64 eq))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  
  (test-->>E -> ;; ne false
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 ne))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; ne true
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 43) (i32 ne))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  
  (test-->>E -> ;; ne float false
             (term ((() () ()) 0 () ((f64 const 42.0) (f64 const 42.0) (f64 ne))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; ne float false
             (term ((() () ()) 0 () ((f64 const 0.0) (f64 const -0.0) (f64 ne))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; ne float true
             (term ((() () ()) 0 () ((f64 const 0.0) (f64 const 42.0) (f64 ne))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  
  (test-->>E -> ;; lt-u
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 lt-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; lt-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 43) (i32 lt-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; lt-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 lt-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; lt-u signed
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 lt-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  
  (test-->>E -> ;; lt-s
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 lt-s))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; lt-s
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 3) (i32 lt-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; lt-s
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 lt-s))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; lt-s signed
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 lt-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
    
  (test-->>E -> ;; gt-u
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 gt-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; gt-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 3) (i32 gt-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; gt-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 gt-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; gt-u signed
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 gt-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  
  (test-->>E -> ;; gt-s
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 gt-s))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; gt-s
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 3) (i32 gt-s))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; gt-s
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 gt-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; gt-s signed
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 gt-s))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  
  (test-->>E -> ;; le-u
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 le-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; le-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 3) (i32 le-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; le-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 le-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; le-u signed
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 le-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  
  (test-->>E -> ;; le-s
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 le-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; le-s
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 3) (i32 le-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; le-s
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 le-s))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; le-s signed
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 le-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  
  (test-->>E -> ;; ge-u
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 ge-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; ge-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 3) (i32 ge-u))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; ge-u
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 ge-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; ge-u signed
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 ge-u))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  
  (test-->>E -> ;; ge-s
             (term ((() () ()) 0 () ((i32 const 42) (i32 const 42) (i32 ge-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; ge-s
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 3) (i32 ge-s))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; ge-s
             (term ((() () ()) 0 () ((i32 const 2) (i32 const 1) (i32 ge-s))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; ge-s signed
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 ge-s))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  
  (test-->>E -> ;; lt
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const 1.0) (f64 lt))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; lt
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const -1.0) (f64 lt))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; lt
             (term ((() () ()) 0 () ((f64 const -1.0) (f64 const 1.0) (f64 lt))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  
  (test-->>E -> ;; gt
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const 1.0) (f64 gt))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; gt
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const -1.0) (f64 gt))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; gt
             (term ((() () ()) 0 () ((f64 const -1.0) (f64 const 1.0) (f64 gt))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  
  (test-->>E -> ;; le
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const 1.0) (f64 le))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; le
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const -1.0) (f64 le))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; le
             (term ((() () ()) 0 () ((f64 const -1.0) (f64 const 1.0) (f64 le))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  
  (test-->>E -> ;; ge
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const 1.0) (f64 ge))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; ge
             (term ((() () ()) 0 () ((f64 const 1.0) (f64 const -1.0) (f64 ge))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; ge
             (term ((() () ()) 0 () ((f64 const -1.0) (f64 const 1.0) (f64 ge))))
             (term ((() () ()) 0 () ((i32 const 0)))))

  ;; cvtop
  (test-->>E -> ;; i64 1 -> i32 1
             (term ((() () ()) 0 () ((i64 const 1) (i32 convert i64))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; i64 -1 -> i32 -1
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFFFFFFFFFF) (i32 convert i64))))
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF)))))
  (test-->>E -> ;; i32 1 -> i64 1
             (term ((() () ()) 0 () ((i32 const 1) (i64 convert i32 signed))))
             (term ((() () ()) 0 () ((i64 const 1)))))
  (test-->>E -> ;; i32 -1 -> i64 -1
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i64 convert i32 signed))))
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFFFFFFFFFF)))))
  (test-->>E -> ;; i32 1 -> i64 1 unsigned
             (term ((() () ()) 0 () ((i32 const 1) (i64 convert i32 unsigned))))
             (term ((() () ()) 0 () ((i64 const 1)))))
  (test-->>E -> ;; i32 -1 -> i64 -1 unsigned
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF) (i64 convert i32 unsigned))))
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFF)))))
  
  (test-->>E -> ;; f32 -> f64
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum 2.3f0)) (f64 convert f32))))
             (term ((() () ()) 0 () ((f64 const 2.299999952316284)))))
  (test-->>E -> ;; f64 -> f32
             (term ((() () ()) 0 () ((f64 const 2.3) (f32 convert f64))))
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum 2.3f0))))))
  (test-->>E -> ;; f64 -> f32 inf
             (term ((() () ()) 0 () ((f64 const 2.3e+40) (f32 convert f64))))
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum +inf.f))))))
  
  (test-->>E -> ;; f64 -> unsigned i64
             (term ((() () ()) 0 () ((f64 const ,pi) (i64 convert f64 unsigned))))
             (term ((() () ()) 0 () ((i64 const 3)))))
  (test-->>E -> ;; f64 -> unsigned i64 inf
             (term ((() () ()) 0 () ((f64 const +inf.0) (i64 convert f64 unsigned))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; f64 -> unsigned i64 nan
             (term ((() () ()) 0 () ((f64 const +nan.0) (i64 convert f64 unsigned))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; f64 -> unsigned i32 overflow
             (term ((() () ()) 0 () ((f64 const ,(expt 2.0 32)) (i32 convert f64 unsigned))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; f64 -> unsigned i32 underflow
             (term ((() () ()) 0 () ((f64 const -1.0) (i32 convert f64 unsigned))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; f64 -> signed i64
             (term ((() () ()) 0 () ((f64 const ,pi) (i64 convert f64 signed))))
             (term ((() () ()) 0 () ((i64 const 3)))))
  (test-->>E -> ;; f64 -> signed i64 inf
             (term ((() () ()) 0 () ((f64 const +inf.0) (i64 convert f64 signed))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; f64 -> signed i64 nan
             (term ((() () ()) 0 () ((f64 const +nan.0) (i64 convert f64 signed))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; f64 -> signed i32 overflow
             (term ((() () ()) 0 () ((f64 const ,(expt 2.0 32)) (i32 convert f64 signed))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; f64 -> signed i32 negative
             (term ((() () ()) 0 () ((f64 const -1.0) (i32 convert f64 signed))))
             (term ((() () ()) 0 () ((i32 const #xFFFFFFFF)))))
  (test-->>E -> ;; f64 -> signed i32 underflow
             (term ((() () ()) 0 () ((f64 const ,(sub1 (- (expt 2.0 31)))) (i32 convert f64 signed))))
             (term ((() () ()) 0 () (trap))))

  (test-->>E -> ;; unsigned i64 -> f64
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFFFFFFFFFF) (f64 convert i64 unsigned))))
             (term ((() () ()) 0 () ((f64 const 1.8446744073709552e+19)))))
  (test-->>E -> ;; signed i64 -> f64
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFFFFFFFFFF) (f64 convert i64 signed))))
             (term ((() () ()) 0 () ((f64 const -1.0)))))
  (test-->>E -> ;; unsigned i64 -> f32
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFFFFFFFFFF) (f32 convert i64 unsigned))))
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum 1.8446744f+19))))))
  (test-->>E -> ;; signed i64 -> f32
             (term ((() () ()) 0 () ((i64 const #xFFFFFFFFFFFFFFFF) (f32 convert i64 signed))))
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum -1.0f0))))))
  
  (test-->>E -> ;; reinterpret i32 -> f32 0
             (term ((() () ()) 0 () ((i32 const 0) (f32 reinterpret i32))))
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum 0.0f0))))))
  (test-->>E -> ;; reinterpret i32 -> f32 inf
             (term ((() () ()) 0 () ((i32 const #x7F800000) (f32 reinterpret i32))))
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum +inf.f))))))
  (test-->>E -> ;; reinterpret i64 -> f64 42.0
             (term ((() () ()) 0 () ((i64 const #x4045000000000000) (f64 reinterpret i64))))
             (term ((() () ()) 0 () ((f64 const 42.0)))))
  (test-->>E -> ;; reinterpret f32 -> i32 -0.0
             (term ((() () ()) 0 () ((f32 const ,(real->single-flonum -0.0f0)) (i32 reinterpret f32))))
             (term ((() () ()) 0 () ((i32 const #x80000000)))))
  (test-->>E -> ;; reinterpret f64 -> i64 -1.0
             (term ((() () ()) 0 () ((f64 const -1.0) (i64 reinterpret f64))))
             (term ((() () ()) 0 () ((i64 const #xBFF0000000000000)))))

  (test-->>E -> ;; select false
             (term ((() () ()) 0 () ((i64 const 0) (i64 const 1) (i32 const 0) select)))
             (term ((() () ()) 0 () ((i64 const 1)))))
  (test-->>E -> ;; select true
             (term ((() () ()) 0 () ((i64 const 0) (i64 const 1) (i32 const 1) select)))
             (term ((() () ()) 0 () ((i64 const 0)))))
  (test-->>E -> ;; select false
             (term ((() () ()) 0 () ((f64 const 0.0) (f64 const 1.0) (i32 const 0) select)))
             (term ((() () ()) 0 () ((f64 const 1.0)))))
  (test-->>E -> ;; select true
             (term ((() () ()) 0 () ((f64 const 0.0) (f64 const 1.0) (i32 const 1234) select)))
             (term ((() () ()) 0 () ((f64 const 0.0)))))

  ;; nop, drop
  (test-->>E -> ;; nop
             (term ((() () ()) 0 () ((i32 const 0) nop)))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; drop
             (term ((() () ()) 0 () ((i32 const 0) (i32 const 1) drop)))
             (term ((() () ()) 0 () ((i32 const 0)))))

  ;; Tests of br (and unreachable)
  (test-->>E -> ;; jump out
             (term ((() () ()) 0 () ((block (() -> ()) ((block (() -> ()) ((br 0))) unreachable)))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; jump over
             (term ((() () ()) 0 () ((block (() -> ()) ((block (() -> ()) ((br 1))) unreachable)))))
             (term ((() () ()) 0 () ())))
  (test-->>E -> ;; br return values
             (term ((() () ()) 0 () ((block (() -> (i32))
                                            ((block (() -> ())
                                                    ((i32 const 0)
                                                     (i32 const 1)
                                                     (br 1)))
                                             unreachable)))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; br-if true
             (term ((() () ()) 0 () ((block (() -> ()) ((i32 const 1) (br-if 0) unreachable)))))
             (term ((() () ()) 0 () ())))
  (test-->>E -> ;; br-if false
             (term ((() () ()) 0 () ((block (() -> ()) ((i32 const 0) (br-if 0) unreachable)))))
             (term ((() () ()) 0 () (trap))))
  (test-->>E -> ;; br-table
             (term ((() () ()) 0 () ((i32 const 0)
                                     (block ((i32) -> (i32))
                                            ((block ((i32) -> ())
                                                    ((block ((i32) -> ())
                                                            ((br-table 0 1 0 1)))
                                                     (i32 const 0)
                                                     (br 1)))
                                             (i32 const 1))))))
             (term ((() () ()) 0 () ((i32 const 0)))))
  (test-->>E -> ;; br-table
             (term ((() () ()) 0 () ((i32 const 3)
                                     (block ((i32) -> (i32))
                                            ((block ((i32) -> ())
                                                    ((block ((i32) -> ())
                                                            ((br-table 0 1 0 1)))
                                                     (i32 const 0)
                                                     (br 1)))
                                             (i32 const 1))))))
             (term ((() () ()) 0 () ((i32 const 1)))))
  (test-->>E -> ;; br-table out of bounds
             (term ((() () ()) 0 () ((i32 const 12)
                                     (block ((i32) -> (i32))
                                            ((block ((i32) -> ())
                                                    ((block ((i32) -> ())
                                                            ((br-table 0 1 0 1)))
                                                     (i32 const 0)
                                                     (br 1)))
                                             (i32 const 1))))))
             (term ((() () ()) 0 () ((i32 const 1)))))

  ;; Locals
  (test-->>E -> ;; get-local
             (term ((() () ()) 0 ((i32 const 0) (f64 const 0.0)) ((get-local 0) (get-local 1))))
             (term ((() () ()) 0 ((i32 const 0) (f64 const 0.0)) ((i32 const 0) (f64 const 0.0)))))
  (test-->>E -> ;; set-local
             (term ((() () ()) 0 ((i32 const 0) (f64 const 0.0)) ((f64 const 1.0) (set-local 1))))
             (term ((() () ()) 0 ((i32 const 0) (f64 const 1.0)) ())))
  (test-->>E -> ;; tee-local
             (term ((() () ()) 0 ((i32 const 0) (f64 const 0.0)) ((f64 const ,pi) (tee-local 1))))
             (term ((() () ()) 0 ((i32 const 0) (f64 const ,pi)) ((f64 const ,pi)))))

  ;; Globals
  (test-->>E -> ;; get-global
             (term ((((() ((f64 const ,pi)) () ())) () ())
                    0
                    ()
                    ((get-global 0))))
             (term ((((() ((f64 const ,pi)) () ())) () ())
                    0
                    ()
                    ((f64 const ,pi)))))
  (test-->>E -> ;; set-global
             (term ((((() ((i32 const 0)) () ())) () ())
                    0
                    ()
                    ((i32 const 1) (set-global 0))))
             (term ((((() ((i32 const 1)) () ())) () ())
                    0
                    ()
                    ())))

  ;; Tests of function calls
  (test-->>E -> ;; call j, call cl, get-local, return
             (term ((((() () () ())
                      (((0 (func ((i32) -> (i32)) (local () ((get-local 0)))))
                        (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1)))))
                        (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2))))))
                       () () ())
                      (() () () ()))
                     ()
                     ()) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 1) (i32 const 2) (call 1)))) ; e stream
             (term ((((() () () ())
                      (((0 (func ((i32) -> (i32)) (local () ((get-local 0)))))
                        (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1)))))
                        (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2))))))
                       () () ())
                      (() () () ()))
                     ()
                     ()) ; store
                    1 ; inst
                    () ; locals
                    ((i32 const 0) (i32 const 2))))) ; e stream

  (test-->>E -> ;; call cl, trap
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ((local 0 (0 ()) (unreachable))))) ; e stream
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    (trap))))

  (test-->>E -> ;; call cl, br
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ((call (0 (func (() -> ())
                                    (local ()
                                      ((br 0)
                                       unreachable)))))))) ; e stream
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ())))

  (test-->>E -> ;; call cl, return ensure branching with instructions after local
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ((call (0 (func (() -> (i32))
                                    (local ()
                                      ((i32 const 42)
                                       return
                                       unreachable)))))
                     (i32 const 2)
                     (i32 add))))
             (term ((() () ()) ; store
                    0 ; inst
                    () ; locals
                    ((i32 const 44)))))

  (test-->>E -> ;; call_indirect
             (term ((((() () (1) ()))
                    (((0 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return)))))
                     ((3 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (4 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (5 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return))))))
                    ())
                    0
                    ()
                    ((i32 const 2) (i32 const 3) (i32 const 1) (call-indirect ((i32 i32) -> (i32))))))
             (term ((((() () (1) ()))
                    (((0 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return)))))
                     ((3 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (4 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (5 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return))))))
                    ())
                    0
                    ()
                    ((i32 const 3)))))

  (test-->>E -> ;; call_indirect wrong type
             (term ((((() () (1) ()))
                    (((0 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return)))))
                     ((3 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (4 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (5 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return))))))
                    ())
                    0
                    ()
                    ((i64 const 2) (i32 const 1) (call-indirect ((i64) -> (i64))))))
             (term ((((() () (1) ()))
                    (((0 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return)))))
                     ((3 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (4 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (5 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return))))))
                    ())
                    0
                    ()
                    (trap))))

  ;; Helper function for testing stores
  (define (store-integer mem offset width value)
    (integer->integer-bytes value width #f #f mem offset))

  (define (store-floating mem offset width value)
    (real->floating-point-bytes value width #f mem offset))

  ;; Use a smaller page size for testing
  (parameterize ([memory-page-size 64])

    (test-->>E -> ;; store then load
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 0) (i32 const 0) (i64 const 65) (i64 store 0 8) (i64 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 8 8 65)))
                      0
                      ()
                      ((i64 const 65)))))

    (test-->>E -> ;; store i32, load i8 (test of endianness)
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 0)
                       (i32 const #x12345678)
                       (i32 store 0 4)
                       (i32 const 0)
                       (i32 load (i8 unsigned) 0 4))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 4 4 #x12345678)))
                      0
                      ()
                      ((i32 const #x78))))) ; would be #x12 if big-endian

    (test-->>E -> ;; store 255, load signed i8
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 0)
                       (i32 const #xFF)
                       (i32 store 0 4)
                       (i32 const 0)
                       (i32 load (i8 signed) 0 4))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 4 4 #xFF)))
                      0
                      ()
                      ((i32 const #xFFFFFFFF)))))

    (test-->>E -> ;; store -1 as i8, load as i32
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 0)
                       (i32 const #xFFFFFFFF)
                       (i32 store i8 0 4)
                       (i32 const 0)
                       (i32 load 0 4))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 4 1 #xFF)))
                      0
                      ()
                      ((i32 const #xFF)))))

    (test-->>E -> ;; store/load f64
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 0)
                       (f64 const ,pi)
                       (f64 store 0 8)
                       (i32 const 0)
                       (f64 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(store-floating (make-memory 1) 8 8 pi)))
                      0
                      ()
                      ((f64 const ,pi)))))
    
    (test-->>E -> ;; store/load f32
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 0)
                       (f32 const ,(real->single-flonum pi))
                       (f32 store 0 8)
                       (i32 const 0)
                       (f32 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(store-floating (make-memory 1) 8 4 pi)))
                      0
                      ()
                      ((f32 const ,(real->single-flonum pi))))))

    (test-->>E -> ;; store i64, load f64 (reinterpret equivalent)
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 0)
                       (i64 const #x400921FB54442D18)
                       (i64 store 0 8)
                       (i32 const 0)
                       (f64 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(store-floating (make-memory 1) 8 8 pi)))
                      0
                      ()
                      ((f64 const ,pi)))))

    (test-->>E -> ;; store out-of-bounds then load
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 0) (i32 const 0) (i64 const 65) (i64 store 0 200) (i64 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      (trap))))

    (test-->>E -> ;; store then load out-of-bounds
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 0) (i32 const 0) (i64 const 65) (i64 store 0 8) (i64 load 0 200))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 8 8 65)))
                      0
                      ()
                      (trap))))

    (test-->>E -> ;; store i16 out-of-bounds (barely)
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 63)
                       (i32 const 0)
                       (i32 store i16 0 0))))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      (trap))))

    (test-->>E -> ;; load i32 packed out-of-bounds (barely)
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 2)))
                      0
                      ()
                      ((i32 const 126)
                       (i64 load (i32 unsigned) 1 0))))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 2)))
                      0
                      ()
                      (trap))))

    (test-->>E -> ;; current-memory
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      (current-memory)))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 1)))))

    (test-->>E -> ;; grow-memory
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 1) grow-memory)))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 2)))
                      0
                      ()
                      ((i32 const 1)))))

    (test-->>E -> ;; grow-memory failure
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const 2) grow-memory)))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      0
                      ()
                      ((i32 const #xFFFFFFFF)))))
    )

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
                           (unreachable)))))
             (term ((() () ())
                    0
                    ()
                    (trap))))

  (test-->>E -> ;; loop, if
             (term ((((((0 (func ((i32) -> (i32))
                                 (local (i32)
                                   ((loop (() -> (i32))
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
                                               ((get-local 1))))))))))
                       () () ())) ; store
                     () ())
                    0
                    ()
                    ((i32 const 5)
                     (call 0))))
             (term ((((((0 (func ((i32) -> (i32))
                                 (local (i32)
                                   ((loop (() -> (i32))
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
                                               ((get-local 1))))))))))
                       () () ())) ; store
                     () ())
                    0
                    ()
                    ((i32 const 15)))))
  )