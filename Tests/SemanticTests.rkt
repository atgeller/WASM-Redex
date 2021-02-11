#lang racket

(module+ test
  (require redex/reduction-semantics
           "../Semantics.rkt"
           rackunit)

  ;; Tests of unops
  (test-->>E (-> 0) ;; clz 0
             (term ((() () ()) () ((i32 const 0) (i32 clz))))
             (term ((() () ()) () ((i32 const 32)))))
  (test-->>E (-> 0) ;; clz #xFFFFFFFF
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 clz))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; clz single bit
             (term ((() () ()) () ((i32 const #x00800000) (i32 clz))))
             (term ((() () ()) () ((i32 const 8)))))
  (test-->>E (-> 0) ;; ctz 0
             (term ((() () ()) () ((i32 const 0) (i32 ctz))))
             (term ((() () ()) () ((i32 const 32)))))
  (test-->>E (-> 0) ;; ctz #xFFFFFFFF
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 ctz))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; ctz single bit
             (term ((() () ()) () ((i32 const #x00800000) (i32 ctz))))
             (term ((() () ()) () ((i32 const 23)))))
  (test-->>E (-> 0) ;; popcnt 0
             (term ((() () ()) () ((i32 const 0) (i32 popcnt))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; popcnt #xFFFFFFFF
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 popcnt))))
             (term ((() () ()) () ((i32 const 32)))))
  (test-->>E (-> 0) ;; popcnt #xDEADBEEF
             (term ((() () ()) () ((i32 const #xDEADBEEF) (i32 popcnt))))
             (term ((() () ()) () ((i32 const 24)))))

  (test-->>E (-> 0) ;; abs 3
             (term ((() () ()) () ((f64 const 3.0) (f64 abs))))
             (term ((() () ()) () ((f64 const 3.0)))))
  (test-->>E (-> 0) ;; abs -3
             (term ((() () ()) () ((f64 const -3.0) (f64 abs))))
             (term ((() () ()) () ((f64 const 3.0)))))
  (test-->>E (-> 0) ;; neg 3
             (term ((() () ()) () ((f64 const 3.0) (f64 neg))))
             (term ((() () ()) () ((f64 const -3.0)))))
  (test-->>E (-> 0) ;; neg -3
             (term ((() () ()) () ((f64 const -3.0) (f64 neg))))
             (term ((() () ()) () ((f64 const 3.0)))))
  (test-->>E (-> 0) ;; sqrt
             (term ((() () ()) () ((f64 const 4.0) (f64 sqrt))))
             (term ((() () ()) () ((f64 const 2.0)))))
  (test-->>E (-> 0) ;; sqrt
             (term ((() () ()) () ((f64 const 4.0) (f64 sqrt) (f64 sqrt))))
             (term ((() () ()) () ((f64 const 1.4142135623730951)))))
  (test-->>E (-> 0) ;; sqrt negative
             (term ((() () ()) () ((f64 const -4.0) (f64 sqrt))))
             (term ((() () ()) () ((f64 const +nan.0)))))
  (test-->>E (-> 0) ;; ceil
             (term ((() () ()) () ((f64 const ,pi) (f64 ceil))))
             (term ((() () ()) () ((f64 const 4.0)))))
  (test-->>E (-> 0) ;; floor
             (term ((() () ()) () ((f64 const ,pi) (f64 floor))))
             (term ((() () ()) () ((f64 const 3.0)))))
  (test-->>E (-> 0) ;; nearest
             (term ((() () ()) () ((f64 const ,pi) (f64 nearest))))
             (term ((() () ()) () ((f64 const 3.0)))))
  (test-->>E (-> 0) ;; nearest
             (term ((() () ()) () ((f64 const 1.5) (f64 nearest))))
             (term ((() () ()) () ((f64 const 2.0)))))

  (test-->>E (-> 0) ;; f32 sqrt
             (term ((() () ()) () ((f32 const ,(real->single-flonum 2.0f0)) (f32 sqrt))))
             (term ((() () ()) () ((f32 const ,(real->single-flonum 1.4142135f0))))))
  (test-->>E (-> 0) ;; f32 sqrt negative
             (term ((() () ()) () ((f32 const ,(real->single-flonum -2.0f0)) (f32 sqrt))))
             (term ((() () ()) () ((f32 const ,(real->single-flonum +nan.f))))))

  ;; Tests of simple binops
  (test-->>E (-> 0) ;; add
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 add))))
             (term ((() () ()) () ((i32 const 3)))))
  (test-->>E (-> 0) ;; sub
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 sub))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; mul
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 mul))))
             (term ((() () ()) () ((i32 const 2)))))
  (test-->>E (-> 0) ;; div-s
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 div-s))))
             (term ((() () ()) () ((i32 const 2)))))
  (test-->>E (-> 0) ;; div-s -1
             (term ((() () ()) () ((i32 const 2) (i32 const #xFFFFFFFF) (i32 div-s))))
             (term ((() () ()) () ((i32 const #xFFFFFFFE)))))
  (test-->>E (-> 0) ;; div-s 0
             (term ((() () ()) () ((i32 const 1) (i32 const 0) (i32 div-s))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; div-u
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 div-u))))
             (term ((() () ()) () ((i32 const 2)))))
  (test-->>E (-> 0) ;; div-u -1
             (term ((() () ()) () ((i32 const 2) (i32 const #xFFFFFFFF) (i32 div-u))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; div-u 0
             (term ((() () ()) () ((i32 const 1) (i32 const 0) (i32 div-u))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; rem-s
             (term ((() () ()) () ((i32 const 3) (i32 const 2) (i32 rem-s))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; rem-s 3 -2
             (term ((() () ()) () ((i32 const 1) (i32 const #xFFFFFFFE) (i32 rem-s))))
             (term ((() () ()) () ((i32 const #xFFFFFFFF)))))
  (test-->>E (-> 0) ;; rem-s 0
             (term ((() () ()) () ((i32 const 1) (i32 const 0) (i32 rem-s))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; rem-u
             (term ((() () ()) () ((i32 const 3) (i32 const 2) (i32 rem-u))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; rem-u 1 -2
             (term ((() () ()) () ((i32 const 3) (i32 const #xFFFFFFFE) (i32 rem-u))))
             (term ((() () ()) () ((i32 const 3)))))
  (test-->>E (-> 0) ;; rem-u 0
             (term ((() () ()) () ((i32 const 1) (i32 const 0) (i32 rem-u))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; and
             (term ((() () ()) () ((i32 const 5) (i32 const 3) (i32 and))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; or
             (term ((() () ()) () ((i32 const 5) (i32 const 3) (i32 or))))
             (term ((() () ()) () ((i32 const 7)))))
  (test-->>E (-> 0) ;; xor
             (term ((() () ()) () ((i32 const 5) (i32 const 3) (i32 xor))))
             (term ((() () ()) () ((i32 const 6)))))
  (test-->>E (-> 0) ;; shl
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 shl))))
             (term ((() () ()) () ((i32 const 4)))))
  (test-->>E (-> 0) ;; shl overflow
             (term ((() () ()) () ((i32 const 2) (i32 const 31) (i32 shl))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; shr-s
             (term ((() () ()) () ((i32 const 4) (i32 const 1) (i32 shr-s))))
             (term ((() () ()) () ((i32 const 2)))))
  (test-->>E (-> 0) ;; shr-s underflow
             (term ((() () ()) () ((i32 const 4) (i32 const 3) (i32 shr-s))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; shr-s negative
             (term ((() () ()) () ((i32 const #x80000000) (i32 const 31) (i32 shr-s))))
             (term ((() () ()) () ((i32 const #xFFFFFFFF)))))
  (test-->>E (-> 0) ;; shr-u
             (term ((() () ()) () ((i32 const 4) (i32 const 1) (i32 shr-u))))
             (term ((() () ()) () ((i32 const 2)))))
  (test-->>E (-> 0) ;; shr-u underflow
             (term ((() () ()) () ((i32 const 4) (i32 const 3) (i32 shr-u))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; shr-u negative
             (term ((() () ()) () ((i32 const #x80000000) (i32 const 31) (i32 shr-u))))
             (term ((() () ()) () ((i32 const #x00000001)))))
  (test-->>E (-> 0) ;; rotl
             (term ((() () ()) () ((i32 const #xFF0000FF) (i32 const 8) (i32 rotl))))
             (term ((() () ()) () ((i32 const #x0000FFFF)))))
  (test-->>E (-> 0) ;; rotl far
             (term ((() () ()) () ((i32 const #xFF0000FF) (i32 const 40) (i32 rotl))))
             (term ((() () ()) () ((i32 const #x0000FFFF)))))
  (test-->>E (-> 0) ;; rotr
             (term ((() () ()) () ((i32 const #xFF0000FF) (i32 const 8) (i32 rotr))))
             (term ((() () ()) () ((i32 const #xFFFF0000)))))
  (test-->>E (-> 0) ;; rotr far
             (term ((() () ()) () ((i32 const #xFF0000FF) (i32 const 40) (i32 rotr))))
             (term ((() () ()) () ((i32 const #xFFFF0000)))))
  
  (test-->>E (-> 0) ;; i64 math
             (term ((() () ()) () ((i64 const 4294967295) (i64 const 2) (i64 mul))))
             (term ((() () ()) () ((i64 const 8589934590)))))

  (test-->>E (-> 0) ;; f64 add
             (term ((() () ()) () ((f64 const 0.1) (f64 const 0.2) (f64 add))))
             (term ((() () ()) () ((f64 const 0.30000000000000004)))))
  (test-->>E (-> 0) ;; f64 sub
             (term ((() () ()) () ((f64 const 0.3) (f64 const 0.2) (f64 sub))))
             (term ((() () ()) () ((f64 const 0.09999999999999998)))))
  (test-->>E (-> 0) ;; f64 mul
             (term ((() () ()) () ((f64 const 42.0) (f64 const 0.1) (f64 mul))))
             (term ((() () ()) () ((f64 const 4.2)))))
  (test-->>E (-> 0) ;; f64 mul
             (term ((() () ()) () ((f64 const 42.0) (f64 const 0.0) (f64 mul))))
             (term ((() () ()) () ((f64 const 0.0)))))
  (test-->>E (-> 0) ;; f64 mul
             (term ((() () ()) () ((f64 const -42.0) (f64 const 0.0) (f64 mul))))
             (term ((() () ()) () ((f64 const -0.0)))))
  (test-->>E (-> 0) ;; f64 div
             (term ((() () ()) () ((f64 const 5.0) (f64 const 2.5) (f64 div))))
             (term ((() () ()) () ((f64 const 2.0)))))
  (test-->>E (-> 0) ;; f64 div
             (term ((() () ()) () ((f64 const 1.0) (f64 const 0.0) (f64 div))))
             (term ((() () ()) () ((f64 const +inf.0)))))
  (test-->>E (-> 0) ;; f64 div
             (term ((() () ()) () ((f64 const -1.0) (f64 const 0.0) (f64 div))))
             (term ((() () ()) () ((f64 const -inf.0)))))
  (test-->>E (-> 0) ;; f64 min
             (term ((() () ()) () ((f64 const 1.0) (f64 const 0.1) (f64 min))))
             (term ((() () ()) () ((f64 const 0.1)))))
  (test-->>E (-> 0) ;; f64 min
             (term ((() () ()) () ((f64 const +inf.0) (f64 const -inf.0) (f64 min))))
             (term ((() () ()) () ((f64 const -inf.0)))))
  (test-->>E (-> 0) ;; f64 max
             (term ((() () ()) () ((f64 const 1.0) (f64 const 0.1) (f64 max))))
             (term ((() () ()) () ((f64 const 1.0)))))
  (test-->>E (-> 0) ;; f64 max
             (term ((() () ()) () ((f64 const +inf.0) (f64 const -inf.0) (f64 max))))
             (term ((() () ()) () ((f64 const +inf.0)))))
  (test-->>E (-> 0) ;; f64 copysign
             (term ((() () ()) () ((f64 const 2.0) (f64 const -3.0) (f64 copysign))))
             (term ((() () ()) () ((f64 const -2.0)))))
  (test-->>E (-> 0) ;; f64 copysign
             (term ((() () ()) () ((f64 const 2.0) (f64 const -0.0) (f64 copysign))))
             (term ((() () ()) () ((f64 const -2.0)))))
  (test-->>E (-> 0) ;; f64 copysign
             (term ((() () ()) () ((f64 const -3.0) (f64 const 2.0) (f64 copysign))))
             (term ((() () ()) () ((f64 const 3.0)))))

  ;; Cursed becuase Racket has an understandable hatred for reading single precision floats
  (test-->>E (-> 0) ;; f32 add
             (term ((() () ()) () ((f32 const ,(real->single-flonum 0.2f0)) (f32 const ,(real->single-flonum 0.1f0)) (f32 add))))
             (term ((() () ()) () ((f32 const ,(real->single-flonum 0.3f0))))))

  (test-->>E (-> 0) ;; eqz false
             (term ((() () ()) () ((i32 const 8) (i32 eqz))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; eqz true
             (term ((() () ()) () ((i32 const 0) (i32 eqz))))
             (term ((() () ()) () ((i32 const 1)))))
  
  (test-->>E (-> 0) ;; eq true
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 eq))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; eq false
             (term ((() () ()) () ((i32 const 42) (i32 const 43) (i32 eq))))
             (term ((() () ()) () ((i32 const 0)))))

  (test-->>E (-> 0) ;; eq float true
             (term ((() () ()) () ((f64 const 42.0) (f64 const 42.0) (f64 eq))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; eq float true
             (term ((() () ()) () ((f64 const 0.0) (f64 const -0.0) (f64 eq))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; eq float false
             (term ((() () ()) () ((f64 const 0.0) (f64 const 42.0) (f64 eq))))
             (term ((() () ()) () ((i32 const 0)))))
  
  (test-->>E (-> 0) ;; ne false
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 ne))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; ne true
             (term ((() () ()) () ((i32 const 42) (i32 const 43) (i32 ne))))
             (term ((() () ()) () ((i32 const 1)))))
  
  (test-->>E (-> 0) ;; ne float false
             (term ((() () ()) () ((f64 const 42.0) (f64 const 42.0) (f64 ne))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; ne float false
             (term ((() () ()) () ((f64 const 0.0) (f64 const -0.0) (f64 ne))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; ne float true
             (term ((() () ()) () ((f64 const 0.0) (f64 const 42.0) (f64 ne))))
             (term ((() () ()) () ((i32 const 1)))))
  
  (test-->>E (-> 0) ;; lt-u
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 lt-u))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; lt-u
             (term ((() () ()) () ((i32 const 2) (i32 const 43) (i32 lt-u))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; lt-u
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 lt-u))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; lt-u signed
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 lt-u))))
             (term ((() () ()) () ((i32 const 0)))))
  
  (test-->>E (-> 0) ;; lt-s
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 lt-s))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; lt-s
             (term ((() () ()) () ((i32 const 2) (i32 const 3) (i32 lt-s))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; lt-s
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 lt-s))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; lt-s signed
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 lt-s))))
             (term ((() () ()) () ((i32 const 1)))))
    
  (test-->>E (-> 0) ;; gt-u
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 gt-u))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; gt-u
             (term ((() () ()) () ((i32 const 2) (i32 const 3) (i32 gt-u))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; gt-u
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 gt-u))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; gt-u signed
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 gt-u))))
             (term ((() () ()) () ((i32 const 1)))))
  
  (test-->>E (-> 0) ;; gt-s
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 gt-s))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; gt-s
             (term ((() () ()) () ((i32 const 2) (i32 const 3) (i32 gt-s))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; gt-s
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 gt-s))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; gt-s signed
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 gt-s))))
             (term ((() () ()) () ((i32 const 0)))))
  
  (test-->>E (-> 0) ;; le-u
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 le-u))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; le-u
             (term ((() () ()) () ((i32 const 2) (i32 const 3) (i32 le-u))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; le-u
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 le-u))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; le-u signed
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 le-u))))
             (term ((() () ()) () ((i32 const 0)))))
  
  (test-->>E (-> 0) ;; le-s
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 le-s))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; le-s
             (term ((() () ()) () ((i32 const 2) (i32 const 3) (i32 le-s))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; le-s
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 le-s))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; le-s signed
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 le-s))))
             (term ((() () ()) () ((i32 const 1)))))
  
  (test-->>E (-> 0) ;; ge-u
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 ge-u))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; ge-u
             (term ((() () ()) () ((i32 const 2) (i32 const 3) (i32 ge-u))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; ge-u
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 ge-u))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; ge-u signed
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 ge-u))))
             (term ((() () ()) () ((i32 const 1)))))
  
  (test-->>E (-> 0) ;; ge-s
             (term ((() () ()) () ((i32 const 42) (i32 const 42) (i32 ge-s))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; ge-s
             (term ((() () ()) () ((i32 const 2) (i32 const 3) (i32 ge-s))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; ge-s
             (term ((() () ()) () ((i32 const 2) (i32 const 1) (i32 ge-s))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; ge-s signed
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i32 const 0) (i32 ge-s))))
             (term ((() () ()) () ((i32 const 0)))))
  
  (test-->>E (-> 0) ;; lt
             (term ((() () ()) () ((f64 const 1.0) (f64 const 1.0) (f64 lt))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; lt
             (term ((() () ()) () ((f64 const 1.0) (f64 const -1.0) (f64 lt))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; lt
             (term ((() () ()) () ((f64 const -1.0) (f64 const 1.0) (f64 lt))))
             (term ((() () ()) () ((i32 const 1)))))
  
  (test-->>E (-> 0) ;; gt
             (term ((() () ()) () ((f64 const 1.0) (f64 const 1.0) (f64 gt))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; gt
             (term ((() () ()) () ((f64 const 1.0) (f64 const -1.0) (f64 gt))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; gt
             (term ((() () ()) () ((f64 const -1.0) (f64 const 1.0) (f64 gt))))
             (term ((() () ()) () ((i32 const 0)))))
  
  (test-->>E (-> 0) ;; le
             (term ((() () ()) () ((f64 const 1.0) (f64 const 1.0) (f64 le))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; le
             (term ((() () ()) () ((f64 const 1.0) (f64 const -1.0) (f64 le))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; le
             (term ((() () ()) () ((f64 const -1.0) (f64 const 1.0) (f64 le))))
             (term ((() () ()) () ((i32 const 1)))))
  
  (test-->>E (-> 0) ;; ge
             (term ((() () ()) () ((f64 const 1.0) (f64 const 1.0) (f64 ge))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; ge
             (term ((() () ()) () ((f64 const 1.0) (f64 const -1.0) (f64 ge))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; ge
             (term ((() () ()) () ((f64 const -1.0) (f64 const 1.0) (f64 ge))))
             (term ((() () ()) () ((i32 const 0)))))

  ;; cvtop
  (test-->>E (-> 0) ;; i64 1 -> i32 1
             (term ((() () ()) () ((i64 const 1) (i32 convert i64))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; i64 -1 -> i32 -1
             (term ((() () ()) () ((i64 const #xFFFFFFFFFFFFFFFF) (i32 convert i64))))
             (term ((() () ()) () ((i32 const #xFFFFFFFF)))))
  (test-->>E (-> 0) ;; i32 1 -> i64 1
             (term ((() () ()) () ((i32 const 1) (i64 convert i32 signed))))
             (term ((() () ()) () ((i64 const 1)))))
  (test-->>E (-> 0) ;; i32 -1 -> i64 -1
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i64 convert i32 signed))))
             (term ((() () ()) () ((i64 const #xFFFFFFFFFFFFFFFF)))))
  (test-->>E (-> 0) ;; i32 1 -> i64 1 unsigned
             (term ((() () ()) () ((i32 const 1) (i64 convert i32 unsigned))))
             (term ((() () ()) () ((i64 const 1)))))
  (test-->>E (-> 0) ;; i32 -1 -> i64 -1 unsigned
             (term ((() () ()) () ((i32 const #xFFFFFFFF) (i64 convert i32 unsigned))))
             (term ((() () ()) () ((i64 const #xFFFFFFFF)))))
  
  (test-->>E (-> 0) ;; f32 -> f64
             (term ((() () ()) () ((f32 const ,(real->single-flonum 2.3f0)) (f64 convert f32))))
             (term ((() () ()) () ((f64 const 2.299999952316284)))))
  (test-->>E (-> 0) ;; f64 -> f32
             (term ((() () ()) () ((f64 const 2.3) (f32 convert f64))))
             (term ((() () ()) () ((f32 const ,(real->single-flonum 2.3f0))))))
  (test-->>E (-> 0) ;; f64 -> f32 inf
             (term ((() () ()) () ((f64 const 2.3e+40) (f32 convert f64))))
             (term ((() () ()) () ((f32 const ,(real->single-flonum +inf.f))))))
  
  (test-->>E (-> 0) ;; f64 -> unsigned i64
             (term ((() () ()) () ((f64 const ,pi) (i64 convert f64 unsigned))))
             (term ((() () ()) () ((i64 const 3)))))
  (test-->>E (-> 0) ;; f64 -> unsigned i64 inf
             (term ((() () ()) () ((f64 const +inf.0) (i64 convert f64 unsigned))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; f64 -> unsigned i64 nan
             (term ((() () ()) () ((f64 const +nan.0) (i64 convert f64 unsigned))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; f64 -> unsigned i32 overflow
             (term ((() () ()) () ((f64 const ,(expt 2.0 32)) (i32 convert f64 unsigned))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; f64 -> unsigned i32 underflow
             (term ((() () ()) () ((f64 const -1.0) (i32 convert f64 unsigned))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; f64 -> signed i64
             (term ((() () ()) () ((f64 const ,pi) (i64 convert f64 signed))))
             (term ((() () ()) () ((i64 const 3)))))
  (test-->>E (-> 0) ;; f64 -> signed i64 inf
             (term ((() () ()) () ((f64 const +inf.0) (i64 convert f64 signed))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; f64 -> signed i64 nan
             (term ((() () ()) () ((f64 const +nan.0) (i64 convert f64 signed))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; f64 -> signed i32 overflow
             (term ((() () ()) () ((f64 const ,(expt 2.0 32)) (i32 convert f64 signed))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; f64 -> signed i32 negative
             (term ((() () ()) () ((f64 const -1.0) (i32 convert f64 signed))))
             (term ((() () ()) () ((i32 const #xFFFFFFFF)))))
  (test-->>E (-> 0) ;; f64 -> signed i32 underflow
             (term ((() () ()) () ((f64 const ,(sub1 (- (expt 2.0 31)))) (i32 convert f64 signed))))
             (term ((() () ()) () (trap))))

  (test-->>E (-> 0) ;; unsigned i64 -> f64
             (term ((() () ()) () ((i64 const #xFFFFFFFFFFFFFFFF) (f64 convert i64 unsigned))))
             (term ((() () ()) () ((f64 const 1.8446744073709552e+19)))))
  (test-->>E (-> 0) ;; signed i64 -> f64
             (term ((() () ()) () ((i64 const #xFFFFFFFFFFFFFFFF) (f64 convert i64 signed))))
             (term ((() () ()) () ((f64 const -1.0)))))
  (test-->>E (-> 0) ;; unsigned i64 -> f32
             (term ((() () ()) () ((i64 const #xFFFFFFFFFFFFFFFF) (f32 convert i64 unsigned))))
             (term ((() () ()) () ((f32 const ,(real->single-flonum 1.8446744f+19))))))
  (test-->>E (-> 0) ;; signed i64 -> f32
             (term ((() () ()) () ((i64 const #xFFFFFFFFFFFFFFFF) (f32 convert i64 signed))))
             (term ((() () ()) () ((f32 const ,(real->single-flonum -1.0f0))))))
  
  (test-->>E (-> 0) ;; reinterpret i32 -> f32 0
             (term ((() () ()) () ((i32 const 0) (f32 reinterpret i32))))
             (term ((() () ()) () ((f32 const ,(real->single-flonum 0.0f0))))))
  (test-->>E (-> 0) ;; reinterpret i32 -> f32 inf
             (term ((() () ()) () ((i32 const #x7F800000) (f32 reinterpret i32))))
             (term ((() () ()) () ((f32 const ,(real->single-flonum +inf.f))))))
  (test-->>E (-> 0) ;; reinterpret i64 -> f64 42.0
             (term ((() () ()) () ((i64 const #x4045000000000000) (f64 reinterpret i64))))
             (term ((() () ()) () ((f64 const 42.0)))))
  (test-->>E (-> 0) ;; reinterpret f32 -> i32 -0.0
             (term ((() () ()) () ((f32 const ,(real->single-flonum -0.0f0)) (i32 reinterpret f32))))
             (term ((() () ()) () ((i32 const #x80000000)))))
  (test-->>E (-> 0) ;; reinterpret f64 -> i64 -1.0
             (term ((() () ()) () ((f64 const -1.0) (i64 reinterpret f64))))
             (term ((() () ()) () ((i64 const #xBFF0000000000000)))))

  (test-->>E (-> 0) ;; select false
             (term ((() () ()) () ((i64 const 0) (i64 const 1) (i32 const 0) select)))
             (term ((() () ()) () ((i64 const 1)))))
  (test-->>E (-> 0) ;; select true
             (term ((() () ()) () ((i64 const 0) (i64 const 1) (i32 const 1) select)))
             (term ((() () ()) () ((i64 const 0)))))
  (test-->>E (-> 0) ;; select false
             (term ((() () ()) () ((f64 const 0.0) (f64 const 1.0) (i32 const 0) select)))
             (term ((() () ()) () ((f64 const 1.0)))))
  (test-->>E (-> 0) ;; select true
             (term ((() () ()) () ((f64 const 0.0) (f64 const 1.0) (i32 const 1234) select)))
             (term ((() () ()) () ((f64 const 0.0)))))

  ;; nop, drop
  (test-->>E (-> 0) ;; nop
             (term ((() () ()) () ((i32 const 0) nop)))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; drop
             (term ((() () ()) () ((i32 const 0) (i32 const 1) drop)))
             (term ((() () ()) () ((i32 const 0)))))

  ;; Tests of br (and unreachable)
  (test-->>E (-> 0) ;; jump out
             (term ((() () ()) () ((block (() -> ()) ((block (() -> ()) ((br 0))) unreachable)))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; jump over
             (term ((() () ()) () ((block (() -> ()) ((block (() -> ()) ((br 1))) unreachable)))))
             (term ((() () ()) () ())))
  (test-->>E (-> 0) ;; br return values
             (term ((() () ()) () ((block (() -> (i32))
                                            ((block (() -> ())
                                                    ((i32 const 0)
                                                     (i32 const 1)
                                                     (br 1)))
                                             unreachable)))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; br-if true
             (term ((() () ()) () ((block (() -> ()) ((i32 const 1) (br-if 0) unreachable)))))
             (term ((() () ()) () ())))
  (test-->>E (-> 0) ;; br-if false
             (term ((() () ()) () ((block (() -> ()) ((i32 const 0) (br-if 0) unreachable)))))
             (term ((() () ()) () (trap))))
  (test-->>E (-> 0) ;; br-table
             (term ((() () ()) () ((i32 const 0)
                                     (block ((i32) -> (i32))
                                            ((block ((i32) -> ())
                                                    ((block ((i32) -> ())
                                                            ((br-table 0 1 0 1)))
                                                     (i32 const 0)
                                                     (br 1)))
                                             (i32 const 1))))))
             (term ((() () ()) () ((i32 const 0)))))
  (test-->>E (-> 0) ;; br-table
             (term ((() () ()) () ((i32 const 3)
                                     (block ((i32) -> (i32))
                                            ((block ((i32) -> ())
                                                    ((block ((i32) -> ())
                                                            ((br-table 0 1 0 1)))
                                                     (i32 const 0)
                                                     (br 1)))
                                             (i32 const 1))))))
             (term ((() () ()) () ((i32 const 1)))))
  (test-->>E (-> 0) ;; br-table out of bounds
             (term ((() () ()) () ((i32 const 12)
                                     (block ((i32) -> (i32))
                                            ((block ((i32) -> ())
                                                    ((block ((i32) -> ())
                                                            ((br-table 0 1 0 1)))
                                                     (i32 const 0)
                                                     (br 1)))
                                             (i32 const 1))))))
             (term ((() () ()) () ((i32 const 1)))))

  ;; Locals
  (test-->>E (-> 0) ;; get-local
             (term ((() () ()) ((i32 const 0) (f64 const 0.0)) ((get-local 0) (get-local 1))))
             (term ((() () ()) ((i32 const 0) (f64 const 0.0)) ((i32 const 0) (f64 const 0.0)))))
  (test-->>E (-> 0) ;; set-local
             (term ((() () ()) ((i32 const 0) (f64 const 0.0)) ((f64 const 1.0) (set-local 1))))
             (term ((() () ()) ((i32 const 0) (f64 const 1.0)) ())))
  (test-->>E (-> 0) ;; tee-local
             (term ((() () ()) ((i32 const 0) (f64 const 0.0)) ((f64 const ,pi) (tee-local 1))))
             (term ((() () ()) ((i32 const 0) (f64 const ,pi)) ((f64 const ,pi)))))

  ;; Globals
  (test-->>E (-> 0) ;; get-global
             (term ((((() ((f64 const ,pi)) () ())) () ())
                    ()
                    ((get-global 0))))
             (term ((((() ((f64 const ,pi)) () ())) () ())
                    ()
                    ((f64 const ,pi)))))
  (test-->>E (-> 0) ;; set-global
             (term ((((() ((i32 const 0)) () ())) () ())
                    ()
                    ((i32 const 1) (set-global 0))))
             (term ((((() ((i32 const 1)) () ())) () ())
                    ()
                    ())))

  ;; Tests of function calls
  (test-->>E (-> 1) ;; call j, call cl, get-local, return
             (term ((((() () () ())
                      (((0 (func ((i32) -> (i32)) (local () ((get-local 0)))))
                        (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1)))))
                        (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2))))))
                       () () ())
                      (() () () ()))
                     ()
                     ()) ; store
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
                    () ; locals
                    ((i32 const 0) (i32 const 2))))) ; e stream

  (test-->>E (-> 0) ;; call cl, trap
             (term ((() () ()) ; store
                    () ; locals
                    ((local 0 (0 ()) (unreachable))))) ; e stream
             (term ((() () ()) ; store
                    () ; locals
                    (trap))))

  (test-->>E (-> 0) ;; call cl, br
             (term ((() () ()) ; store
                    () ; locals
                    ((call (0 (func (() -> ())
                                    (local ()
                                      ((br 0)
                                       unreachable)))))))) ; e stream
             (term ((() () ()) ; store
                    () ; locals
                    ())))

  (test-->>E (-> 0) ;; call cl, return ensure branching with instructions after local
             (term ((() () ()) ; store
                    () ; locals
                    ((call (0 (func (() -> (i32))
                                    (local ()
                                      ((i32 const 42)
                                       return
                                       unreachable)))))
                     (i32 const 2)
                     (i32 add))))
             (term ((() () ()) ; store
                    () ; locals
                    ((i32 const 44)))))

  (test-->>E (-> 0) ;; call_indirect
             (term ((((() () (1) ()))
                    (((0 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return)))))
                     ((3 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (4 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (5 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return))))))
                    ())
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
                    ()
                    ((i32 const 3)))))

  (test-->>E (-> 0) ;; call_indirect wrong type
             (term ((((() () (1) ()))
                    (((0 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return)))))
                     ((3 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (4 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (5 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return))))))
                    ())
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
                    ()
                    (trap))))

  (test-->>E (-> 0) ;; call_indirect out of bounds
             (term ((((() () (1) ()))
                    (((0 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return)))))
                     ((3 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (4 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (5 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return))))))
                    ())
                    ()
                    ((i64 const 2) (i32 const 71) (call-indirect ((i64) -> (i64))))))
             (term ((((() () (1) ()))
                    (((0 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (1 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (2 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return)))))
                     ((3 (func ((i32) -> (i32)) (local () ((get-local 0) return))))
                      (4 (func ((i32 i32) -> (i32)) (local () ((get-local 1) return))))
                      (5 (func ((i32 i32 i32) -> (i32)) (local () ((get-local 2) return))))))
                    ())
                    ()
                    (trap))))

  ;; Helper function for testing stores
(define (make-memory size)
  (make-bytes (* (memory-page-size) size) 0))
  
  (define (store-integer mem offset width value)
    (integer->integer-bytes value width #f #f mem offset))

  (define (store-floating mem offset width value)
    (real->floating-point-bytes value width #f mem offset))

  ;; Use a smaller page size for testing
  (parameterize ([memory-page-size 64])

    (test-->>E (-> 0) ;; store then load
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 0) (i32 const 0) (i64 const 65) (i64 store 0 8) (i64 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 8 8 65)))
                      ()
                      ((i64 const 65)))))

    (test-->>E (-> 0) ;; store i32, load i8 (test of endianness)
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 0)
                       (i32 const #x12345678)
                       (i32 store 0 4)
                       (i32 const 0)
                       (i32 load (i8 unsigned) 0 4))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 4 4 #x12345678)))
                      ()
                      ((i32 const #x78))))) ; would be #x12 if big-endian

    (test-->>E (-> 0) ;; store 255, load signed i8
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 0)
                       (i32 const #xFF)
                       (i32 store 0 4)
                       (i32 const 0)
                       (i32 load (i8 signed) 0 4))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 4 4 #xFF)))
                      ()
                      ((i32 const #xFFFFFFFF)))))

    (test-->>E (-> 0) ;; store -1 as i8, load as i32
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 0)
                       (i32 const #xFFFFFFFF)
                       (i32 store i8 0 4)
                       (i32 const 0)
                       (i32 load 0 4))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 4 1 #xFF)))
                      ()
                      ((i32 const #xFF)))))

    (test-->>E (-> 0) ;; store/load f64
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 0)
                       (f64 const ,pi)
                       (f64 store 0 8)
                       (i32 const 0)
                       (f64 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(store-floating (make-memory 1) 8 8 pi)))
                      ()
                      ((f64 const ,pi)))))
    
    (test-->>E (-> 0) ;; store/load f32
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 0)
                       (f32 const ,(real->single-flonum pi))
                       (f32 store 0 8)
                       (i32 const 0)
                       (f32 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(store-floating (make-memory 1) 8 4 pi)))
                      ()
                      ((f32 const ,(real->single-flonum pi))))))

    (test-->>E (-> 0) ;; store i64, load f64 (reinterpret equivalent)
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 0)
                       (i64 const #x400921FB54442D18)
                       (i64 store 0 8)
                       (i32 const 0)
                       (f64 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(store-floating (make-memory 1) 8 8 pi)))
                      ()
                      ((f64 const ,pi)))))

    (test-->>E (-> 0) ;; store out-of-bounds then load
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 0) (i32 const 0) (i64 const 65) (i64 store 0 200) (i64 load 0 8))))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      (trap))))

    (test-->>E (-> 0) ;; store then load out-of-bounds
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 0) (i32 const 0) (i64 const 65) (i64 store 0 8) (i64 load 0 200))))
               (term ((((() () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 8 8 65)))
                      ()
                      (trap))))

    (test-->>E (-> 0) ;; store i16 out-of-bounds (barely)
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 63)
                       (i32 const 0)
                       (i32 store i16 0 0))))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      (trap))))

    (test-->>E (-> 0) ;; load i32 packed out-of-bounds (barely)
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 2)))
                      ()
                      ((i32 const 126)
                       (i64 load (i32 unsigned) 1 0))))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 2)))
                      ()
                      (trap))))

    (test-->>E (-> 0) ;; current-memory
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      (current-memory)))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 1)))))

    (test-->>E (-> 0) ;; grow-memory
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 1) grow-memory)))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 2)))
                      ()
                      ((i32 const 1)))))

    (test-->>E (-> 0) ;; grow-memory failure
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const 2) grow-memory)))
               (term ((((() () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((i32 const #xFFFFFFFF)))))
    )

  (test-->>E (-> 0) ;; if-true
             (term ((() () ())
                    ()
                    ((i32 const 1)
                     (if (() -> (i32))
                         ((i32 const 2))
                         else
                         ((i32 const 3))))))
             (term ((() () ())
                    ()
                    ((i32 const 2)))))

  (test-->>E (-> 0) ;; if-false
             (term ((() () ())
                    ()
                    ((i32 const 0)
                     (if (() -> (i32))
                         ((i32 const 2))
                         else
                         ((i32 const 3))))))
             (term ((() () ())
                    ()
                    ((i32 const 3)))))

  (test-->>E (-> 0) ;; loop with trap (trap inside label with instructions)
             (term ((() () ())
                    ()
                    ((loop (() -> ())
                           (unreachable)))))
             (term ((() () ())
                    ()
                    (trap))))

  (test-->>E (-> 0) ;; loop, if
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
                       () () ()))
                     () ()) ; store
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
                       () () ()))
                     () ()) ; store
                    ()
                    ((i32 const 15)))))

  ;; test of non-deterministic reduction in locals
  (parameterize ([memory-page-size 8])
    (let ([insts (term ((([0 (func ((i32) -> ())
                                   (local ()
                                     ((get-local 0)
                                      (call 1)
                                      (get-local 0)
                                      grow-memory
                                      drop)))]
                          [0 (func ((i32) -> ())
                                   (local ()
                                     ((get-local 0)
                                      grow-memory
                                      drop)))])
                         ()
                         ()
                         (0))))])
    
      (test-->>E (-> 0)
                 (term ((,insts () (,(make-bytes 8 0)))
                        ()
                        ((i32 const 1)
                         (call 0))))
                 (term ((,insts () (,(make-bytes 8 0)))
                        ()
                        ())))
  
      (test-->>E (-> 0)
                 (term ((,insts
                         ()
                         (,(make-bytes 8 0)))
                        ()
                        ((i32 const 1)
                         (call 0))))
                 (term ((,insts () (,(make-bytes 16 0)))
                        ()
                        ())))
  
      (test-->>E (-> 0)
                 (term ((,insts
                         ()
                         (,(make-bytes 8 0)))
                        ()
                        ((i32 const 1)
                         (call 0))))
                 (term ((,insts () (,(make-bytes 24 0)))
                        ()
                        ())))
      ))
  )
