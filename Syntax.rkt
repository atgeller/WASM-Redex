#lang racket

(require racket/flonum
         redex/reduction-semantics)

(provide WASM)

(define (flsingle-flonum? c)
  (and (flonum? c)
       (equal? c (flsingle c))))

(define-language WASM
  (e ::= unreachable nop drop select
     (block tf (e ...)) (loop tf (e ...)) (if tf (e ...) else (e ...))
     (br i) (br-if i) (br-table i i ...)
     return (call i) (call-indirect tf)
     (get-local i) (set-local i) (tee-local i)
     (get-global i) (set-global i)
     (t load a o) (t load (tp sx) a o) (t store a o) (t store tp a o)
     current-memory grow-memory

     (inn iunop) (fnn funop)
     (inn ibinop) (fnn fbinop)
     (inn itestop)
     (inn irelop) (fnn frelop)
     (t cvtop t) (t cvtop t sx)

     (i32 const (side-condition integer_1 (<= 0 (term integer_1) (sub1 (expt 2 32)))))
     (i64 const (side-condition integer_1 (<= 0 (term integer_1) (sub1 (expt 2 64)))))
     (f32 const (side-condition real_1 (flsingle-flonum? (term real_1))))
     (f64 const (side-condition real_1 (flonum? (term real_1)))))

  (inn ::= i32 i64)
  (fnn ::= f32 f64)

  (t ::= i32 i64 f32 f64)
  (tp ::= i8 i16 i32)
  (tf ::= ((t ...) -> (t ...)))
  (mut ::= const var)
  (tg ::= (mut t))
  (sx ::= signed unsigned)

  (unop ::= iunop funop)
  (binop ::= ibinop fbinop)
  (testop ::= itestop)
  (relop ::= irelop frelop)

  (iunop ::= clz ctz popcnt)
  (ibinop ::= add sub mul div-s div-u rem-s rem-u
         and or xor shl shr-s shr-u rotl rotr)
  (itestop ::= eqz)
  (irelop ::= eq ne lt-s lt-u gt-s gt-u le-s le-u ge-s ge-u)

  (funop ::= abs neg sqrt ceil floor nearest)
  (fbinop ::= add sub mul div min max copysign)
  (frelop ::= eq ne lt gt le ge)

  (cvtop ::= convert reinterpret)

  (i j n m ::= natural)
  (a o ::= (side-condition natural_1 (<= 0 (term natural_1) (sub1 (expt 2 32)))))

  ; real is a superset of all constant types
  (c k ::= real)

  (f ::= ((ex ...) (func tf (local (t ...) (e ...))))
     ((ex ...) (func tf im)))
  (glob ::= ((ex ...) (global tg (e ...)))
        ((ex ...) (global tg im)))
  (tab ::= ((ex ...) (table n i ...))
       ((ex ...) (table n im)))
  (mem ::= ((ex ...) (memory n))
       ((ex ...) (memory n im)))
  (im ::= (import string string))
  (ex ::= (export string))
  (mod ::= (module (f ...) (glob ...) (tab ...) (mem ...))))
