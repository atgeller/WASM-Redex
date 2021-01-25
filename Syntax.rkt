#lang racket

(require redex/reduction-semantics)

(require "MachineOps.rkt")

(provide WASM WASMrt)

; needed to read single precision floats
(read-single-flonum #t)

(define-language WASM
  (e ::= (unreachable) (nop) (drop) (select)
     (block tf (e ...)) (loop tf (e ...))
     (if tf (e ...) else (e ...)) (br i) (br-if i)
     (br-table (i ...)) (return) (call i)
     (call-indirect tf) (get-local i) (set-local i)
     (tee-local i) (get-global i) (set-global i)
     (t load a o) (t load (tp sx) a o) (t store a o)
     (t store (tp) a o) (current-memory) (grow-memory)

     (i.t iunop) (f.t funop)
     (i.t ibinop) (f.t fbinop)
     (i.t itestop)
     (i.t irelop) (f.t frelop)
     (t cvtop t) (t cvtop t sx)
     
     (i32 const (side-condition integer_1 ((or/c ui32? si32?) (term integer_1))))
     (i64 const (side-condition integer_1 ((or/c ui64? si64?) (term integer_1))))
     (f32 const (side-condition real_1 (single-flonum? (term real_1))))
     (f64 const (side-condition real_1 (double-flonum? (term real_1)))))

  (i.t ::= i32 i64)
  (f.t ::= f32 f64)
  
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
  (a o ::= (side-condition natural_1 (ui32? (term natural_1))))
  
  ; real is a superset of all constant types
  (c ::= real)

  (f ::= (func (ex ...) tf (local (t ...) (e ...)))
     (func (ex ...) tf im))
  (glob ::= (global (ex ...) tg (e ...))
        (global (ex ...) tg im))
  (tab ::= (table (ex ...) n (i ...))
       (table (ex ...) n im))
  (mem ::= (memory (ex ...) n)
       (memory (ex ...) n im))
  (im ::= (import string string))
  (ex ::= (export string))
  (mod ::= (module (f ...) (glob ...) (tab ...) (mem ...))))

(define-extended-language WASMrt WASM
  (v ::= (t const c))

  (e ::= .... (trap) (call cl) (label n (e ...) (e ...))
     (local n (i (v ...)) (e ...)))
  (L ::= hole (v ... (label n (e ...) L) e ...))

  (s ::= ((inst ...) (tabinst ...) (meminst ...)))
  (cl ::= (i f))
  (inst ::= ((cl ...) (v ...) (table i) (memory i))
        ((cl ...) (v ...) (table i) (memory))
        ((cl ...) (v ...) (table) (memory i))
        ((cl ...) (v ...) (table) (memory)))

  (tabinst ::= (cl ...))
  (meminst ::= (bits any)))