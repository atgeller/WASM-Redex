#lang racket

(require redex/reduction-semantics)

(provide WASM WASMrt)

(define-language WASM
  (e ::= (unreachable) (nop) (drop) (select)
     (block tf (e ...)) (loop tf (e ...))
     (if tf (e ...) else (e ...)) (br j) (br-if j)
     (br-table (j ...)) (return) (call j)
     (call-indirect tf) (get-local j) (set-local j)
     (tee-local i) (get-global i) (set-global i)
     (t load c c) (t load (tp sx) c c) (t store c c)
     (t store (tp) c c) (current-memory) (grow-memory)
     (t unop) (t binop) (t testop)
     (t relop) (t cvtop t) (t const c))
  
  (t ::= i32 i64) ; f32 f64 No floats for now
  (tp ::= i8 i16 i32)
  (tf ::= ((t ...) -> (t ...)))
  (mut? ::= boolean)
  (tg ::= (mut? t))
  (sx ::= signed unsigned)

  ;(unop ::= clz ctz popcnt) Omitted, need bit magic
  (binop ::= add sub mul div-s div-u rem-s rem-u
         and or xor shl shr-s shr-u rotl rotr)
  (testop ::= eqz)
  (relop ::= eq ne lt-s lt-u gt-s gt-u le-s le-u ge-s ge-u)
  (cvtop ::= convert reinterpret) ; Ignored for now because no floats

  (i j ::= natural)
  (c ::= natural) ; No floats for now

  ;; TODO: We don't handle imports
  (f ::= ((ex ...) (func tf (local (t ...) (e ...))))
     #;((ex ...) (func tf im)))
  (glob ::= ((ex ...) (global tg (e ...)))
        #;((ex ...) (global tg im)))
  (tab ::= ((ex ...) (table j (i ...)))
       #;((ex ...) (table j im)))
  (mem ::= ((ex ...) (memory i))
       #;((ex ...) (memory im)))
  ;(im ::= (import string string))
  (ex ::= (export string))
  (m ::= (module (f ...) (glob ...) (tab ...) (mem ...)))
  )

(define-extended-language WASMrt WASM
  (v ::= (t const c))

  (e ::= .... (trap) (call cl) (label (e ...) (e ...))
     (local (j (v ...)) (e ...)))
  (L ::= hole (v ... (label (e ...) L) e ...))

  (s ::= ((inst ...) (tabinst ...) (meminst ...)))
  (cl ::= (j f))
  (inst ::= ((cl ...) (v ...) (table j) (memory j))
        ((cl ...) (v ...) (table j) (memory))
        ((cl ...) (v ...) (table) (memory j))
        ((cl ...) (v ...) (table) (memory)))

  (tabinst ::= (cl ...))
  (meminst ::= (bits any)))

#| TODO: Deprecated
(define-language WASM
  (e ::= (unreachable) (nop) (drop) (select)
     (block tf e ... end) (loop tf e ... end)
     (if tf e ... else e ... end) (br i) (br-if i)
     (br-table i ...) (return) (call i)
     (call-indirect tf) (get-local i) (set-local i)
     (tee-local i) (get-global i) (set-global i)
     (load t tp? sx? a o) (store t tp? a o)
     (current-memory) (grow-memory) (const t c)
     (unop t) (binop t) (testop t)
     (relop t) (cvtop t t sx?))
  
  (t ::= i32 i64 f32 f64)
  (tp ::= i8 i16 i32)
  (tp? ::= tp None)
  (tf ::= (-> (t ...) (t ...)))
  (tg ::= (mut t) t)

  (unop.i ::= clz ctz popcnt)
  (unop.f ::= neg abs ceil floor trunc nearest sqrt)
  (unop ::= unop.i unop.f)
  
  (binop.i ::= add sub mul (div sx) (rem sx)
         and or xor shl (shr sx) rotl rotr)
  (binop.f ::= add sub mul div min max copysign)
  (binop ::= binop.i binop.f)
  
  (testop.i ::= eqz)
  (testop ::= testop.i)
  
  (relop.i ::= eq ne (lt sx) (gt sx) (le sx) (ge sx))
  (relop.f ::= eq ne lt gt le ge)
  (relop ::= relop.i relop.f)
  
  (cvtop ::= convert reinterpret)
  (sx ::= s u)
  (sx? ::= sx None)

  (i ::= variable-not-otherwise-mentioned)
  (c ::= natural real)

  ;; We'll worry about these things later
  #|(f ::= (ex ... func tf local t ... e ...)
     (ex ... func tf im))
  (glob ::= (ex ... global tg e ...) (ex ... global tg im))
  (tab ::= (ex ... table n i ...) (ex ... table n im))
  (im ::= (import string string))
  (ex ::= (export string))
  (tab? ::= tab None)
  (mem? ::= mem None)
  (m ::= (module f ... glob ... tab? mem?))|#
  )

(define-extended-language WASMrt WASM
  ;; We'll worry about these things later too
  ;(s ::= ((insts inst ...) (tab tabinst ...) (mem meminst ...)))
  ;(inst ::= ((func cl ...) (glob v ...) (tab i?) (mem i?)))

  (v ::= (t const c))
  (e ::= .... (trap) (call cl) (label n (e ...) e ... end) (local n (i v ...) e ... end))
  (L ::= (0 (v ... hole e ...)) (k (v ... label n (e ...) L end e ...)))

  (stack ::= [] (v stack)))
|#