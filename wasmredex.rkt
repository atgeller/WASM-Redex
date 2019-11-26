#lang racket

(require redex/reduction-semantics)

(define-language WASM
  (e ::= (unreachable) (nop) (drop) (select)
     (block tf (e ...)) (loop tf (e ...))
     (if tf (e ...) else (e ...)) (br j) (br-if j)
     (br-table (j ...)) (return) (call i)
     (call-indirect tf) (get-local j) (set-local j)
     (tee-local i) (get-global i) (set-global i)
     (load t a o) (store t a o)
     (current-memory) (grow-memory) (t const c)
     (unop t)
     (binop t) (testop t)
     (relop t) (cvtop t t))
  
  (t ::= i32 i64 f32 f64) ; No floats for now
  (tp? ::= tp None)
  (tf ::= ((t ...) -> (t ...)))
  (tg ::= (mut t) t)

  ;(unop ::= clz ctz popcnt) Omitted, need bit magic
  (binop ::= add sub mul div rem
         and or xor) ; shl shr rotl rotr Omitted, need bit magic
  (testop ::= eqz)
  (relop ::= eq ne lt gt le ge)
  (cvtop ::= convert reinterpret) ; Ignored for now because no floats

  (i ::= variable-not-otherwise-mentioned)
  (j ::= natural)
  (c ::= natural) ; No floats for now

  ;; TODO: PARENTHESIZE
  (f ::= (ex ... func tf local t ... e ...)
  (ex ... func tf im))
  (glob ::= (ex ... global tg e ...) (ex ... global tg im))
  (tab ::= (ex ... table n i ...) (ex ... table n im))
  (im ::= (import string string))
  (ex ::= (export string))
  (tab? ::= tab None)
  (mem? ::= mem None)
  (m ::= (module f ... glob ... tab? mem?))
  )

(define-extended-language WASMrt WASM
  (v ::= (t const c))
  (stack ::= [] (v stack))

  (e ::= .... (trap) (call cl) (label (e ...) (e ...)) (local (i (v ...)) (e ...)))
  (L ::= hole (v ... (label (e ...) L) e ...))
  )

(define wasm_binop->racket
  `([add . ,+]
    [sub . ,-]
    [mul . ,*]
    [div . ,(lambda (a b) (exact-floor (/ a b)))]
    [rem . ,remainder]
    [and . ,bitwise-and]
    [or . ,bitwise-ior]
    [xor . ,bitwise-xor]))

(define wasm_testop->racket
  `([eqz . ,(if (curry = 0) 1 0)]))

(define wasm_relop->racket
  `([eq . ,=]
    [ne . ,(lambda (a b) (not (= a b)))]
    [lt . ,<]
    [gt . ,>]
    [le . ,<=]
    [ge . ,>=]))

;; decompose local contexts
;; Function to calculate local context depth
;; Second function to extract jth outer-layer

(define-metafunction WASMrt
  eval-binop : binop c c t -> v
  [(eval-binop binop c_1 c_2 t)
   (t const ,((dict-ref wasm_binop->racket (term binop)) (term c_1) (term c_2)))])

(define-metafunction WASMrt
  eval-testop : testop c t -> v
  [(eval-testop testop c t)
   (t const ,((dict-ref wasm_testop->racket (term testop)) (term c)))])

(define-metafunction WASMrt
  eval-relop : relop c c -> v
  [(eval-relop relop c_1 c_2 t)
   (t const ,(if ((dict-ref wasm_relop->racket (term binop)) (term c_1) (term c_2)) 1 0))])

(define-metafunction WASMrt
  seq->stack : v ... stack -> stack
  [(seq->stack stack) stack]
  [(seq->stack v stack) (v stack)]
  [(seq->stack v_1 v_2 ... stack) (v_1 (seq->stack v_2 ... stack))])

(define-metafunction WASMrt
  context-depth : L -> j
  [(context-depth hole) 0]
  [(context-depth (v ... (label (e ...) L_1) e_2 ...)) ,(+ (term 1) (term (context-depth L_1)))])

(define-metafunction WASMrt
  decompose : L j (v ...) -> (e ...)
  [(decompose hole)
   (error "This shouldn't happen...")]
  [(decompose (v ... (label (e ...) L_1) e_2 ...) j (v_2 ...))
   (v_2 ... e ...)
   (side-condition (>= (term j) (term (context-depth L_1))))]
  [(decompose (v ... (label (e ...) L_1) e_2 ...) j (v_2 ...))
   (v ... (label (e ...) (decompose L_1 j)) e_2 ...)
   (side-condition (< (term j) (term (context-depth L_1))))])

(define-metafunction WASMrt
  do-get : (v ...) j -> v
  [(do-get () j)
   (error "Not enough locals!")]
  [(do-get (v ...) j)
   ,(car (drop (term (v ...)) (term j)))])

(define-metafunction WASMrt
  do-set : (v ...) j v_2 -> (v ...)
  [(do-set () j v_2)
   (error "Not enough locals!")]
  [(do-set (v ...) j v_2)
   ,(append (take (term (v ...)) (term j)) (term (v_2)) (drop (term (v ...)) (add1 (term j))))])

;Test: (term (do-get (do-set ((i32 const 0) (i32 const 1) (i32 const 2)) 1 (i32 const 11)) 1))

;; TL;DR about stack: the stack is implicit in the stream of instructions being processed.
;; This is because v \subset e, so although we say (e ...) it ends up looking like (v ... e ...).
;; Thus, the next instruction to execute is the head of e ..., and the stack is v ...
(define ->
    (reduction-relation
     WASMrt
     #:domain ((v ...) (e ...))
     #:codomain ((v ...) (e ...))
     ;; ((v ...) (e ...)) -> ((v ...) (e ...))
     ;; Every instruction is operating inside of an execution environment.
     ;; We need these contexts to keep track of the stack and next instructions at each level,
     ;; and to handle branching/returning from levels.
     ;; Much of the instructions don't need to know about the environment they are executing in, but some do.
     ;; Todo: It would be nice to reduce the boilerplate for those that don't.
     
     ;; Due to validation we can be sure we are returning the proper number of values
     ;; Todo: Add store.
     (--> ((v ...) (in-hole L (v_1 ... (t const c_1) (t const c_2) (binop t) e ...)))
          ((v ...) (in-hole L (v_1 ... (eval-binop binop c_1 c_2 t) e ...))))
     
     (--> ((v ...) (in-hole L (v_1 ... (t const c) (testop t) e ...)))
          ((v ...) (in-hole L (v_1 ... (eval-testop testop c t) e ...))))
     
     (--> ((v ...) (in-hole L (v_1 ... (t const c_1) (t const c_2) (relop t) e ...)))
          ((v ...) (in-hole L (v_1 ... (eval-relop relop c_1 c_2 t) e ...))))
          
     (--> ((v ...) (in-hole L (v_1 ... (nop) e ...)))
          ((v ...) (in-hole L (v_1 ... e ...))))

     (--> ((v ...) (in-hole L (v_1 ... (unreachable) e ...)))
          ((v ...) (in-hole L (v_1 ... (trap) e ...))))

     (--> ((v ...) (in-hole L (v_1 ... v_2 v_3 (i32 const 0) (select) e ...)))
          ((v ...) (in-hole L (v_1 ... v_3 e ...))))

     (--> ((v ...) (in-hole L (v_1 ... v_2 v_3 (i32 const c) (select) e ...)))
          ((v ...) (in-hole L (v_1 ... v_2 e ...)))
          (side-condition (> (term c) 0)))
     
     (--> ((v ...) (in-hole L (v_1 ... v_2 (drop) e ...)))
          ((v ...) (in-hole L (v_1 ... e ...))))
     
     (--> ((v ...) (in-hole L (v_1 ... (i32 const 0) (if tf (e_1 ...) else (e_2 ...)) e ...)))
          ((v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e ...))))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const c) (if tf (e_1 ...) else (e_2 ...)) e ...)))
          ((v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e ...)))
          (side-condition (> (term c) 0)))
     
     (--> ((v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e_2 ...)))
          ((v ...) (in-hole L (v_1 ... (label () (e_1 ...)) e_2 ...))))

     (--> ((v ...) (in-hole L (v_1 ... (get-local j) e ...)))
          ((v ...) (in-hole L (v_1 ... ,(car (drop (term (v ...)) (term j))) e ...))))
     
     (--> ((v ...) (in-hole L (v_1 ... v_2 (set-local j) e ...)))
          (,(append (take (term (v ...)) (term j)) (term (v_2)) (drop (term (v ...)) (add1 (term j))))
           (in-hole L (v_1 ... e ...))))

     (--> ((v ...) (in-hole L (v_1 ... trap e ...)))
          ((v ...) (in-hole L (trap))))

     (--> ((v ...) (in-hole L (v_1 ... (label () (trap)) e ...)))
          ((v ...) (in-hole L (trap))))

     ;; Knowing about contexts is necessary for this (so can't shortcut the rest :/)!
     (--> ((v ...) (in-hole L (v_1 ... (br j) e ...)))
          ((v ...) (decompose L j (v_1 ...))))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const 0) (br-if j) e ...)))
          ((v ...) (in-hole L (v_1 ... e ...))))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const c) (br-if j) e ...)))
          ((v ...) (in-hole L (v_1 ... (br j) e ...)))
          (side-condition (> (term c) 0)))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const c) (br-table (j ...)) e ...)))
          ((v ...) (in-hole L (v_1 ... v_2 (br ,(car (drop (term (j ...)) (term c)))) e ...)))
          (side-condition (<= (term c) (length (term (j ...))))))

     (--> ((v ...) (in-hole L (v_1 ... (i32 const c) (br-table (j ...)) e ...)))
          ((v ...) (in-hole L (v_1 ... v_2 (br ,(last (term (j ...)))) e ...)))
          (side-condition (> (term c) (length (term (j ...))))))

     (--> ((v ...) (in-hole L (v_1 ... (label () (v_2 ...)) e ...)))
          ((v ...) (v_1 ... v_2 ... e ...)))
     ))

#;(redex-match WASMrt
             (in-hole L (br j))
             (term ((label ((add i32)) ((label ((div i32)) ((label ((mul i32)) ((br 1))))))))))

;(term (decompose (label ((add i32)) ((label ((div i32)) ((label ((mul i32)) ((br 1))))))) 1))

;(apply-reduction-relation -> (term (((i32 const 0) ((i32 const 1) [])) (add i32))))
;(apply-reduction-relation -> (term (((i32 const 10) ((i32 const 3) [])) (div i32))))

(redex-match WASMrt ((v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e_2 ...)))
             (term (() ((block (() -> ()) ((block (() -> ()) ((i32 const 0) (i32 const 1) (add i32) (br-if 1))) (unreachable)))))))

(redex-match WASMrt ((v ...) (in-hole L (v_1 ... (block tf (e_1 ...)) e_2 ...)))
             (term (() ((label () ((block (() -> ()) ((i32 const 0) (i32 const 1) (add i32) (br-if 1))) (unreachable)))))))

(apply-reduction-relation* -> (term (() ((block (() -> ()) ((block (() -> ()) ((i32 const 0) (i32 const 1) (add i32) (br-if 1))) (unreachable)))))))

#|
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

(define-extended-language WASMct WASM
  (C ::= (local t ...)))

(define-metafunction WASMrt
  eval-binop.i : c c binop.i t -> c
  [(eval-binop.i c1 c2 add t)
   (t const ,(+ (term c1) (term c2)))
   #;(where (cnew) ,(+ (term c1) (term c2)))])

(define-metafunction WASMrt
  eval-binop.f : c c binop.f t -> c
  [(eval-binop.f c1 c2 add t)
   (t const ,(+ (term c1) (term c2)))
   #;(where (cnew) ,(+ (term c1) (term c2)))])

(define-metafunction WASMrt
  eval-binop : c c binop t -> c
  [(eval-binop c1 c2 op i32)
   (eval-binop.i c1 c2 op t)]
  [(eval-binop c1 c2 op i64)
   (eval-binop.i c1 c2 op t)]
  [(eval-binop c1 c2 op f32)
   (eval-binop.f c1 c2 op t)]
  [(eval-binop c1 c2 op f64)
   (eval-binop.f c1 c2 op t)])

(define ->
    (reduction-relation
     WASMrt
     #:domain (stack e)
     (--> (((t1 const c1) ((t2 const c2) s*)) (binop t3))
          ((eval-binop (term binop) (term c1) (term c2)) s*))))

(redex-match? WASMrt (binop t) (term (add i32)))
(redex-match? WASMrt stack (term ((i32 const 0) ((i32 const 1) []))))
(redex-match? WASMrt t (term i32))
(redex-match? WASMrt c (term 1))
(redex-match? WASMrt v (term (i32 const 0)))

(redex-match? WASMrt ((t const c) ((t1 const c1) [])) (term ((i32 const 0) (i32 const 1) [])))
(redex-match WASMrt stack (term ((i32 const 0) ((i32 const 1) []))))
(redex-match WASMrt (v1 (v2 [])) (term ((i32 const 0) (i32 const 1) [])))

(apply-reduction-relation -> (term (((i32 const 0) ((i32 const 1) [])) (add i32))))|#