#lang racket

(require redex/reduction-semantics "Syntax.rkt")

(provide (except-out (all-defined-out) wasm_binop->racket wasm_testop->racket wasm_relop->racket))

;; TODO: don't provide these
(define wasm_binop->racket
  `([add . ,+]
    [sub . ,-]
    [mul . ,*]
    [div . ,(lambda (a b) (exact-floor (/ a b)))]
    [rem . ,remainder]
    [and . ,bitwise-and]
    [or . ,bitwise-ior]
    [xor . ,bitwise-xor]))

;; TODO: don't provide these
(define wasm_testop->racket
  `([eqz . ,(if (curry = 0) 1 0)]))

;; TODO: don't provide these
(define wasm_relop->racket
  `([eq . ,=]
    [ne . ,(lambda (a b) (not (= a b)))]
    [lt . ,<]
    [gt . ,>]
    [le . ,<=]
    [ge . ,>=]))

(define-metafunction WASMrt
  eval-binop : binop c c t -> e
  [(eval-binop div c 0 t)
   (trap)]
  [(eval-binop rem c 0 t)
   (trap)]
  [(eval-binop binop c_1 c_2 t)
   (t const ,((dict-ref wasm_binop->racket (term binop)) (term c_1) (term c_2)))
   (side-condition (not (and (eq? (term c_2) 0)
                             (or (eq? (term binop) 'rem) (eq? (term binop) 'div)))))])

(define-metafunction WASMrt
  eval-testop : testop c t -> e
  [(eval-testop testop c t)
   (t const ,((dict-ref wasm_testop->racket (term testop)) (term c)))])

(define-metafunction WASMrt
  eval-relop : relop c c -> e
  [(eval-relop relop c_1 c_2 t)
   (t const ,(if ((dict-ref wasm_relop->racket (term binop)) (term c_1) (term c_2)) 1 0))])

;; decompose local contexts
;; Function to calculate local context depth
(define-metafunction WASMrt
  context-depth : L -> j
  [(context-depth hole) 0]
  [(context-depth (v ... (label (e ...) L_1) e_2 ...)) ,(+ (term 1) (term (context-depth L_1)))])

;; Second function to extract jth outer-layer
(define-metafunction WASMrt
  decompose : L j (v ...) -> (e ...)
  [(decompose (v ... (label (e ...) L_1) e_2 ...) j (v_2 ...))
   (v ... v_2 ... e ... e_2 ...)
   (side-condition (>= (term j) (term (context-depth L_1))))]
  [(decompose (v ... (label (e ...) L_1) e_2 ...) j (v_2 ...))
   (v ... (label (e ...) (decompose L_1 j (v_2 ...))) e_2 ...)
   (side-condition (< (term j) (term (context-depth L_1))))])

;; Todo: these were very easily inlined, should we have them here?
(define-metafunction WASMrt
  do-get : (v ...) j -> v
  [(do-get () j)
   (error "Not enough locals!")]
  [(do-get (v ...) j)
   ,(car (drop (term (v ...)) (term j)))])

;; Todo: these were very easily inlined, should we have them here?
(define-metafunction WASMrt
  do-set : (v ...) j v_2 -> (v ...)
  [(do-set () j v_2)
   (error "Not enough locals!")]
  [(do-set (v ...) j v_2)
   ,(append (take (term (v ...)) (term j)) (term (v_2)) (drop (term (v ...)) (add1 (term j))))])

;Test: (term (do-get (do-set ((i32 const 0) (i32 const 1) (i32 const 2)) 1 (i32 const 11)) 1))