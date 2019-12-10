#lang racket

(require redex/reduction-semantics "Syntax.rkt")

(provide (except-out (all-defined-out) wasm_binop->racket wasm_testop->racket wasm_relop->racket))

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

;; Decompose local contexts
; Function to calculate local context depth
(define-metafunction WASMrt
  context-depth : L -> j
  [(context-depth hole) 0]
  [(context-depth (v ... (label (e ...) L_1) e_2 ...)) ,(+ (term 1) (term (context-depth L_1)))])

; Second function to extract jth outer-layer
(define-metafunction WASMrt
  decompose : L j (v ...) -> (e ...)
  [(decompose (v ... (label (e ...) L_1) e_2 ...) j (v_2 ...))
   (v ... v_2 ... e ... e_2 ...)
   (side-condition (>= (term j) (term (context-depth L_1))))]
  [(decompose (v ... (label (e ...) L_1) e_2 ...) j (v_2 ...))
   (v ... (label (e ...) (decompose L_1 j (v_2 ...))) e_2 ...)
   (side-condition (< (term j) (term (context-depth L_1))))])

;; TODO: Pretty much all the utils below here are kind of awkward and unwieldy in combination.
;; I'm certain the solution is either calling into Racket more often or less often,
;; but I'm not sure which

;; NOTE: Useful for the very popular myriad index-based lookups
(define-metafunction WASMrt
  do-get : (any ...) j -> any
  [(do-get () j)
   (error "Not enough locals!")]
  [(do-get (any ...) j)
   ,(car (drop (term (any ...)) (term j)))])

;; NOTE: Useful for the very popular myriad index-based lookups
(define-metafunction WASMrt
  do-set : (v ...) j v_2 -> (v ...)
  [(do-set () j v_2)
   (error "Not enough locals!")]
  [(do-set (v ...) j v_2)
   ,(append (take (term (v ...)) (term j))
            (term (v_2))
            (drop (term (v ...)) (add1 (term j))))])

(define-metafunction WASMrt
  do-global-get : (inst ...) j j_1 -> v
  [(do-global-get (inst_1 inst_2 ...) j j_1)
   (do-global-get (inst_2 ...) ,(sub1 (term j)) j_1)
   (side-condition (> (term j) 0))]
  ;; Todo: This is probably slower than calling into racket, but is it significant?
  [(do-global-get (((cl ...) (v ...)) inst_2 ...) 0 j_1)
   (do-get (v ...) j_1)])

(define-metafunction WASMrt
  inst-set : inst j v -> (inst)
  [(inst-set ((cl ...) (v ...)) j v_2)
   (((cl ...) (do-set (v ...) j v_2)))])

(define-metafunction WASMrt
  do-global-set : (inst ...) j j_1 v -> (inst ...)
  [(do-global-set (inst ...) j j_1)
   ,(let* ([head (take (term (inst ...)) (term j))]
           [tail (drop (term (inst ...)) (term j))]
           [to-change (car tail)]
           [rest (cdr tail)])
      (append head (term (inst-set to-change j_1 v)) rest))])

(define-metafunction WASMrt
  function-lookup : (inst ...) j j_1 -> cl
  [(function-lookup (inst_1 inst_2 ...) j j_1)
   (function-lookup (inst_2 ...) ,(sub1 (term j)) j_1)
   (side-condition (> (term j) 0))]
  [(function-lookup (((cl ...) (v ...)) inst_2 ...) 0 j_1)
   (do-get (cl ...) j_1)])

(define-metafunction WASMrt
  setup-call : (v ...) cl (e ...) -> (e ...)
  [(setup-call (v ...) (j (func ((t ...) -> (t_2 ...)) (local (t_3 ...) (e ...)))) (e_2 ...))
   ; 1. strip arguments from stack
   ,(let-values ([(stack args) (split-at (term (v ...)) (- (length (term (v ...)))
                                                           (length (term (t ...)))))])
      ; 2. initialize locals
      ;; TODO: something we can optimize in the type system? Check F to TAL
      ;; May need something more explicit in the type system
      (let* ([initialized (map (lambda (t) (term (,t const 0))) (term (t_3 ...)))]
             [locals (append args initialized)])
        ; 3. combine and return
        (term ,(append stack (term ((local (j ,locals) ((block (() -> (t_2 ...)) (e ...)))))) (term (e_2 ...))))))
   ])

(define-metafunction WASMrt
  handle-call-indirect : s j j_1 tf -> cl)
