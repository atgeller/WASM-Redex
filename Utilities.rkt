#lang racket

(require redex/reduction-semantics "Syntax.rkt" "Bits.rkt" "MachineOps.rkt")

(provide (except-out (all-defined-out) wasm_binop->racket wasm_testop->racket wasm_relop->racket))

(define (wasm_binop->racket size binop)
  (match binop
    [`add (curry sized-add size)]
    [`sub (curry sized-sub size)]
    [`mul (curry sized-mul size)]
    [`(div unsigned) (curry sized-unsigned-div size)]
    [`(div signed) (curry sized-signed-div size)]
    [`(rem unsigned) (curry sized-unsigned-rem size)]
    [`(rem signed) (curry sized-signed-rem size)]
    [`and bitwise-and]
    [`or bitwise-ior]
    [`xor bitwise-xor]
    [`shl (curry sized-shl size)]
    [`(shr unsigned) (curry sized-unsigned-shr size)]
    [`(shr signed) (curry sized-signed-shr size)]
    [`rotl (curry sized-rotl size)]
    [`rotr (curry sized-rotr size)]))

(define wasm_testop->racket
  `([eqz . ,(if (curry = 0) 1 0)]))

(define (wasm_relop->racket word-size relop)
  (match relop
    [`eq =]
    [`ne (lambda (a b) (not (= a b)))]
    [`(lt unsigned) <]
    [`(gt unsigned) >]
    [`(le unsigned) <=]
    [`(ge unsigned) >=]
    [`(lt signed) (lambda (a b) (< (to-signed-sized word-size a) (to-signed-sized word-size b)))]
    [`(gt signed) (lambda (a b) (> (to-signed-sized word-size a) (to-signed-sized word-size b)))]
    [`(le signed) (lambda (a b) (<= (to-signed-sized word-size a) (to-signed-sized word-size b)))]
    [`(ge signed) (lambda (a b) (>= (to-signed-sized word-size a) (to-signed-sized word-size b)))]))

(define-metafunction WASMrt
  eval-binop : binop c c t -> e
  [(eval-binop (div _) c 0 t)
   (trap)]
  [(eval-binop (rem _) c 0 t)
   (trap)]
  [(eval-binop binop c_1 c_2 t)
   (t const ,((wasm_binop->racket (type-width (term t)) (term binop)) (term c_1) (term c_2)))
   (side-condition (not (and (eq? (term c_2) 0)
                             (or (eq? (term binop) 'rem) (eq? (term binop) 'div)))))])

(define-metafunction WASMrt
  eval-testop : testop c t -> e
  [(eval-testop testop c t)
   (i32 const ,((wasm_testop->racket (type-width (term t)) (term testop)) (term c)))])

(define-metafunction WASMrt
  eval-relop : relop c c -> e
  [(eval-relop relop c_1 c_2 t)
   (i32 const ,(if ((wasm_relop->racket (type-width (term t)) (term relop)) (term c_1) (term c_2)) 1 0))])

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
   (side-condition (= (term j) (term (context-depth L_1))))]
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
  do-set : (any ...) j any -> (any ...)
  [(do-set (any_1 ...) j any_2)
   ,(append (take (term (any_1 ...)) (term j))
            (term (any_2))
            (drop (term (any_1 ...)) (add1 (term j))))])

(define-metafunction WASMrt
  do-global-get : (inst ...) j j_1 -> v
  [(do-global-get (inst_1 inst_2 ...) j j_1)
   (do-global-get (inst_2 ...) ,(sub1 (term j)) j_1)
   (side-condition (> (term j) 0))]
  ;; Todo: This is probably slower than calling into racket, but is it significant?
  [(do-global-get (((cl ...) (v ...)) inst_2 ...) 0 j_1)
   (do-get (v ...) j_1)])

(define-metafunction WASMrt
  inst-global-set : inst j v -> (inst)
  [(inst-global-set ((cl ...) (v ...) (name table any) (name memory any)) j v_2)
   (((cl ...) (do-set (v ...) j v_2) table memory))])

(define-metafunction WASMrt
  do-global-set : (inst ...) j j_1 v -> (inst ...)
  [(do-global-set (inst ...) j j_1)
   ,(let* ([head (take (term (inst ...)) (term j))]
           [tail (drop (term (inst ...)) (term j))]
           [to-change (car tail)]
           [rest (cdr tail)])
      (append head (term (inst-global-set to-change j_1 v)) rest))])

(define-metafunction WASMrt
  function-lookup : (inst ...) j j_1 -> cl
  [(function-lookup (inst_1 inst_2 ...) j j_1)
   (function-lookup (inst_2 ...) ,(sub1 (term j)) j_1)
   (side-condition (> (term j) 0))]
  [(function-lookup (((cl ...) (v ...) _ _) inst_2 ...) 0 j_1)
   (do-get (cl ...) j_1)])

(define-metafunction WASMrt
  setup-call : (v ...) cl (e ...) -> (e ...)
  [(setup-call (v ...) (j (() (func ((t ...) -> (t_2 ...)) (local (t_3 ...) (e ...))))) (e_2 ...))
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
  inst-table : inst -> j
  [(inst-table (_ _ (table j) _)) j])

(define-metafunction WASMrt
  inst-memory : inst -> j
  [(inst-memory (_ _ _ (memory j))) j])

(define-metafunction WASMrt
  check-tf : tf cl -> e
  [(check-tf tf (j (_ (func tf (local (t ...) (e ...))))))
   (call (j (() (func tf (local (t ...) (e ...))))))]
  [(check-tf tf_!_ (_ (_ (func tf_!_ _))))
   (trap)])

(define-metafunction WASMrt
  handle-call-indirect : s j j_1 tf -> e
  [(handle-call-indirect ((inst ...) (tabinst ...) _) j j_1 tf)
   (check-tf tf
             (do-get
              (do-get (tabinst ...)
                      (inst-table (do-get (inst ...) j)))
              j_1))])

(define (get-mem insts meminsts index)
  (match-let* ([memindex (term (inst-memory (do-get ,insts ,index)))]
               [`(bits ,mem) (term (do-get ,meminsts ,memindex))])
    (cons mem memindex)))

; Memory operations
;; TODO: cleanup on aisle 5
(define-metafunction WASMrt
  ; c - align, c_1 - offset + index
  do-load : s j t c c_1 any -> e
  [(do-load ((inst ...) (tabinst ...) (meminst ...)) j t c c_1 (name tp-sx? any))
   ,(wrapped-load (car (get-mem (term (inst ...)) (term (meminst ...)) (term j)))
                  (term t)
                  (term c)   ; align
                  (term c_1) ; offset + index
                  (term tp-sx?))])

(define-metafunction WASMrt
  ; c - align, c_1 - offset + index, c_2 - value
  do-store : s j t c c_1 c_2 any -> (s (e ...))
  [(do-store ((inst ...) (tabinst ...) (meminst ...)) j t c c_1 c_2 (name tp? any))
   ,(let* ([meminfo (get-mem (term (inst ...)) (term (meminst ...)) (term j))]
           [mem (car meminfo)]
           [memindex (cdr meminfo)]
           [result (wrapped-store mem
                                  (term t)
                                  (term c)   ; align
                                  (term c_1) ; offset + index
                                  (term c_2) ; value
                                  (term tp?))])
      (if (redex-match? WASMrt (trap) result)
          (term (((inst ...) (tabinst ...) (meminst ...)) ((trap))))
          (term (((inst ...)
                  (tabinst ...)
                  (do-set (meminst ...) ,memindex ,result))
                 ()))))])

(define-metafunction WASMrt
  mem-size : s j -> v
  [(mem-size ((inst ...) _ (meminst ...)) j)
   (i32 const ,(memory-size (car (get-mem (term (inst ...)) (term (meminst ...)) (term j)))))])

(define-metafunction WASMrt
  grow-mem : s j c -> (s (e ...))
  [(grow-mem ((inst ...) _ (meminst ...)) j c)
   ,(match-let* ([(cons mem memindex) (get-mem (term (inst ...)) (term (meminst ...)) (term j))]
                 [(cons newmem res) (grow-memory (car mem) (term c))])
      (term (((inst ...) _ (do-set (meminst ...) ,memindex ,res)) (i32 const ,res))))])
