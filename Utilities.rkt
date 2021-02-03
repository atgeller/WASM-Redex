#lang racket

(require racket/flonum redex/reduction-semantics "Syntax.rkt" "Bits.rkt" "MachineOps.rkt")

(provide (except-out (all-defined-out) wasm_binop->racket wasm_testop->racket wasm_relop->racket))

(define (wasm_unop->racket type unop)
  (match unop
    [`clz (curry sized-clz (type-width type))]
    [`ctz (curry sized-ctz (type-width type))]
    [`popcnt (curry sized-popcnt (type-width type))]
    [`abs abs]
    [`neg -]
    [`sqrt (lambda (c)
             (if (negative? c)
                 (match type
                   [`f32 (real->single-flonum +nan.f)]
                   [`f64 +nan.0])
                 (sqrt c)))]
    [`ceil ceiling]
    [`floor floor]
    [`nearest round]))

(define (wasm_binop->racket type binop)
  (match binop
    [`add (if (integer-type? type)
              (curry sized-add (type-width type))
              +)]
    [`sub (if (integer-type? type)
              (curry sized-sub (type-width type))
              -)]
    [`mul (if (integer-type? type)
              (curry sized-mul (type-width type))
              *)]
    [`div-s (curry sized-signed-div (type-width type))]
    [`div-u (curry sized-unsigned-div (type-width type))]
    [`rem-s (curry sized-signed-rem (type-width type))]
    [`rem-u (curry sized-unsigned-rem (type-width type))]
    [`and bitwise-and]
    [`or bitwise-ior]
    [`xor bitwise-xor]
    [`shl (curry sized-shl (type-width type))]
    [`shr-s (curry sized-signed-shr (type-width type))]
    [`shr-u (curry sized-unsigned-shr (type-width type))]
    [`rotl (curry sized-rotl (type-width type))]
    [`rotr (curry sized-rotr (type-width type))]
    [`div /]
    [`min min]
    [`max max]
    [`copysign
     (lambda (a b)
       (if (or (eq? b -0.0) (eq? b -inf.0) (equal? (sgn b) -1.0)
               (eq? b -0.0f0) (eq? b -inf.f) (equal? (sgn b) -1.0f0))
           (- (abs a))
           (abs a)))]))

(define (wasm_testop->racket type testop)
  (match testop
    [`eqz (lambda (n) (if (= n 0) 1 0))]))

(define (wasm_relop->racket type relop)
  (match relop
    [`eq =]
    [`ne (compose not =)]
    [`lt-u <]
    [`gt-u >]
    [`le-u <=]
    [`ge-u >=]
    [`lt-s (lambda (a b) (< (to-signed-sized (type-width type) a) (to-signed-sized (type-width type) b)))]
    [`gt-s (lambda (a b) (> (to-signed-sized (type-width type) a) (to-signed-sized (type-width type) b)))]
    [`le-s (lambda (a b) (<= (to-signed-sized (type-width type) a) (to-signed-sized (type-width type) b)))]
    [`ge-s (lambda (a b) (>= (to-signed-sized (type-width type) a) (to-signed-sized (type-width type) b)))]
    ; floating point relops don't have a sign
    [`lt <]
    [`gt >]
    [`le <=]
    [`ge >=]))

(define (wasm-truncate to-size sx c)
  (cond
    [(nan? c) #f]
    [(infinite? c) #f]
    [else
     (let ([val (inexact->exact (truncate c))])
       (match sx
         [`signed (if (< (sub1 (- (expt 2 (sub1 to-size)))) val (expt 2 (sub1 to-size)))
                      (to-unsigned-sized to-size val)
                      #f)]
         [`unsigned (if (< -1 val (expt 2 to-size))
                        val
                        #f)]))]))

(define (wasm_cvtop->racket from-type to-type cvtop sx?)
  (match cvtop
    [`convert
     (match `(,from-type ,to-type)
       [`(i64 i32) (curry to-unsigned-sized 32)]
       [`(i32 i64) (match sx?
                     [`signed (lambda (c) (to-unsigned-sized 64 (to-signed-sized 32 c)))]
                     [`unsigned identity])]
       [`(f64 f32) real->single-flonum]
       [`(f32 f64) real->double-flonum]
       [`(,_ f32) (match sx?
                    [`signed (lambda (c) (real->single-flonum (to-signed-sized (type-width from-type) c)))]
                    [`unsigned real->single-flonum])]
       [`(,_ f64) (match sx?
                    [`signed (lambda (c) (real->double-flonum (to-signed-sized (type-width from-type) c)))]
                    [`unsigned real->double-flonum])]
       [`(,_ i32) (curry wasm-truncate 32 sx?)]
       [`(,_ i64) (curry wasm-truncate 64 sx?)])]
    [`reinterpret
     (match from-type
       [`i32 (lambda (c) (real->single-flonum (floating-point-bytes->real (integer->integer-bytes c 4 #f #f) #f)))]
       [`i64 (lambda (c) (floating-point-bytes->real (integer->integer-bytes c 8 #f #f) #f))]
       [`f32 (lambda (c) (integer-bytes->integer (real->floating-point-bytes c 4 #f) #f #f))]
       [`f64 (lambda (c) (integer-bytes->integer (real->floating-point-bytes c 8 #f) #f #f))])]))

(define-metafunction WASMrt
  eval-unop : unop c t -> e
  [(eval-unop unop c t)
   (t const ,((wasm_unop->racket (term t) (term unop)) (term c)))])

(define-metafunction WASMrt
  eval-binop : binop c c t -> e
  [(eval-binop div-s c 0 t)
   (trap)]
  [(eval-binop div-u c 0 t)
   (trap)]
  [(eval-binop rem-s c 0 t)
   (trap)]
  [(eval-binop rem-u c 0 t)
   (trap)]
  [(eval-binop binop c_1 c_2 t)
   (t const ,((wasm_binop->racket (term t) (term binop)) (term c_1) (term c_2)))
   (side-condition (not (and (eq? (term c_2) 0)
                             (or (eq? (term binop) 'div-s) (eq? (term binop) 'div-u)
                                 (eq? (term binop) 'rem-s) (eq? (term binop) 'rem-u)))))])

(define-metafunction WASMrt
  eval-testop : testop c t -> e
  [(eval-testop testop c t)
   (i32 const ,((wasm_testop->racket (term t) (term testop)) (term c)))])

(define-metafunction WASMrt
  eval-relop : relop c c t -> e
  [(eval-relop relop c_1 c_2 t)
   (i32 const ,(if ((wasm_relop->racket (term t) (term relop)) (term c_1) (term c_2)) 1 0))])

(define-metafunction WASMrt
  eval-cvtop : cvtop c t_1 t_2 any -> e
  [(eval-cvtop cvtop c t_1 t_2 (name sx? any))
   ,(match ((wasm_cvtop->racket (term t_1) (term t_2) (term cvtop) (term sx?)) (term c))
      [#f (term (trap))]
      [c-new (term (t_2 const ,c-new))])])

;; Decompose local contexts
; Function to calculate local context depth
(define-metafunction WASMrt
  context-depth : L -> j
  [(context-depth hole) 0]
  [(context-depth (v ... (label n (e ...) L_1) e_2 ...)) ,(+ (term 1) (term (context-depth L_1)))])

; Second function to extract jth outer-layer
(define-metafunction WASMrt
  decompose : L j (v ...) -> (e ...)
  [(decompose (v ... (label n (e ...) L_1) e_2 ...) j (v_2 ...))
   (v ... v_2 ... e ... e_2 ...)
   (side-condition (= (term j) (term (context-depth L_1))))]
  [(decompose (v ... (label n (e ...) L_1) e_2 ...) j (v_2 ...))
   (v ... (label n (e ...) (decompose L_1 j (v_2 ...))) e_2 ...)
   (side-condition (< (term j) (term (context-depth L_1))))])

;; TODO: Pretty much all the utils below here are kind of awkward and unwieldy in combination.
;; I'm certain the solution is either calling into Racket more often or less often,
;; but I'm not sure which

;; NOTE: Useful for the very popular myriad index-based lookups
(define-metafunction WASMrt
  do-get : (any ...) j -> any
  [(do-get () j)
   (error "do-get index out of bounds!")]
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
  [(do-global-get (((cl ...) (v ...) _ _) inst_2 ...) 0 j_1)
   (do-get (v ...) j_1)])

(define-metafunction WASMrt
  inst-global-set : inst j v -> (inst)
  [(inst-global-set ((cl ...) (v ...) (name table any_1) (name memory any_2)) j v_2)
   (((cl ...) (do-set (v ...) j v_2) table memory))])

(define-metafunction WASMrt
  do-global-set : (inst ...) j j_1 v -> (inst ...)
  [(do-global-set (inst ...) j j_1 v)
   ,(let* ([head (take (term (inst ...)) (term j))]
           [tail (drop (term (inst ...)) (term j))]
           [to-change (car tail)]
           [rest (cdr tail)])
      (append head (term (inst-global-set ,to-change j_1 v)) rest))])

(define-metafunction WASMrt
  function-lookup : (inst ...) j j_1 -> cl
  [(function-lookup (inst_1 inst_2 ...) j j_1)
   (function-lookup (inst_2 ...) ,(sub1 (term j)) j_1)
   (side-condition (> (term j) 0))]
  [(function-lookup (((cl ...) (v ...) _ _) inst_2 ...) 0 j_1)
   (do-get (cl ...) j_1)])

(define-metafunction WASMrt
  setup-call : (v ...) cl (e ...) -> (e ...)
  [(setup-call (v ...) (j (func _ ((t ...) -> (t_2 ...)) (local (t_3 ...) (e ...)))) (e_2 ...))
   ; 1. strip arguments from stack
   ,(let-values ([(stack args) (split-at (term (v ...)) (- (length (term (v ...)))
                                                           (length (term (t ...)))))])
      ; 2. initialize locals
      ;; TODO: something we can optimize in the type system? Check F to TAL
      ;; May need something more explicit in the type system
      (let* ([initialized (map (lambda (t) (term (,t const 0))) (term (t_3 ...)))]
             [locals (append args initialized)]
             [m (length (term (t_2 ...)))])
        ; 3. combine and return
        (term ,(append stack (term ((local ,m (j ,locals) ((block (() -> (t_2 ...)) (e ...)))))) (term (e_2 ...))))))
   ])

(define-metafunction WASMrt
  inst-table : inst -> j
  [(inst-table (_ _ (table j) _)) j])

(define-metafunction WASMrt
  inst-memory : inst -> j
  [(inst-memory (_ _ _ (memory j))) j])

(define-metafunction WASMrt
  check-tf : tf cl -> e
  [(check-tf tf (j (func (ex ...) tf (local (t ...) (e ...)))))
   (call (j (func (ex ...) tf (local (t ...) (e ...)))))]
  [(check-tf tf_!_ (_ (func _ tf_!_ _)))
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
  (let* ([memindex (term (inst-memory (do-get ,insts ,index)))]
         [mem (term (do-get ,meminsts ,memindex))])
    (values mem memindex)))

; Memory operations
;; TODO: cleanup on aisle 5
(define-metafunction WASMrt
  do-load : s j t a o -> e
  [(do-load ((inst ...) (tabinst ...) (meminst ...)) j t a o)
   ,(let-values ([(mem _) (get-mem (term (inst ...)) (term (meminst ...)) (term j))])
      (match (wrapped-load mem (term t) (term a) (term o))
        [#f (term (trap))]
        [val (term (t const ,val))]))])

(define-metafunction WASMrt
  do-load-packed : s j t a o tp sx -> e
  [(do-load-packed ((inst ...) (tabinst ...) (meminst ...)) j t a o tp sx)
   ,(let-values ([(mem _) (get-mem (term (inst ...)) (term (meminst ...)) (term j))])
      (match (wrapped-load-packed mem (term t) (term a) (term o) (term tp) (term sx))
        [#f (term (trap))]
        [val (term (t const ,val))]))])

(define-metafunction WASMrt
  do-store : s j t a o c -> (s (e ...))
  [(do-store ((inst ...) (tabinst ...) (meminst ...)) j t a o c)
   ,(let-values ([(mem memindex) (get-mem (term (inst ...)) (term (meminst ...)) (term j))])
      (match (wrapped-store mem (term t) (term a) (term o) (term c))
        [#f (term (((inst ...) (tabinst ...) (meminst ...)) ((trap))))]
        [newmem (term (((inst ...)
                        (tabinst ...)
                        (do-set (meminst ...) ,memindex ,newmem))
                       ()))]))])

(define-metafunction WASMrt
  do-store-packed : s j t a o c tp -> (s (e ...))
  [(do-store-packed ((inst ...) (tabinst ...) (meminst ...)) j t a o c tp)
   ,(let-values ([(mem memindex) (get-mem (term (inst ...)) (term (meminst ...)) (term j))])
      (match (wrapped-store-packed mem (term t) (term a) (term o) (term c) (term tp))
        [#f (term (((inst ...) (tabinst ...) (meminst ...)) ((trap))))]
        [newmem (term (((inst ...)
                        (tabinst ...)
                        (do-set (meminst ...) ,memindex ,newmem))
                       ()))]))])

(define-metafunction WASMrt
  mem-size : s j -> v
  [(mem-size ((inst ...) _ (meminst ...)) j)
   (i32 const ,(let-values ([(mem _) (get-mem (term (inst ...)) (term (meminst ...)) (term j))])
                 (memory-pages mem)))])

(define-metafunction WASMrt
  grow-mem : s j c -> (s (e ...))
  [(grow-mem ((inst ...) (tabinst ...) (meminst ...)) j c)
   ,(let*-values ([(mem memindex) (get-mem (term (inst ...)) (term (meminst ...)) (term j))]
                  [(newmem res) (grow-memory mem (term c))])
      (term (((inst ...)
              (tabinst ...)
              (do-set (meminst ...) ,memindex ,newmem))
             ((i32 const ,res)))))])
