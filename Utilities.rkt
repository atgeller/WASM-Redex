#lang racket

(require racket/flonum redex/reduction-semantics "Syntax.rkt" "Bits.rkt" "MachineOps.rkt")

(provide (all-defined-out))

(define-metafunction WASMrt
  bool : boolean -> c
  [(bool #f) 0]
  [(bool #t) 1])

(define-metafunction WASMrt
  signed : t n -> integer
  [(signed i32 n) ,(to-signed-sized 32 (term n))]
  [(signed i64 n) ,(to-signed-sized 64 (term n))])

(define-metafunction WASMrt
  const->bstr : t c -> bstr
  [(const->bstr i32 c) ,(integer->integer-bytes (term c) 4 #f #f)]
  [(const->bstr i64 c) ,(integer->integer-bytes (term c) 8 #f #f)]
  [(const->bstr f32 c) ,(real->floating-point-bytes (term c) 4 #f)]
  [(const->bstr f64 c) ,(real->floating-point-bytes (term c) 8 #f)])

(define-metafunction WASMrt
  bstr->const : t bstr -> c
  [(bstr->const inn bstr) ,(integer-bytes->integer (term bstr) #f #f)]
  [(bstr->const f32 bstr) ,(real->single-flonum (floating-point-bytes->real (term bstr) #f))]
  [(bstr->const f64 bstr) ,(real->double-flonum (floating-point-bytes->real (term bstr) #f))])


(define-metafunction WASMrt
  eval-unop : unop t c -> c
  
  [(eval-unop clz t c) ,(sized-clz (type-width (term t)) (term c))]
  [(eval-unop ctz t c) ,(sized-ctz (type-width (term t)) (term c))]
  [(eval-unop popcnt t c) ,(sized-popcnt (type-width (term t)) (term c))]
  [(eval-unop abs t c) ,(abs (term c))]
  [(eval-unop neg t c) ,(- (term c))]
  
  [(eval-unop sqrt t c)
   ,(if (negative? (term c))
        (match (term t)
          [`f32 (real->single-flonum +nan.f)]
          [`f64 +nan.0])
        (sqrt (term c)))]
  
  [(eval-unop ceil t c) ,(ceiling (term c))]
  [(eval-unop floor t c) ,(floor (term c))]
  [(eval-unop nearest t c) ,(round (term c))])


(define-metafunction WASMrt
  eval-binop : binop t c c -> (c ...)
  
  [(eval-binop add inn c_1 c_2) (,(sized-add (type-width (term inn)) (term c_1) (term c_2)))]
  [(eval-binop sub inn c_1 c_2) (,(sized-sub (type-width (term inn)) (term c_1) (term c_2)))]
  [(eval-binop mul inn c_1 c_2) (,(sized-mul (type-width (term inn)) (term c_1) (term c_2)))]
  
  [(eval-binop div-s inn c_1 c_2)
   (,(sized-signed-div (type-width (term inn)) (term c_1) (term c_2)))
   (side-condition (not (equal? (term c_2) 0)))
   or
   ()]
  
  [(eval-binop div-u inn c_1 c_2)
   (,(sized-unsigned-div (type-width (term inn)) (term c_1) (term c_2)))
   (side-condition (not (equal? (term c_2) 0)))
   or
   ()]
  
  [(eval-binop rem-s inn c_1 c_2)
   (,(sized-signed-rem (type-width (term inn)) (term c_1) (term c_2)))
   (side-condition (not (equal? (term c_2) 0)))
   or
   ()]
  
  [(eval-binop rem-u inn c_1 c_2)
   (,(sized-unsigned-rem (type-width (term inn)) (term c_1) (term c_2)))
   (side-condition (not (equal? (term c_2) 0)))
   or
   ()]
  
  [(eval-binop and inn c_1 c_2) (,(bitwise-and (term c_1) (term c_2)))]
  [(eval-binop or inn c_1 c_2) (,(bitwise-ior (term c_1) (term c_2)))]
  [(eval-binop xor inn c_1 c_2) (,(bitwise-xor (term c_1) (term c_2)))]
  [(eval-binop shl inn c_1 c_2) (,(sized-shl (type-width (term inn)) (term c_1) (term c_2)))]
  [(eval-binop shr-s inn c_1 c_2) (,(sized-signed-shr (type-width (term inn)) (term c_1) (term c_2)))]
  [(eval-binop shr-u inn c_1 c_2) (,(sized-unsigned-shr (type-width (term inn)) (term c_1) (term c_2)))]
  [(eval-binop rotl inn c_1 c_2) (,(sized-rotl (type-width (term inn)) (term c_1) (term c_2)))]
  [(eval-binop rotr inn c_1 c_2) (,(sized-rotr (type-width (term inn)) (term c_1) (term c_2)))]
  
  [(eval-binop add fnn c_1 c_2) (,(+ (term c_1) (term c_2)))]
  [(eval-binop sub fnn c_1 c_2) (,(- (term c_1) (term c_2)))]
  [(eval-binop mul fnn c_1 c_2) (,(* (term c_1) (term c_2)))]
  [(eval-binop div fnn c_1 c_2) (,(/ (term c_1) (term c_2)))]
  [(eval-binop min fnn c_1 c_2) (,(min (term c_1) (term c_2)))]
  [(eval-binop max fnn c_1 c_2) (,(max (term c_1) (term c_2)))]
  
  [(eval-binop copysign fnn c_1 c_2)
   (,(if (or (negative? (term c_2))
             (equal? (term c_2) -0.0)
             (equal? (term c_2) (real->single-flonum -0.0f0)))
         (- (abs (term c_1)))
         (abs (term c_1))))])


(define-metafunction WASMrt
  eval-testop : testop t c -> c
  [(eval-testop eqz t c)
   ,(if (= (term c) 0) 1 0)])


(define-metafunction WASMrt
  eval-relop : relop t c c -> c
  
  [(eval-relop eq t c_1 c_2) (bool ,(= (term c_1) (term c_2)))]
  [(eval-relop ne t c_1 c_2) (bool ,(not (= (term c_1) (term c_2))))]

  [(eval-relop lt-u t c_1 c_2) (bool ,(< (term c_1) (term c_2)))]
  [(eval-relop gt-u t c_1 c_2) (bool ,(> (term c_1) (term c_2)))]
  [(eval-relop le-u t c_1 c_2) (bool ,(<= (term c_1) (term c_2)))]
  [(eval-relop ge-u t c_1 c_2) (bool ,(>= (term c_1) (term c_2)))]
  
  [(eval-relop lt-s t c_1 c_2) (bool ,(< (term (signed t c_1)) (term (signed t c_2))))]
  [(eval-relop gt-s t c_1 c_2) (bool ,(> (term (signed t c_1)) (term (signed t c_2))))]
  [(eval-relop le-s t c_1 c_2) (bool ,(<= (term (signed t c_1)) (term (signed t c_2))))]
  [(eval-relop ge-s t c_1 c_2) (bool ,(>= (term (signed t c_1)) (term (signed t c_2))))]
  
  [(eval-relop lt t c_1 c_2) (bool ,(< (term c_1) (term c_2)))]
  [(eval-relop gt t c_1 c_2) (bool ,(> (term c_1) (term c_2)))]
  [(eval-relop le t c_1 c_2) (bool ,(<= (term c_1) (term c_2)))]
  [(eval-relop ge t c_1 c_2) (bool ,(>= (term c_1) (term c_2)))])


(define-metafunction WASMrt
  do-convert : t_1 t_2 any c -> (c ...)

  [(do-convert i64 i32 #f c) (,(to-unsigned-sized 32 (term c)))]
  [(do-convert i32 i64 signed c) (,(to-unsigned-sized 64 (to-signed-sized 32 (term c))))]
  [(do-convert i32 i64 unsigned c) (c)]
  
  [(do-convert f64 f32 #f c) (,(real->single-flonum (term c)))]
  [(do-convert f32 f64 #f c) (,(real->double-flonum (term c)))]

  [(do-convert inn f32 signed c) (,(real->single-flonum (to-signed-sized (type-width (term inn)) (term c))))]
  [(do-convert inn f32 unsigned c) (,(real->single-flonum (term c)))]
  
  [(do-convert inn f64 signed c) (,(real->double-flonum (to-signed-sized (type-width (term inn)) (term c))))]
  [(do-convert inn f64 unsigned c) (,(real->double-flonum (term c)))]

  [(do-convert fnn inn sx c)
   ()
   (side-condition (or (nan? (term c)) (infinite? (term c))))]

  [(do-convert fnn inn signed c)
   (,(to-unsigned-sized (type-width (term inn)) (inexact->exact (truncate (term c)))))
   (side-condition (< (sub1 (- (expt 2 (sub1 (type-width (term inn))))))
                      (truncate (term c))
                      (expt 2 (sub1 (type-width (term inn))))))
   or
   ()]
  
  [(do-convert fnn inn unsigned c)
   (,(inexact->exact (truncate (term c))))
   (side-condition (< -1 (truncate (term c)) (expt 2 (type-width (term inn)))))
   or
   ()])


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
  [(setup-call (v ...) (j (_ (func ((t ...) -> (t_2 ...)) (local (t_3 ...) (e ...))))) (e_2 ...))
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
  [(check-tf tf (j ((ex ...) (func tf (local (t ...) (e ...))))))
   (call (j ((ex ...) (func tf (local (t ...) (e ...))))))]
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
