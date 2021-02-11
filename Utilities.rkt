#lang racket

(require racket/flonum redex/reduction-semantics "Syntax.rkt" "MachineOps.rkt")

(provide (all-defined-out))

(define-syntax where/not
  (syntax-rules ()
   [(where/not x y) (match (term y)
                      [x #f]
                      [_ #t])]))

(define-metafunction WASMrt
  bit-width : t -> natural
  [(bit-width i32) 32]
  [(bit-width i64) 64]
  [(bit-width f32) 32]
  [(bit-width f64) 64])

(define-metafunction WASMrt
  packed-bit-width : tp -> natural
  [(packed-bit-width i8) 8]
  [(packed-bit-width i16) 16]
  [(packed-bit-width i32) 32])

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
  const->packed-bstr : t natural c -> bstr
  [(const->packed-bstr inn (name width natural) c)
   ,(integer->integer-bytes (modulo (term c) (expt 2 (term width))) (/ (term width) 8) #f #f)])

(define-metafunction WASMrt
  bstr->const : t bstr -> c
  [(bstr->const inn bstr) ,(integer-bytes->integer (term bstr) #f #f)]
  [(bstr->const f32 bstr) ,(real->single-flonum (floating-point-bytes->real (term bstr) #f))]
  [(bstr->const f64 bstr) ,(real->double-flonum (floating-point-bytes->real (term bstr) #f))])

(define-metafunction WASMrt
  packed-bstr->const : t sx bstr -> c
  [(packed-bstr->const inn signed bstr) ,(to-unsigned-sized (term (bit-width inn)) (integer-bytes->integer (term bstr) #t #f))]
  [(packed-bstr->const inn unsigned bstr) ,(integer-bytes->integer (term bstr) #f #f)])


(define-metafunction WASMrt
  eval-unop : unop t c -> c
  
  [(eval-unop clz t c) ,(sized-clz (term (bit-width t)) (term c))]
  [(eval-unop ctz t c) ,(sized-ctz (term (bit-width t)) (term c))]
  [(eval-unop popcnt t c) ,(sized-popcnt (term (bit-width t)) (term c))]
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
  
  [(eval-binop add inn c_1 c_2) (,(sized-add (term (bit-width inn)) (term c_1) (term c_2)))]
  [(eval-binop sub inn c_1 c_2) (,(sized-sub (term (bit-width inn)) (term c_1) (term c_2)))]
  [(eval-binop mul inn c_1 c_2) (,(sized-mul (term (bit-width inn)) (term c_1) (term c_2)))]
  
  [(eval-binop div-s inn c_1 c_2)
   (,(sized-signed-div (term (bit-width inn)) (term c_1) (term c_2)))
   (side-condition (not (equal? (term c_2) 0)))
   or
   ()]
  
  [(eval-binop div-u inn c_1 c_2)
   (,(sized-unsigned-div (term (bit-width inn)) (term c_1) (term c_2)))
   (side-condition (not (equal? (term c_2) 0)))
   or
   ()]
  
  [(eval-binop rem-s inn c_1 c_2)
   (,(sized-signed-rem (term (bit-width inn)) (term c_1) (term c_2)))
   (side-condition (not (equal? (term c_2) 0)))
   or
   ()]
  
  [(eval-binop rem-u inn c_1 c_2)
   (,(sized-unsigned-rem (term (bit-width inn)) (term c_1) (term c_2)))
   (side-condition (not (equal? (term c_2) 0)))
   or
   ()]
  
  [(eval-binop and inn c_1 c_2) (,(bitwise-and (term c_1) (term c_2)))]
  [(eval-binop or inn c_1 c_2) (,(bitwise-ior (term c_1) (term c_2)))]
  [(eval-binop xor inn c_1 c_2) (,(bitwise-xor (term c_1) (term c_2)))]
  [(eval-binop shl inn c_1 c_2) (,(sized-shl (term (bit-width inn)) (term c_1) (term c_2)))]
  [(eval-binop shr-s inn c_1 c_2) (,(sized-signed-shr (term (bit-width inn)) (term c_1) (term c_2)))]
  [(eval-binop shr-u inn c_1 c_2) (,(sized-unsigned-shr (term (bit-width inn)) (term c_1) (term c_2)))]
  [(eval-binop rotl inn c_1 c_2) (,(sized-rotl (term (bit-width inn)) (term c_1) (term c_2)))]
  [(eval-binop rotr inn c_1 c_2) (,(sized-rotr (term (bit-width inn)) (term c_1) (term c_2)))]
  
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
   (bool ,(= (term c) 0))])


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
  do-convert : t_1 t_2 (sx ...) c -> (c ...)

  [(do-convert i64 i32 () c) (,(to-unsigned-sized 32 (term c)))]
  [(do-convert i32 i64 (signed) c) (,(to-unsigned-sized 64 (to-signed-sized 32 (term c))))]
  [(do-convert i32 i64 (unsigned) c) (c)]
  
  [(do-convert f64 f32 () c) (,(real->single-flonum (term c)))]
  [(do-convert f32 f64 () c) (,(real->double-flonum (term c)))]

  [(do-convert inn f32 (signed) c) (,(real->single-flonum (to-signed-sized (term (bit-width inn)) (term c))))]
  [(do-convert inn f32 (unsigned) c) (,(real->single-flonum (term c)))]
  
  [(do-convert inn f64 (signed) c) (,(real->double-flonum (to-signed-sized (term (bit-width inn)) (term c))))]
  [(do-convert inn f64 (unsigned) c) (,(real->double-flonum (term c)))]

  [(do-convert fnn inn (sx) c)
   ()
   (side-condition (or (nan? (term c)) (infinite? (term c))))]

  [(do-convert fnn inn (signed) c)
   (,(to-unsigned-sized (term (bit-width inn)) (inexact->exact (truncate (term c)))))
   (side-condition (< (sub1 (- (expt 2 (sub1 (term (bit-width inn))))))
                      (truncate (term c))
                      (expt 2 (sub1 (term (bit-width inn))))))
   or
   ()]
  
  [(do-convert fnn inn (unsigned) c)
   (,(inexact->exact (truncate (term c))))
   (side-condition (< -1 (truncate (term c)) (expt 2 (term (bit-width inn)))))
   or
   ()])


;; Decompose local contexts
; Function to calculate local context depth
(define-metafunction WASMrt
  context-depth : L -> j
  [(context-depth hole) 0]
  [(context-depth (v ... (label n (e ...) L) e_2 ...)) ,(add1 (term (context-depth L)))])

;; TODO: Pretty much all the utils below here are kind of awkward and unwieldy in combination.
;; I'm certain the solution is either calling into Racket more often or less often,
;; but I'm not sure which

;; NOTE: Useful for the very popular myriad index-based lookups
(define-metafunction WASMrt
  do-get : (any ...) j -> any
  [(do-get (any ...) j)
   ,(list-ref (term (any ...)) (term j))])

;; NOTE: Useful for the very popular myriad index-based lookups
(define-metafunction WASMrt
  do-set : (any ...) j any -> (any ...)
  [(do-set (any_1 ...) j any_2)
   ,(append (take (term (any_1 ...)) (term j))
            (term (any_2))
            (drop (term (any_1 ...)) (add1 (term j))))])

;; closure accessors
(define-metafunction WASMrt
  cl-code : cl -> (func tf (local (t ...) (e ...)))
  [(cl-code (i (func tf (local (t ...) (e ...)))))
   (func tf (local (t ...) (e ...)))])

(define-metafunction WASMrt
  cl-code-opt : any -> any
  [(cl-code-opt (i (func tf (local (t ...) (e ...)))))
   (func tf (local (t ...) (e ...)))]
  [(cl-code-opt _) #f])

(define-metafunction WASMrt
  cl-inst : cl -> i
  [(cl-inst (i (func tf (local (t ...) (e ...)))))
   i])

;; same as s_func(i,j)
(define-metafunction WASMrt
  store-func : s i j -> cl
  [(store-func ((inst ...) _ _) i j)
   (inst-func (do-get (inst ...) i) j)])

(define-metafunction WASMrt
  inst-func : inst j -> cl
  [(inst-func ((cl ...) _ _ _) j)
   (do-get (cl ...) j)])

;; same as s_tab(i,j)
(define-metafunction WASMrt
  store-tab : s i j -> any
  [(store-tab ((inst ...) (tabinst ...) _) i j)
   (tab-func (do-get (tabinst ...) (inst-tab (do-get (inst ...) i))) j)])

(define-metafunction WASMrt
  tab-func : tabinst j -> any
  [(tab-func (cl ...) j)
   (do-get (cl ...) j)
   (side-condition (< (term j) (length (term (cl ...)))))
   or
   #f])

(define-metafunction WASMrt
  inst-tab : inst -> i
  [(inst-tab (_ _ (i) _)) i])

(define-metafunction WASMrt
  inst-mem : inst -> i
  [(inst-mem (_ _ _ (i))) i])

(define-metafunction WASMrt
  inst-glob : inst j -> v
  [(inst-glob (_ (v ...) _ _) j)
   (do-get (v ...) j)])

(define-metafunction WASMrt
  inst-with-glob : inst j v -> inst
  [(inst-with-glob ((cl ...) (v_g ...) (i_t ...) (i_m ...)) j v)
   ((cl ...) (do-set (v_g ...) j v) (i_t ...) (i_m ...))])

(define-metafunction WASMrt
  store-glob : s i j -> v
  [(store-glob ((inst ...) _ _) i j)
   (inst-glob (do-get (inst ...) i) j)])

(define-metafunction WASMrt
  store-with-glob : s i j v -> s
  [(store-with-glob ((inst ...) (tabinst ...) (meminst ...)) i j v)
   ((do-set (inst ...) i (inst-with-glob (do-get (inst ...) i) j v)) (tabinst ...) (meminst ...))])

(define-metafunction WASMrt
  store-mem : s i -> meminst
  [(store-mem ((inst ...) _ (meminst ...)) i)
   (do-get (meminst ...) (inst-mem (do-get (inst ...) i)))])

(define-metafunction WASMrt
  store-with-mem : s i meminst -> s
  [(store-with-mem ((inst ...) (tabinst ...) (meminst ...)) i meminst_new)
   ((inst ...) (tabinst ...) (do-set (meminst ...) (inst-mem (do-get (inst ...) i)) meminst_new))])

(define-metafunction WASMrt
  mem-bytes : meminst natural natural -> (bstr ...)
  [(mem-bytes meminst (name offset natural_1) (name width natural_2))
   (,(subbytes (term meminst) (term offset) (+ (term offset) (/ (term width) 8))))
   (side-condition (<= (+ (term offset) (/ (term width) 8)) (bytes-length (term meminst))))
   or
   ()])

(define-metafunction WASMrt
  mem-with-bytes : meminst natural bstr -> (meminst ...)
  [(mem-with-bytes meminst (name offset natural) bstr)
   (,(bytes-append (subbytes (term meminst) 0 (term offset))
                   (term bstr)
                   (subbytes (term meminst) (+ (term offset) (bytes-length (term bstr))))))
   (side-condition (<= (+ (term offset) (bytes-length (term bstr))) (bytes-length (term meminst))))
   or
   ()])
