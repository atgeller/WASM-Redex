#lang racket

(require racket/flonum
         redex/reduction-semantics
         "../Syntax.rkt"
         "../Utilities.rkt"
         "SizedOps.rkt"
         "ConstUtilities.rkt")

(provide (all-defined-out))

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