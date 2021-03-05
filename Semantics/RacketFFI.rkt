#lang racket

(require racket/flonum
         redex/reduction-semantics
         "SizedOps.rkt")

(provide racket-trampoline)

(define (racket-trampoline post proc s cs)
  (let-values ([(s_new cs_post) (apply proc s cs)])
    (unless (= (length cs_post) (length post))
      (error "Racket procedure produced an incorrect number of values"))
    (term (,s_new ,(map coerce-value post cs_post)))))

(define (coerce-value t n)
  (unless (real? n)
    (error "Racket procedure produced a non-real value"))
  (match t
    ['i32
     (unless (exact? n)
       (error "Racket procedure expected i32, but produced an inexact value"))
     (unless (<= (- (expt 2 31)) n (sub1 (expt 2 32)))
       (error "Racket procedure expected i32, but produced a value outside that range"))
     (to-unsigned-sized 32 n)]
    
    ['i64
     (unless (exact? n)
       (error "Racket procedure expected i64, but produced an inexact value"))
     (unless (<= (- (expt 2 63)) n (sub1 (expt 2 64)))
       (error "Racket procedure expected i64, but produced a value outside that range"))
     (to-unsigned-sized 64 n)]
    
    ['f32 (flsingle (real->double-flonum n))]
    
    ['f64 (real->double-flonum n)]))