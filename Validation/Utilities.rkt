#lang racket

(require redex/reduction-semantics
         "../Syntax.rkt")

(provide (all-defined-out))

(define-extended-language WASMTyping WASM  
  (C ::= ((func (tf ...)) (global (tg ...)) (table j ...) (memory j ...) (local (t ...)) (label ((t ...) ...)) (return (t ...)))
     ((func (tf ...)) (global (tg ...)) (table j ...) (memory j ...) (local (t ...)) (label ((t ...) ...)) (return)))

  (S ::= ((C ...) (j ...) (j ...))))

(define-metafunction WASMTyping
  reverse-get : (any ...) j -> any
  [(reverse-get (any ... any_1) j)
   (reverse-get (any ...) ,(sub1 (term j)))
   (side-condition (< 0 (term j)))]
  [(reverse-get (any ... any_1) 0) any_1])

(define-metafunction WASMTyping
  in-label : C (t ...) -> C
  [(in-label ((func (tf ...)) (global (tg ...)) (table j_1 ...) (memory j_2 ...) (local (t_1 ...)) (label ((t_2 ...) ...)) (return (t_3 ...))) (t ...))
   ((func (tf ...)) (global (tg ...)) (table j_1 ...) (memory j_2 ...) (local (t_1 ...)) (label ((t_2 ...) ... (t ...))) (return (t_3 ...)))]
  [(in-label ((func (tf ...)) (global (tg ...)) (table j_1 ...) (memory j_2 ...) (local (t_1 ...)) (label ((t_2 ...) ...)) (return)) (t ...))
   ((func (tf ...)) (global (tg ...)) (table j_1 ...) (memory j_2 ...) (local (t_1 ...)) (label ((t_2 ...) ... (t ...))) (return))])

(define-judgment-form WASMTyping
  #:contract (label-types ((t ...) ...) (j ...) (t ...))
  #:mode (label-types I I O)

  [(where (t_2 ...) (reverse-get ((t ...) ...) j))
   -----------------------------------------------
   (label-types ((t ...) ...) (j) (t_2 ...))]

  [(where (t_2 ...) (reverse-get ((t ...) ...) j))
   (label-types ((t ...) ...) (j_2 ...) (t_2 ...))
   -----------------------------------
   (label-types ((t ...) ...) (j j_2 ...) (t_2 ...))])
