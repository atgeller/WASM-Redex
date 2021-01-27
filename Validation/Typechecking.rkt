#lang racket

(require redex/reduction-semantics
         "../Syntax.rkt"
         "../Bits.rkt"
         "ModuleTyping.rkt"
         "InstructionTyping.rkt"
         "Utilities.rkt")

;; I'm doing this all using Racket's match instead of Redexes term-match
;; because I like the syntax of Racket's match better.

;; mod -> derivation of ⊢-module or #f
(define (typecheck-module module)
  (let ([C (term (extract-module-type module))])
    (match module
      [`(module ,fs ,globs (,tab) (,mem))
       (let ([tab-deriv? (typecheck-table C tab)]
             [mem-deriv (build-memory-derivation C mem)]
             [globs-deriv? (typecheck-globals globs)]
             [funcs-deriv? (typecheck-funcs C fs)])
         (if (and tab-deriv? globs-deriv? funcs-deriv?)
             (derivation `(⊢-module ,module)
                         #f (list tab-deriv? mem-deriv funcs-deriv? globs-deriv?))
             #f))]
      [`(module ,fs ,globs (,tab) ())
       (let ([tab-deriv? (typecheck-table C tab)]
             [globs-deriv? (typecheck-globals globs)]
             [funcs-deriv? (typecheck-funcs C fs)])
         (if (and tab-deriv? globs-deriv? funcs-deriv?)
             (derivation `(⊢-module ,module)
                         #f (list tab-deriv? funcs-deriv? globs-deriv?))
             #f))]
      [`(module ,fs ,globs () (,mem))
       (let ([mem-deriv (build-memory-derivation C mem)]
             [globs-deriv? (typecheck-globals globs)]
             [funcs-deriv? (typecheck-funcs C fs)])
         (if (and globs-deriv? funcs-deriv?)
             (derivation `(⊢-module ,module)
                         #f (list mem-deriv funcs-deriv? globs-deriv?))
             #f))]
      [`(module ,fs ,globs () ())
       (let ([globs-deriv? (typecheck-globals globs)]
             [funcs-deriv? (typecheck-funcs C fs)])
         (if (and globs-deriv? funcs-deriv?)
             (derivation `(⊢-module ,module)
                         #f (list funcs-deriv? globs-deriv?))
             #f))])))

;; C tab -> derivation of ⊢-module-table or #f
(define (typecheck-table C tab)
  (match tab
    [`(table ,exs ,n (import ,_ ,_))
     (derivation `(⊢-module-table ,C ,tab (,exs ,n)))]
    [`(table ,exs ,n ,indexes)
     (if (term (valid-indexes ,C ,indexes ,n))
          (derivation `(⊢-module-table ,C ,tab (,exs ,n)))
          #f)]))

;; C mem -> derivation of ⊢-module-mem
(define (build-memory-derivation C mem)
  (match mem
    [`(memory ,exs ,n) (derivation `(⊢-module-memory ,C ,mem (,exs ,n)) #f (list))]
    [`(memory ,exs ,n ,im) (derivation `(⊢-module-memory ,C ,mem (,exs ,n)) #f (list))]))

;; (listof glob) -> derivation of ⊢-module-global-list or #f
(define (typecheck-globals globs)
  (match globs
    [`() (derivation `(⊢-module-global-list () ()) #f (list))]
    [`(,rest-globs .. ,glob)
     (let ([rest-deriv? (typecheck-globals rest-globs)])
       (match rest-deriv?
         [(derivation `(⊢-module-global-list ,_ ((,exss ,tgs) ...)) _ _)
          (let ([glob-deriv? (typecheck-global `((func ())
                                                 (global ,tgs)
                                                 (table)
                                                 (memory)
                                                 (local ())
                                                 (label ())
                                                 (return))
                                               glob)]
                [rest-types (map list exss tgs)])
            (match glob-deriv?
              [(derivation `(⊢-module-global ,_ ,_ (,exs ,tg)) _ _)
               (derivation `(⊢-module-global-list ,globs (,@rest-types ... (,exs ,tg)))
                           #f (list rest-deriv? glob-deriv?))]
              [#f #f]))]
         [#f #f]))]))

;; C glob -> derivation of ⊢-module-global or #f
(define (typecheck-global C glob)
  (match glob
    [`(global ,exs (,mut? ,t) ,es)
     (if (or (empty? exs) (not mut?))
         (let ([ins-deriv? (typecheck-ins C es)])
           (match ins-deriv?
             [(derivation `(⊢ ,_ ,_ (,_ -> (((,t_1 ,_)) ,_ ,_ ,_))) _ _)
              (if (equal? t t_1)
                  (derivation `(⊢-module-global ,C ,glob (,exs (,mut? ,t)))
                              #f (list ins-deriv?))
                  #f)]
             [#f #f]))
         #f)]
    [`(global ,exs (#f ,t) im)
     (derivation `(⊢-module-global ,C ,glob (,exs (#f ,t))) #f (list))]))

;; C (listof f) -> derivation of ⊢-module-func-list or #f
(define (typecheck-funcs C funcs)
  (match funcs
    [`() (derivation `(⊢-module-func-list ,C () ()) #f (list))]
    [`(,func ,rest-funcs ...)
     (let ([func-deriv? (typecheck-func C func)]
           [rest-deriv? (typecheck-funcs C rest-funcs)])
       (match func-deriv?
         [(derivation `(⊢-module-func ,_ ,_ (,exs ,tf)) _ _)
          (match rest-deriv?
            [(derivation `(⊢-module-func-list ,_ ,_ (,rest-types ...)) _ _)
             (derivation `(⊢-module-func-list ,C ,funcs ((,exs ,tf) ,@rest-types))
                         #f (list func-deriv? rest-deriv?))]
            [#f #f])]
         [#f #f]))]))

;; C f -> derivation of ⊢-module-func or #f
(define (typecheck-func C func)
  (match func
    [`(func ,exs (,t_1 -> ,t_2) (local (,tl ...) ,ins))
     (match-let ([`(,funcs ,globs ,tables ,memories ,_ ,_ ,_) C])
       (let* ([C2 `(,funcs ,globs ,tables ,memories (local (,@t_1 ,@tl)) (label (,t_2)) (return ,t_2))]
              [ins-deriv? (typecheck-ins C2 ins `() t_2)])
         (if ins-deriv?
             (derivation `(⊢-module-func ,C ,func (,exs (,t_1 -> ,t_2)))
                         #f
                         (list ins-deriv?))
             #f)))]
    [`(func ,exs ,tf (import ,_ ,_))
     (derivation `(⊢-module-func ,C ,func (,exs ,tf)) #f (list))]))

;; C (listof e) (listof t) (listof t) -> (listof t) or #f
;; synthesizes stacks for the instruction sequence or #f upon failure
;; this does not need to be done by any WASM implementations
;; since normally you only need to know if a typing derivation exists, not find one.
;; This function only checks if there's a plausible stack for the instruction sequence,
;; it doesn't check the premises of the instructions.
(define (synthesize-stacks C ins pre post)

  (define (find-unify stack ts)
    (foldl (λ (s t subs)
             (if (redex-match? WASM t s)
                 (if (equal? s t)
                     subs
                     #f)
                 ;; Note: assumes that a type variable cannot be duplicated on the stack getting consumed
                 (cons (cons s t) subs)))
           '()
           stack
           ts))

  (define (apply-subs subs stacks)
    (map (λ (stack)
           (cons (first stack)
                 (map (λ (s)
                        (if (redex-match? WASM t s)
                            s
                            (let ([sub (assoc s subs)])
                              (if sub
                                  (cdr sub)
                                  s))))
                      (rest stack))))
         stacks))

  (define (consume stacks ts)
    (let ([pre-stack (first stacks)])
      (if (>= (length (rest pre-stack)) (length ts))
          (let ([subs (find-unify (take-right pre-stack (length ts)) ts)])
            (if subs
                ;; Note: also assumes type variables are not duplicated on the stack
                (values (drop-right pre-stack (length ts)) (apply-subs subs stacks))
                (values #f stacks)))
          (match (first pre-stack)
            [#f (values #f stacks)]
            [(? natural? n)
             (let ([subs (find-unify (rest pre-stack) (take-right ts (length (rest pre-stack))))])
               (if subs
                   (let ([new-stacks (apply-subs subs stacks)])
                     (let-values ([(start end) (split-at-right new-stacks n)]
                                  [(from-ellipses) (drop-right ts (length (rest (first new-stacks))))])
                       (values (list n)
                               (append (map (λ (stack)
                                              (cons n (append from-ellipses (rest stack))))
                                            start)
                                       end))))
                   (values #f stacks)))]))))

  (define (apply-pre-post stacks pre post)
    (match/values (consume stacks pre)
      [(#f _) #f]
      [(mid new-stacks)
       (cons (append mid post) new-stacks)]))

  (define (apply-e stacks e)
    (match e
      [`(,t const ,_)
       (cons (append (first stacks) (list t)) stacks)]

      [`(,t ,(? (redex-match? WASM unop) _))
       (apply-pre-post stacks (list t) (list t))]

      [`(,t ,(? (redex-match? WASM binop) _))
       (apply-pre-post stacks (list t t) (list t))]

      [`(,t ,(? (redex-match? WASM testop) _))
       (apply-pre-post stacks (list t) (list 'i32))]

      [`(,t ,(? (redex-match? WASM relop) _))
       (apply-pre-post stacks (list t t) (list 'i32))]

      [`(,t1 ,(? (redex-match? WASM cvtop) _) ,t2)
       (apply-pre-post stacks (list t2) (list t1))]

      [`(,t1 ,(? (redex-match? WASM cvtop) _) ,t2 ,_)
       (apply-pre-post stacks (list t2) (list t1))]

      [`(unreachable)
       (cons (list (length stacks)) stacks)]

      [`(nop)
       (cons (first stacks) stacks)]

      [`(drop)
       (if (>= (length (rest (first stacks))) 1)
           (cons (drop-right (first stacks) 1) stacks)
           (if (false? (first (first stacks)))
               #f
               (apply-pre-post stacks (list 'i32) (list))))]

      [`(select)
       (if (>= (length (rest (first stacks))) 3)
           (let ([t (last (drop-right (first stacks) 1))])
             (if (and (equal? (last (first stacks)) 'i32)
                      (equal? (last (drop-right (first stacks) 2)) t))
                 (apply-pre-post stacks (list t t 'i32) (list t))
                 #f))
           (if (false? (first (first stacks)))
               #f
               (if (< (length (rest (first stacks))) 2)
                   (let ([tv (gensym)])
                     (apply-pre-post stacks (list tv tv 'i32) (list tv)))
                   (let ([t (last (drop-right (first stacks) 1))])
                     (apply-pre-post stacks (list t t 'i32) (list t))))))]

      [`(block (,b-pre -> ,b-post) ,_)
       (apply-pre-post stacks b-pre b-post)]

      [`(loop (,l-pre -> ,l-post) ,_)
       (apply-pre-post stacks l-pre l-post)]

      [`(if (,i-pre -> ,i-post) ,_ else ,_)
       (apply-pre-post stacks (append i-pre (list 'i32)) i-post)]

      [`(br ,i)
       (match/values (consume stacks (term (get-label ,C ,i)))
         [(#f _) #f]
         [(_ new-stacks) (cons (list (length new-stacks)) new-stacks)])]

      [`(br-if ,i)
       (let ([ts (term (get-label ,C ,i))])
         (apply-pre-post stacks (append ts (list 'i32)) ts))]

      [`(br-table ,i ...)
       (if (judgment-holds (label-types (get-labels ,C) ,i (t ...)))
           (match/values (consume stacks (term (get-label ,C ,(first i))))
             [(#f _) #f]
             [(_ new-stacks) (cons (list (length new-stacks)) new-stacks)])
           #f)]

      ;; TODO: rest of instructions

      [`(current-memory)
       (apply-pre-post stacks (list) (list 'i32))]

      [`(grow-memory)
       (apply-pre-post stacks (list 'i32) (list 'i32))]))
  
  (define (synthesize-stacks-rec stacks ins)
    (if (empty? ins)
        (match/values (consume stacks post)
          [(#f _) #f]
          [(_ new-stacks) new-stacks])
        (match (apply-e stacks (first ins))
          [#f #f]
          [new-stacks (synthesize-stacks-rec new-stacks (rest ins))])))

  (define (replace-vars stacks)
    (map (λ (stacks)
           (cons (first stacks)
                 (map (λ (s)
                        (if (redex-match? WASM t s)
                            s
                            'i32))
                      (rest stacks))))
         stacks))

  (match (synthesize-stacks-rec (list (cons #f pre)) ins)
    [#f #f]
    [new-stacks (reverse (map rest (replace-vars new-stacks)))]))

(define (typecheck-ins C ins pre post)

  (define (stack-polyize deriv e-pre e-post)
    (match-let ([(derivation `(⊢ ,d-C ,d-ins (,d-pre -> ,d-post)) _ _) deriv])
      (if (equal? e-pre d-pre)
          deriv
          (derivation `(⊢ ,d-C ,d-ins (,e-pre -> ,e-post)) #f (list deriv)))))

  ;; TODO: not a good name
  ;; assumes that e fits e-pre and e-post since output comes from synthesize-stacks
  ;; premises of the instructions are checked here
  (define (build-e-deriv e e-pre e-post)
    (match e
      [`(,t const ,_)
       (stack-polyize (derivation `(⊢ ,C (,e) (() -> (,t))) #f (list)) e-pre e-post)]
      
      [`(,t ,(? (redex-match? WASM unop) _))
       (stack-polyize (derivation `(⊢ ,C (,e) ((,t) -> (,t))) #f (list)) e-pre e-post)]

      [`(,t ,(? (redex-match? WASM binop) _))
       (stack-polyize (derivation `(⊢ ,C (,e) ((,t ,t) -> (,t))) #f (list)) e-pre e-post)]

      [`(,t ,(? (redex-match? WASM testop) _))
       (stack-polyize (derivation `(⊢ ,C (,e) ((,t) -> (i32))) #f (list)) e-pre e-post)]

      [`(,t ,(? (redex-match? WASM relop) _))
       (stack-polyize (derivation `(⊢ ,C (,e) ((,t ,t) -> (i32))) #f (list)) e-pre e-post)]

      [`(,t1 convert ,t2)
       (if (and (not (equal? t1 t2))
                (or (and (integer-type? t1) (integer-type? t2) (< (type-width t1) (type-width t2)))
                    (and (floating-type? t1) (floating-type? t2))))
           (stack-polyize (derivation `(⊢ ,C (,e) ((,t2) -> (,t1))) #f (list)) e-pre e-post)
           #f)]

      [`(,t1 convert ,t2 _)
       (if (and (not (equal? t1 t2))
                (nor (and (integer-type? t1) (integer-type? t2) (< (type-width t1) (type-width t2)))
                     (and (floating-type? t1) (floating-type? t2))))
           (stack-polyize (derivation `(⊢ ,C (,e) ((,t2) -> (,t1))) #f (list)) e-pre e-post)
           #f)]

      [`(,t1 reinterpret ,t2)
       (if (and (not (equal? t1 t2)) (= (type-width t1) (type-width t2)))
           (stack-polyize (derivation `(⊢ ,C (,e) ((,t2) -> (,t1))) #f (list)) e-pre e-post)
           #f)]

      [`(unreachable)
       (derivation `(⊢ ,C ((unreachable)) (,e-pre -> ,e-post)) #f (list))]

      [`(nop)
       (stack-polyize (derivation `(⊢ ,C ((nop)) (() -> ()) #f (list))) e-pre e-post)]

      [`(drop)
       (stack-polyize (derivation `(⊢ ,C ((drop)) ((,(first e-pre)) -> ())) #f (list))
                      e-pre e-post)]

      [`(select)
       (stack-polyize (derivation `(⊢ ,C ((select)) (,(take-right e-pre 3) -> (,(second e-pre))))
                                  #f
                                  (list))
                      e-pre e-post)]

      [`(block (,b-pre -> ,b-post) ,b-ins)
       (match (typecheck-ins (term (in-label ,C ,b-post)) b-ins b-pre b-post)
         [#f #f]
         [b-deriv (stack-polyize (derivation `(⊢ ,C (,e) (,b-pre -> ,b-post)) #f (list b-deriv))
                                 e-pre e-post)])]

      [`(loop (,l-pre -> ,l-post) ,l-ins)
       (match (typecheck-ins (term (in-label ,C ,l-pre)) l-ins l-pre l-post)
         [#f #f]
         [l-deriv (stack-polyize (derivation `(⊢ ,C (,e) (,l-pre -> l-post)) #f (list l-deriv))
                                 e-pre e-post)])]

      [`(if (,i-pre -> ,i-post) ,then-ins else ,else-ins)
       (match (typecheck-ins (term (in-label ,C ,i-post)) then-ins i-pre i-post)
         [#f #f]
         [then-deriv
          (match (typecheck-ins (term (in-label ,C ,i-post)) else-ins i-pre i-post)
            [#f #f]
            [else-deriv
             (stack-polyize (derivation `(⊢ ,C (,e) (,(append i-pre (list 'i32)) -> ,i-post))
                                        #f
                                        (list then-deriv else-deriv))
                            e-pre e-post)])])]

      [`(br ,i)
       ;; if synthesize-stacks succeeded then the stack types br
       (derivation `(⊢ ,C (,e) (,e-pre -> ,e-post)) #f (list))]

      [`(br-if ,i)
       (let ([ts (term (get-label ,C ,i))])
         (stack-polyize (derivation `(⊢ ,C (,e) (,(append ts (list 'i32)) -> ,ts)) #f (list))
                        e-pre e-post))]

      [`(br-table ,i ...)
       ;; if synthesize-stacks succeeded then the stack types br-table
       (derivation `(⊢ ,C (,e) (,e-pre -> ,e-post)) #f (list))]

      ;; TODO: rest of the instructions

      [`(current-memory)
       (match-let ([`(,_ ,_ ,_ (memory ,j ...) ,_ ,_ ,_) C])
         (if (not (empty? j))
             (stack-polyize (derivation `(⊢ ,C ((current-memory)) (() -> (i32))) #f (list)) e-pre e-post)
             #f))]

      [`(grow-memory)
       (match-let ([`(,_ ,_ ,_ (memory ,j ...) ,_ ,_ ,_) C])
         (if (not (empty? j))
             (stack-polyize (derivation `(⊢ ,C ((current-memory)) ((i32) -> (i32))) #f (list)) e-pre e-post)
             #f))]

      [_ #f]))

  (define (build-ins-deriv ins-rev stacks)
    (if (= (length ins-rev) 1)
        (build-e-deriv (first ins-rev) (second stacks) (first stacks))
        (match (build-e-deriv (first ins-rev) (second stacks) (first stacks))
          [#f #f]
          [e-deriv
           (match (build-ins-deriv (rest ins-rev) (rest stacks))
             [#f #f]
             [es-deriv
              (derivation `(⊢ ,C ,(reverse ins-rev) (,(last stacks) -> ,(first stacks)))
                          #f
                          (list es-deriv e-deriv))])])))

  (if (empty? ins)
      (cond
        [(and (empty? pre) (empty? post))
         (derivation `(⊢ ,C () (() -> ())) #f (list))]
        [(equal? pre post)
         (derivation `(⊢ ,C () (,pre -> ,post))
                     #f
                     (list (derivation `(⊢ ,C () (() -> ())) #f (list))))]
        [else #f])
      (match (synthesize-stacks C ins pre post)
        [#f #f]
        [stacks
         (build-ins-deriv (reverse ins) (reverse stacks))])))