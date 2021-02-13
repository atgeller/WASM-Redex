#lang racket

(require redex/reduction-semantics
         "../Syntax.rkt"
         "ModuleTyping.rkt"
         "InstructionTyping.rkt"
         "Utilities.rkt")

(provide (all-defined-out))

(define (type-width t)
  (match t
    ['i8 8]
    ['i16 16]
    ['i32 32]
    ['i64 64]
    ['f32 32]
    ['f64 64]))

(define (integer-type? t)
  (match t
    ['i32 #t]
    ['i64 #t]
    ['f32 #f]
    ['f64 #f]))

(define (floating-type? t)
  (match t
    ['i32 #f]
    ['i64 #f]
    ['f32 #t]
    ['f64 #t]))

;; I'm doing this all using Racket's match instead of Redexes term-match
;; because I like the syntax of Racket's match better.

;; mod -> derivation of ⊢-module or #f
(define (typecheck-module module)
  (let* ([C (term (extract-module-type ,module))]
         [glob-Cs (term (global-contexts (context-globals ,C)))])
    (match module
      [`(module ,fs ,globs ,tabs ,mems)
       (let ([fs-derivs (map (curry typecheck-func C) fs)]
             [globs-derivs (map typecheck-global glob-Cs globs)]
             [tabs-derivs (map (curry typecheck-table C) tabs)]
             [mems-derivs (map (curry memory-derivation C mems))])
         (if (and (andmap identity fs-derivs)
                  (andmap identity globs-derivs)
                  (andmap identity tabs-derivs))
             ;; TODO check for export distinction
             (derivation `(⊢-module ,module)
                         #f (append fs-derivs
                                    globs-derivs
                                    tabs-derivs
                                    mems-derivs))
             #f))])))

;; C tab -> derivation of ⊢-module-table or #f
(define (typecheck-table C tab)
  (match tab
    [`(,exs (table ,n (import ,_ ,_)))
     (derivation `(⊢-module-table ,C ,tab (,exs ,n)) #f (list))]
    [`(,exs (table ,n ,indexes ...))
     (if (and (= n (length indexes))
              (andmap (curry > (length (term (context-funcs C)))) indexes))
         (derivation `(⊢-module-table ,C ,tab (,exs ,n)) #f (list))
         #f)]))

;; C mem -> derivation of ⊢-module-mem
(define (memory-derivation C mem)
  (match mem
    [`(,exs (memory ,n)) (derivation `(⊢-module-memory ,C ,mem (,exs ,n)) #f (list))]
    [`(,exs (memory ,n ,im)) (derivation `(⊢-module-memory ,C ,mem (,exs ,n)) #f (list))]))

;; C glob -> derivation of ⊢-module-global or #f
(define (typecheck-global C glob)
  (match glob
    [`(,exs (global (,mut ,t) ,es))
     (if (or (empty? exs) (equal? mut 'const))
         (match (typecheck-ins C es `() `(,t))
           [#f #f]
           [ins-deriv
            (derivation `(⊢-module-global ,C ,glob (,exs (,mut ,t)))
                        #f
                        (list ins-deriv))])
         #f)]
    [`(,exs (global (,mut ,t) im))
     (if (equal? mut 'const)
         (derivation `(⊢-module-global ,C ,glob (,exs (const ,t))) #f (list))
         #f)]))

;; C f -> derivation of ⊢-module-func or #f
(define (typecheck-func C func)
  (match func
    [`(,exs (func (,t_1 -> ,t_2) (local (,tl ...) ,ins)))
     (match-let ([`(,funcs ,globs ,tables ,memories ,_ ,_ ,_) C])
       (let* ([C2 `(,funcs ,globs ,tables ,memories (local (,@t_1 ,@tl)) (label (,t_2)) (return ,t_2))]
              [ins-deriv? (typecheck-ins C2 ins `() t_2)])
         (if ins-deriv?
             (derivation `(⊢-module-func ,C ,func (,exs (,t_1 -> ,t_2)))
                         #f
                         (list ins-deriv?))
             #f)))]
    [`(,exs (func ,tf (import ,_ ,_)))
     (derivation `(⊢-module-func ,C ,func (,exs ,tf)) #f (list))]))

;; C (listof e) (listof t) (listof t) -> (listof t) or #f
;; synthesizes stacks for the instruction sequence or #f upon failure
;; this does not need to be done by any WASM implementations
;; since normally you only need to know if a typing derivation exists, not find one.
;; This function only checks if there's a plausible stack for the instruction sequence,
;; it doesn't check the premises of the instructions.
(define (synthesize-stacks C ins pre post)

  ;; stack is (cons (or/c false? natural?) (listof t))

  (define (find-unify stack ts)
    (foldl (λ (s t subs)
             (cond
               ;; s and t are both types
               [(and (redex-match? WASM t s) (redex-match? WASM t t))
                (if (equal? s t)
                    subs
                    #f)]

               ;; s is a type, t is a type variable
               [(and (redex-match? WASM t s) (not (redex-match? WASM t t)))
                (match (assoc t subs)
                  [#f (cons (cons t s) subs)]
                  [sub (if (equal? (cdr sub) s)
                           subs
                           #f)])]

               ;; s is a type variable, t is a type
               [(and (not (redex-match? WASM t s)) (redex-match? WASM t t))
                (match (assoc t subs)
                  [#f (cons (cons s t) subs)]
                  [sub (if (equal? (cdr sub) t)
                           subs
                           #f)])]

               ;; s and t are both type variables
               [(nor (redex-match? WASM t s) (redex-match? WASM t t))
                (match/values (values (assoc t subs) (assoc s subs))
                  [(#f #f) (cons (cons s t) subs)]
                  [(#f t-sub) (if (equal? s (cdr t-sub))
                                  subs
                                  #f)]
                  [(s-sub #f) (if (equal? t (cdr s-sub))
                                  subs
                                  #f)]
                  [(s-sub t-sub) (if (equal? (cdr s-sub) (cdr t-sub))
                                     subs
                                     #f)])]))
           '()
           stack
           ts))

  (define (apply-subs subs lst)
    (map (λ (s)
           (if (redex-match? WASM t s)
               s
               (match (assoc s subs)
                 [#f s]
                 [sub (cdr sub)])))
         lst))

  (define (apply-subs-stack subs stack)
    (cons (first stack) (apply-subs subs (rest stack))))

  (define (consume stacks ts)
    (let ([pre-stack (first stacks)])
      (if (>= (length (rest pre-stack)) (length ts))
          (match (find-unify (take-right pre-stack (length ts)) ts)
            [#f (values #f (void) (void))]
            [subs (values (apply-subs-stack subs (drop-right pre-stack (length ts)))
                          (map (curry apply-subs-stack subs) stacks)
                          subs)])
          (match (first pre-stack)
            [#f (values #f (void) (void))]
            [(? natural? n)
             (match (find-unify (rest pre-stack) (take-right ts (length (rest pre-stack))))
               [#f (values #f (void) (void))]
               [subs (let* ([new-stacks (map (curry apply-subs-stack subs) stacks)]
                            [from-ellipses (apply-subs subs (drop-right ts (length (rest (first new-stacks)))))])
                       (let-values ([(start end) (split-at-right new-stacks n)])
                         (values (list n)
                                 (append (map (λ (stack)
                                                (cons n (append from-ellipses (rest stack))))
                                              start)
                                         end)
                                 subs)))])]))))

  (define (apply-pre-post stacks pre post)
    (match/values (consume stacks pre)
      [(#f _ _) #f]
      [(mid new-stacks subs)
       (cons (append mid (apply-subs subs post)) new-stacks)]))

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

      [`unreachable
       (cons (list (length stacks)) stacks)]

      [`nop
       (cons (first stacks) stacks)]

      [`drop
       (apply-pre-post stacks (list (gensym)) (list))]

      [`select
       (let ([tv (gensym)])
         (apply-pre-post stacks (list tv tv 'i32) (list tv)))]

      [`(block (,b-pre -> ,b-post) ,_)
       (apply-pre-post stacks b-pre b-post)]

      [`(loop (,l-pre -> ,l-post) ,_)
       (apply-pre-post stacks l-pre l-post)]

      [`(if (,i-pre -> ,i-post) ,_ else ,_)
       (apply-pre-post stacks (append i-pre (list 'i32)) i-post)]

      [`(br ,i)
       (match-let ([`(,_ ,_ ,_ ,_ ,_ (label ,labels ...) ,_) C])
         (if (< i (length labels))
             (match/values (consume stacks (list-ref-right labels i))
               [(#f _ _) #f]
               [(_ new-stacks _) (cons (list (length new-stacks)) new-stacks)])
             #f))]

      [`(br-if ,i)
       (match-let ([`(,_ ,_ ,_ ,_ ,_ (label ,labels ...) ,_) C])
         (if (< i (length labels))
             (let ([ts (list-ref-right labels i)])
               (apply-pre-post stacks (append ts (list 'i32)) ts))
             #f))]

      [`(br-table ,i ,is ...)
       (match-let ([`(,_ ,_ ,_ ,_ ,_ (label ,labels ...) ,_) C])
         (if (and (< i (length labels))
                  (andmap (curry > (length labels)) is)
                  (andmap (curry equal? (list-ref-right labels i))
                          (map (curry list-ref-right labels) is)))
             (match/values (consume stacks (append (list-ref-right labels i) (list 'i32)))
                           [(#f _ _) #f]
                           [(_ new-stacks _) (cons (list (length new-stacks)) new-stacks)])
             #f))]

      [`return
       (match C
         [`(,_ ,_ ,_ ,_ ,_ ,_ (return)) #f]
         [`(,_ ,_ ,_ ,_ ,_ ,_ (return ,ts))
          (match/values (consume stacks ts)
            [(#f _ _) #f]
            [(_ new-stacks _) (cons (list (length new-stacks)) new-stacks)])])]

      [`(call ,i)
       (match-let ([`((func ,tfs ...) ,_ ,_ ,_ ,_ ,_ ,_) C])
         (if (< i (length tfs))
             (match-let ([`(,c-pre -> ,c-post) (list-ref tfs i)])
               (apply-pre-post stacks c-pre c-post))
             #f))]

      [`(call-indirect (,c-pre -> ,c-post))
       (apply-pre-post stacks (append c-pre (list 'i32)) c-post)]

      [`(get-local ,i)
       (match-let ([`(,_ ,_ ,_ ,_ (local ,locals ...) ,_ ,_) C])
         (if (< i (length locals))
             (apply-pre-post stacks (list) (list (list-ref locals i)))
             #f))]

      [`(set-local ,i)
       (match-let ([`(,_ ,_ ,_ ,_ (local ,locals ...) ,_ ,_) C])
         (if (< i (length locals))
             (apply-pre-post stacks (list (list-ref locals i)) (list))
             #f))]

      [`(tee-local ,i)
       (match-let ([`(,_ ,_ ,_ ,_ (local ,locals ...) ,_ ,_) C])
         (if (< i (length locals))
             (let ([t (list-ref locals i)])
               (apply-pre-post stacks (list t) (list t)))
             #f))]

      [`(get-global ,i)
       (match-let ([`(,_ (global ,globals ...) ,_ ,_ ,_ ,_ ,_) C])
         (if (< i (length globals))
             (apply-pre-post stacks (list) (list (second (list-ref globals i))))
             #f))]

      [`(set-global ,i)
       (match-let ([`(,_ (global ,globals ...) ,_ ,_ ,_ ,_ ,_) C])
         (if (< i (length globals))
             (apply-pre-post stacks (list (second (list-ref globals i))) (list))
             #f))]

      [(or `(,t load ,_ ,_)
           `(,t load ,_ ,_ ,_))
       (apply-pre-post stacks (list 'i32) (list t))]

      [(or `(,t store ,_ ,_)
           `(,t store ,_ ,_ ,_))
       (apply-pre-post stacks (list 'i32 t) (list))]

      [`current-memory
       (apply-pre-post stacks (list) (list 'i32))]

      [`grow-memory
       (apply-pre-post stacks (list 'i32) (list 'i32))]))
  
  (define (synthesize-stacks-rec stacks ins)
    (if (empty? ins)
        (match/values (consume stacks post)
          [(#f _ _) #f]
          [(_ new-stacks _) new-stacks])
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
  ;; premises of the instructions not required for stack validity are checked here
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

      [`(,t1 convert ,t2 ,_)
       (if (and (not (equal? t1 t2))
                (nor (and (integer-type? t1) (integer-type? t2) (< (type-width t1) (type-width t2)))
                     (and (floating-type? t1) (floating-type? t2))))
           (stack-polyize (derivation `(⊢ ,C (,e) ((,t2) -> (,t1))) #f (list)) e-pre e-post)
           #f)]

      [`(,t1 reinterpret ,t2)
       (if (and (not (equal? t1 t2)) (= (type-width t1) (type-width t2)))
           (stack-polyize (derivation `(⊢ ,C (,e) ((,t2) -> (,t1))) #f (list)) e-pre e-post)
           #f)]

      [`unreachable
       (derivation `(⊢ ,C (unreachable) (,e-pre -> ,e-post)) #f (list))]

      [`nop
       (stack-polyize (derivation `(⊢ ,C (nop) (() -> ()) #f (list))) e-pre e-post)]

      [`drop
       (stack-polyize (derivation `(⊢ ,C (drop) ((,(first e-pre)) -> ())) #f (list))
                      e-pre e-post)]

      [`select
       (stack-polyize (derivation `(⊢ ,C (select) (,(take-right e-pre 3) -> (,(second e-pre))))
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
       ;; if synthesize-stacks succeeded then the given stacks type br
       (derivation `(⊢ ,C (,e) (,e-pre -> ,e-post)) #f (list))]

      [`(br-if ,i)
       (match-let ([`(,_ ,_ ,_ ,_ ,_ (label ,labels ...) ,_) C])
         (let ([ts (list-ref-right labels i)])
           (stack-polyize (derivation `(⊢ ,C (,e) (,(append ts (list 'i32)) -> ,ts)) #f (list))
                          e-pre e-post)))]

      [`(br-table ,i ...)
       ;; if synthesize-stacks succeeded then the given stacks type br-table
       (derivation `(⊢ ,C (,e) (,e-pre -> ,e-post)) #f (list))]

      [`return
       ;; if synthesize-stacks succeeded then the given stacks type return
       (derivation `(⊢ ,C (return) (,e-pre -> ,e-post)) #f (list))]

      [`(call ,i)
       (match-let* ([`((func ,tfs) ,_ ,_ ,_ ,_ ,_ ,_) C]
                    [`(,c-pre -> ,c-post) (list-ref tfs i)])
         (stack-polyize (derivation `(⊢ ,C (,e) (,c-pre -> ,c-post)) #f (list))
                        e-pre e-post))]

      [`(call-indirect (,c-pre -> ,c-post))
       (match C
         [`(,_ ,_ (table) ,_ ,_ ,_ ,_) #f]
         [`(,_ ,_ (table ,_) ,_ ,_ ,_ ,_)
          (derivation `(⊢ ,C (,e) (,(append c-pre (list 'i32)) -> ,c-post)) #f (list))])]

      [`(get-local ,i)
       (match-let ([`(,_ ,_ ,_ ,_ (local ,locals ...) ,_ ,_) C])
         (stack-polyize (derivation `(⊢ ,C (,e) (() -> (,(list-ref locals i)))) #f (list))
                        e-pre e-post))]

      [`(set-local ,i)
       (match-let ([`(,_ ,_ ,_ ,_ (local ,locals ...) ,_ ,_) C])
         (stack-polyize (derivation `(⊢ ,C (,e) ((,(list-ref locals i)) -> ())) #f (list))
                        e-pre e-post))]

      [`(tee-local ,i)
       (match-let ([`(,_ ,_ ,_ ,_ (local ,locals ...) ,_ ,_) C])
         (let ([t (list-ref locals i)])
           (stack-polyize (derivation `(⊢ ,C (,e) ((,t) -> (,t))) #f (list))
                          e-pre e-post)))]

      [`(get-global ,i)
       (match-let ([`(,_ (global ,globals ...) ,_ ,_ ,_ ,_ ,_) C])
         (stack-polyize (derivation `(⊢ ,C (,e) (() -> (,(second (list-ref globals i))))) #f (list))
                        e-pre e-post))]

      [`(set-global ,i)
       (match-let ([`(,_ (global ,globals ...) ,_ ,_ ,_ ,_ ,_) C])
         (if (< i (length globals))
             (let ([glob (list-ref globals i)])
               (if (equal? (first glob) 'var)
                   (stack-polyize (derivation `(⊢ ,C (,e) ((,(second glob)) -> ())) #f (list))
                                  e-pre e-post)
                   #f))
             #f))]

      [`(,t load ,a ,_)
       (match C
         [`(,_ ,_ ,_ (memory) ,_ ,_ ,_) #f]
         [`(,_ ,_ ,_ (memory ,_) ,_ ,_ ,_)
          (if (<= (expt 2 a) (type-width t))
              (stack-polyize (derivation `(⊢ ,C (,e) ((i32) -> (,t))) #f (list))
                             e-pre e-post)
              #f)])]

      [`(,t load (,tp ,sx) ,a ,_)
       (match C
         [`(,_ ,_ ,_ (memory) ,_ ,_ ,_) #f]
         [`(,_ ,_ ,_ (memory ,_) ,_ ,_ ,_)
          (if (and (<= (expt 2 a) (type-width tp))
                   (< (type-width tp) (type-width t))
                   (integer-type? t))
              (stack-polyize (derivation `(⊢ ,C (,e) ((i32) -> (,t))) #f (list))
                             e-pre e-post)
              #f)])]

      [`(,t store ,a ,_)
       (match C
         [`(,_ ,_ ,_ (memory) ,_ ,_ ,_) #f]
         [`(,_ ,_ ,_ (memory ,_) ,_ ,_ ,_)
          (if (<= (expt 2 a) (type-width t))
              (stack-polyize (derivation `(⊢ ,C (,e) ((i32 ,t) -> ())) #f (list))
                             e-pre e-post)
              #f)])]

      [`(,t store ,tp ,a ,_)
       (match C
         [`(,_ ,_ ,_ (memory) ,_ ,_ ,_) #f]
         [`(,_ ,_ ,_ (memory ,_) ,_ ,_ ,_)
          (if (and (<= (expt 2 a) (type-width tp))
                   (< (type-width tp) (type-width t))
                   (integer-type? t))
              (stack-polyize (derivation `(⊢ ,C (,e) ((i32 ,t) -> ())) #f (list))
                             e-pre e-post)
              #f)])]

      [`current-memory
       (match C
         [`(,_ ,_ ,_ (memory) ,_ ,_ ,_) #f]
         [`(,_ ,_ ,_ (memory ,_) ,_ ,_ ,_)
          (stack-polyize (derivation `(⊢ ,C (current-memory) (() -> (i32))) #f (list)) e-pre e-post)])]

      [`grow-memory
       (match C
         [`(,_ ,_ ,_ (memory) ,_ ,_ ,_) #f]
         [`(,_ ,_ ,_ (memory ,_) ,_ ,_ ,_)
          (stack-polyize (derivation `(⊢ ,C (grow-memory) ((i32) -> (i32))) #f (list)) e-pre e-post)])]

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