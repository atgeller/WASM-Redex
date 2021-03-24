#lang racket

(require racket/flonum
         redex/reduction-semantics
         "AdministrativeSyntax.rkt"
         "../Utilities.rkt"
         "StoreUtilities.rkt"
         "SizedOps.rkt")

(provide racket-trampoline
         memory-page-size
         wasm-grow-memory
         wasm-mem-read-integer
         wasm-mem-write-integer!
         wasm-grow-table!
         wasm-table-get
         wasm-table-set!
         wasm-lookup-export

         coerce-value

         ;; These structs are provided so instantiation can use them,
         ;; but they should not be constructed by ffi functions
         (struct-out wasm-memory)
         (struct-out wasm-table)
         (struct-out wasm-func)
         (struct-out wasm-global))


(define memory-page-size (make-parameter 65536))

;; these parameters are set when calling racket ffi procedures as the getter/setter for the wasm store
(define get-store (make-parameter #f))
(define set-store! (make-parameter #f))

(define wasm-exports (make-parameter #f))

(struct wasm-memory (index))

;; wasm-memory natural -> void
(define (wasm-grow-memory mem n)
  (let ([i (wasm-memory-index mem)])
    (match-let ([`(,insts ,tabinsts ,meminsts) ((get-store))])
      ((set-store!) (term (,insts ,tabinsts (with-index ,meminsts ,i ,(bytes-append (list-ref meminsts i) (make-bytes (* (memory-page-size) n) 0)))))))))

;; wasm-memory natural natural boolean -> integer
(define (wasm-mem-read-integer mem offset width signed?)
  (unless (or (= width 1) (= width 2) (= width 4) (= width 8))
    (error "Integer width must be one of 1, 2, 4, 8"))
  (match-let ([`(,_ ,_ ,meminsts) ((get-store))])
    (let ([mem-bytes (list-ref meminsts (wasm-memory-index mem))])
      (integer-bytes->integer mem-bytes signed? #f offset (+ offset width)))))

;; wasm-memory natural natural integer -> void
(define (wasm-mem-write-integer! mem offset width value)
  (unless (or (= width 1) (= width 2) (= width 4) (= width 8))
    (error "Integer width must be one of 1, 2, 4, 8"))
  (match-let ([`(,insts ,tabinsts ,meminsts) ((get-store))])
    (let* ([i (wasm-memory-index mem)]
           [mem-bytes (bytes-copy (list-ref meminsts i))])
      ((set-store!) (term (,insts ,tabinsts (with-index ,meminsts ,i ,(integer->integer-bytes value width (< value 0) #f mem-bytes offset))))))))

(struct wasm-table (index))

;; wasm-table natural -> void
(define (wasm-grow-table! table n)
  (let ([i (wasm-table-index table)])
    (match-let ([`(,insts ,tabinsts ,meminsts) ((get-store))])
      ((set-store!) (term (,insts (with-index ,tabinsts ,i ,(append (list-ref tabinsts i) (build-list n (λ (_) (term uninit))))) ,meminsts))))))

;; wasm-table natural -> cl
(define (wasm-table-get table n)
  (let ([i (wasm-table-index table)])
    (match-let ([`(,insts ,tabinsts ,meminsts) ((get-store))])
      (list-ref (list-ref tabinsts i) n))))

(struct wasm-func (inst index))

;; wasm-table natural (or/c wasm-func 'uninit) -> void
(define (wasm-table-set! table n func)
  (unless (or (wasm-func? func) (equal? func 'uninit))
    (error "Table entry must be an exported function or 'uninit"))
  (let* ([i (wasm-table-index table)]
         [s ((get-store))]
         [cl (if (wasm-func? func)
                 (term (store-func ,s ,(wasm-func-inst func) ,(wasm-func-index func)))
                 'uninit)])
    (match-let ([`(,insts ,tabinsts ,meminsts) s])
      ((set-store!) (term (,insts (with-index ,tabinsts ,i (with-index ,(list-ref tabinsts i) n cl)) ,meminsts))))))


;; wasm-func . args -> e e
#;(define (wasm-func-call func . args)
  ; need to change trampoline code to return es, then produce a local call and foreign call with the continuation
  'TODO)

(struct wasm-global (inst index))

;; wasm-global real -> void
;; commented out since you can't export mutable globals with the current WASM version modelled
#;(define (wasm-set-global! global v)
  (let ([inst (wasm-global-inst global)]
        [i (wasm-global-index global)])
    (match-let* ([`(,insts ,tabinsts ,meminsts) ((get-store))]
                 [`(,cls ,globs ,tab ,mem) (list-ref insts inst)]
                 [`(,t const ,_) (list-ref globs i)])
      ((set-store!) (term ((with-index ,insts ,inst
                             (,cls (with-index ,globs ,i (,t const ,(coerce-value t v))) ,tab ,mem))
                           ,tabinsts
                           ,meminsts))))))

;; wasm-global -> real
(define (wasm-get-global global)
  (match-let* ([`(,insts ,_ ,_) ((get-store))]
               [`(,_ ,globs ,_ ,_) (list-ref insts (wasm-global-inst global))]
               [`(,_ const ,v) (list-ref globs (wasm-global-index global))])
    v))
      

;; Exports are assumed to be valid
(define (wasm-lookup-export name)
  (dict-ref (wasm-exports) name))

(define (racket-trampoline post exports proc s args)
  (let* ([s-box (box s)]
         [ret-vals (filter (λ (rv) (not (void? rv)))
                           (call-with-values (thunk
                                              (parameterize ([get-store (λ () (unbox s-box))]
                                                             [set-store! (λ (s-new) (set-box! s-box s-new))]
                                                             [wasm-exports exports])
                                                (apply proc args)))
                                             list))])
    (unless (= (length post) (length ret-vals))
      (error "Racket procedure produced an incorrect number of values"))

    (term (,(unbox s-box) ,(map coerce-value post ret-vals)))))

(define (coerce-value t n)
  (unless (real? n)
    (error "Cannot coerce a non-real value"))
  (match t
    ['i32
     (unless (exact? n)
       (error "Cannot coerce an inexact to an i32"))
     (unless (<= (- (expt 2 31)) n (sub1 (expt 2 32)))
       (error "Value outside of i32 range"))
     (to-unsigned-sized 32 n)]
    
    ['i64
     (unless (exact? n)
       (error "Cannot coerce an inexact to an i64"))
     (unless (<= (- (expt 2 63)) n (sub1 (expt 2 64)))
       (error "Value outside of i64 range"))
     (to-unsigned-sized 64 n)]
    
    ['f32 (flsingle (real->double-flonum n))]
    
    ['f64 (real->double-flonum n)]))

