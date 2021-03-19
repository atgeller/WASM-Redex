#lang racket

(require redex/reduction-semantics
         "RacketFFI.rkt"
         "Semantics.rkt")

(provide empty-store wasm-instantiate)

(define empty-store (term (() () ())))

;; s (listof (WASM mod)) import-dict -> s export-dict
;; takes a WASM module, a start function name, and returns a store and a list of exports
(define (wasm-instantiate s mod imports)

  (define (lookup-import ns name)
    (dict-ref (dict-ref imports
                        ns
                        (thunk (error (format "Link error: namespace ~a not in imports" ns)))
              name
              (thunk (error (format "Link error: imported object ~a not in namespace ~a" name ns))))))

  (define (create-exports exs obj)
    (map (λ (ex)
           (match-let ([`(export ,name) ex])
             (cons name obj)))
         exs))
  
  (match-let ([`(,insts ,tabinsts ,meminsts) s]
              [`(module ,fs ,globs ,tab? ,mem?) mod])
    (let ([inst-num (length insts)])

      (define (instantiate-funcs fs (i 0))
        (if (empty? fs)
            (values '() '())
            (let-values ([(cl cl-exs)
                          (match (first fs)
                            [`(,exs (func ,tf (import ,ns ,name)))
                             (values (match (lookup-import ns name)
                                       [(? procedure? proc)
                                        (term (host-func ,tf ,proc))]
                                       [(wasm-func import-inst import-index)
                                        ;; lookup the cl from the previous store
                                        (match-let ([`(,s-cls ,_ ,_ ,_) (list-ref insts import-inst)])
                                          (list-ref s-cls import-index))]
                                       [_ (error "Link error: imported object is not a function")])
                                     exs)]
                            [`(,exs (func ,tf (local ,ts ,es)))
                             (values (term (,inst-num (func ,tf (local ,ts ,es)))) exs)])]
                         
                         [(cls exports) (instantiate-funcs (rest fs) (add1 i))])
          
              (values (cons cl cls)
                      (append (create-exports cl-exs (wasm-func inst-num i)) exports)))))

      (define (instantiate-globs globs (i 0) (vs '()) (exports '()))
        (if (empty? globs)
            (values vs exports)
            (let-values ([(vs-new v-exs)
                          (match (first globs)
                            [`(,exs (global (,_ ,t) (import ,ns ,name)))
                             (values (match (lookup-import ns name)
                                       [(? real? num)
                                        (term (,t const ,(coerce-value t num)))]
                                       [(wasm-global import-inst import-index)
                                        ;; lookup the v from the previous store
                                        (match-let ([`(,_ ,s-vs ,_ ,_) (list-ref insts import-inst)])
                                          (list-ref s-vs import-index))]
                                       [_ (error "Link error: imported object is not a global variable")])
                                     exs)]
                            [`(,exs (global ,_ ,es))
                             (match-let ([`(((((() ,vs-new () ())) () ()) () (,v)))
                                          (apply-reduction-relation*
                                           (-> 0)
                                           (term ((((() ,vs () ())) () ()) () ,es)))])
                               (if (equal? v 'trap)
                                   (error "Evaluating global initial value trapped")
                                   (values (append vs-new (list v)) exs)))])])
              (instantiate-globs
               (rest globs)
               (add1 i)
               vs-new
               (append exports (create-exports v-exs (wasm-global inst-num i)))))))

      (define (instantiate-tab tab? cls)
        (if (empty? tab?)
            (values '() '() '())
            (let-values ([(index tabinst tab-exs)
                          (match (first tab?)
                            [`(,exs (table ,n (import ,ns ,name)))
                             (values (match (lookup-import ns name)
                                       ;; TODO: check imported table is the right size
                                       [(wasm-table import-index) import-index]
                                       [_ (error "Link error: imported object is not a table")])
                                     '()
                                     exs)]
                            [`(,exs (table ,n ,is ...))
                             (values (length tabinsts)
                                     (map (curry list-ref cls) is)
                                     exs)])])
              (values (list index)
                      (list tabinst)
                      (create-exports tab-exs (wasm-table index))))))

      (define (instantiate-mem mem?)
        (if (empty? mem?)
            (values '() '() '())
            (let-values ([(index meminst mem-exs)
                          (match (first mem?)
                            [`(,exs (memory ,n (import ,ns ,name)))
                             (values (match (lookup-import ns name)
                                       ;; TODO: check imported memory is the right size
                                       [(wasm-memory import-index) import-index]
                                       [_ (error "Link error: imported object is not a memory")])
                                     '()
                                     exs)]
                            [`(,exs (memory ,n))
                             (values (length meminsts)
                                     (make-bytes (* (memory-page-size) n) 0)
                                     exs)])])
              (values (list index)
                      (list meminst)
                      (create-exports mem-exs (wasm-memory index))))))
      
      (let*-values ([(cls fs-exports) (instantiate-funcs fs)]
                    [(vs globs-exports) (instantiate-globs globs)]
                    [(tab-index mod-tabinsts tab-exports) (instantiate-tab tab? cls)]
                    [(mem-index mod-meminsts mem-exports) (instantiate-mem mem?)]
                    [(inst) (term (,cls ,vs ,tab-index ,mem-index))])
        
        (values (term (,(append insts (list inst))
                       ,(append tabinsts mod-tabinsts)
                       ,(append meminsts mod-meminsts)))
                (append fs-exports globs-exports tab-exports mem-exports))))))