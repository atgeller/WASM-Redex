#lang racket

(module+ test
  (require rackunit
           redex/reduction-semantics
           "../Instantiation.rkt")

  ;; Basic empty module instantiation
  (let-values ([(s exs) (wasm-instantiate
                         empty-store
                         (term (module () () () ()))
                         empty)])
    (check-equal? s (term (((() () () ())) () ())))
    (check-equal? exs empty))

  ;; Chain module instantiation
  (let-values ([(s exs) (wasm-instantiate
                         (term (((() () () ())) () ()))
                         (term (module () () () ()))
                         empty)])
    (check-equal? s (term (((() () () ())
                            (() () () ()))
                           ()
                           ())))
    (check-equal? exs empty))

  ;; TODO: more tests
  
  )