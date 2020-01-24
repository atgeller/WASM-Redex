#lang racket

(module+ test
  (require redex/reduction-semantics
           "../InstructionTyping.rkt"
           rackunit)

  (define empty-context (term ((func ()) (global ()) (table) (memory) (local ()) (label ()) (return))))

  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i32 const 0) (i32 const 1) (i32 add)) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i32 const 0) (i32 const 1)) (() -> (i32 i32)))
                                                #f
                                                 (list
                                                  (derivation `(⊢ ,empty-context ((i32 const 0)) (() -> (i32))) #f (list))
                                                  (derivation `(⊢ ,empty-context ((i32 const 1)) ((i32) -> (i32 i32)))
                                                              #f
                                                              (list
                                                               (derivation `(⊢ ,empty-context ((i32 const 1)) (() -> (i32))) #f (list))))))
                                    (derivation `(⊢ ,empty-context ((i32 add)) ((i32 i32) -> (i32)))
                                                #f
                                                (list)))))
  )
