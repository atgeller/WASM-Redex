#lang racket

(module+ test
  (require redex/reduction-semantics
           "../ModuleTyping.rkt"
           "../InstructionTyping.rkt"
           rackunit)

  (define empty-context (term ((func ()) (global ()) (table) (memory) (local ()) (label ()) (return))))
  (define context1 (term ((func ((() -> (i32))))
                          (global ())
                          (table) (memory) (local ()) (label ()) (return))))
  (define context2 (term ((func ((() -> (i32)) (() -> (i32))))
                          (global ())
                          (table) (memory) (local ()) (label ()) (return))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Basic test that an empty module is well-typed
  (test-judgment-holds ⊢-module
                       (derivation `(⊢-module (module () () () ()) ,empty-context)
                                   #f
                                   (list
                                    (derivation `(⊢-module-func-list ,empty-context () ())
                                                #f
                                                 (list))
                                    (derivation `(⊢-module-global-list () ())
                                                #f
                                                (list)))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Tests that a very simple function is well-typed under a context type containing only that function
  (test-judgment-holds ⊢-module-func
                       (derivation `(⊢-module-func ,context1
                                                   (() (func (() -> (i32)) (local () ((i32 const 0)))))
                                                   (() (() -> (i32))))
                                   #f
                                   (list
                                    (derivation `(⊢ ((func ((() -> (i32)))) (global ()) (table) (memory) (local ()) (label ((i32))) (return (i32)))
                                                    ((i32 const 0))
                                                    (() -> (i32)))
                                                #f
                                                (list)))))

  ;; Tests that a module with a single, well-typed, simple function is well-typed
  (test-judgment-holds ⊢-module
                       (derivation `(⊢-module (module ((() (func (() -> (i32)) (local () ((i32 const 0)))))) () () ()) ,context1)
                                   #f
                                   (list
                                    (derivation `(⊢-module-func-list ,context1
                                                                     ((() (func (() -> (i32)) (local () ((i32 const 0))))))
                                                                     ((() (() -> (i32)))))
                                                #f
                                                (list
                                                 (derivation `(⊢-module-func ,context1
                                                                             (() (func (() -> (i32)) (local () ((i32 const 0)))))
                                                                             (() (() -> (i32))))
                                                             #f
                                                             (list
                                                              (derivation `(⊢ ((func ((() -> (i32)))) (global ()) (table) (memory) (local ()) (label ((i32))) (return (i32)))
                                                                              ((i32 const 0))
                                                                              (() -> (i32)))
                                                                          #f
                                                                          (list))))
                                                 (derivation `(⊢-module-func-list ,context1 () ())
                                                             #f
                                                             (list))))
                                    (derivation `(⊢-module-global-list () ())
                                                #f
                                                (list)))))
  
  ;; Tests that a module with two well-typed functions that refer to each other, is well-typed
  (test-judgment-holds ⊢-module
                       (derivation `(⊢-module (module ((() (func (() -> (i32)) (local () ((call 1)))))
                                                       (() (func (() -> (i32)) (local () ((call 0)))))) () () ()) ,context2)
                                   #f
                                   (list
                                    (derivation `(⊢-module-func-list ,context2
                                                                     ((() (func (() -> (i32)) (local () ((call 1)))))
                                                                      (() (func (() -> (i32)) (local () ((call 0))))))
                                                                     ((() (() -> (i32))) (() (() -> (i32)))))
                                                #f
                                                (list
                                                 (derivation `(⊢-module-func ,context2
                                                                             (() (func (() -> (i32)) (local () ((call 1)))))
                                                                             (() (() -> (i32))))
                                                             #f
                                                             (list
                                                              (derivation `(⊢ ((func ((() -> (i32)) (() -> (i32)))) (global ()) (table) (memory) (local ()) (label ((i32))) (return (i32)))
                                                                              ((call 1))
                                                                              (() -> (i32)))
                                                                          #f
                                                                          (list))))
                                                 (derivation `(⊢-module-func-list ,context2
                                                                                  ((() (func (() -> (i32)) (local () ((call 0))))))
                                                                                  ((() (() -> (i32)))))
                                                             #f
                                                             (list
                                                              (derivation `(⊢-module-func ,context2
                                                                             (() (func (() -> (i32)) (local () ((call 0)))))
                                                                             (() (() -> (i32))))
                                                                          #f
                                                                          (list
                                                                           (derivation `(⊢ ((func ((() -> (i32)) (() -> (i32)))) (global ()) (table) (memory) (local ()) (label ((i32))) (return (i32)))
                                                                                           ((call 0))
                                                                                           (() -> (i32)))
                                                                                       #f
                                                                                       (list))))
                                                              (derivation `(⊢-module-func-list ,context2 () ())
                                                                          #f
                                                                          (list))))))
                                    (derivation `(⊢-module-global-list () ())
                                                #f
                                                (list)))))
)
