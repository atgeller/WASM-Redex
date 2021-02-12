#lang racket

(module+ test
  (require redex/reduction-semantics
           "TestUtilities.rkt"
           "../ModuleTyping.rkt"
           "../InstructionTyping.rkt"
           rackunit)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Basic test that an empty module is well-typed
  (test-judgment-holds ⊢-module
                       (derivation `(⊢-module (module () () () ()) ,empty-context)
                                   #f
                                   (list)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Tests that a very simple function is well-typed under a context type containing only that function
  (test-judgment-holds ⊢-module-func
                       (derivation `(⊢-module-func ,context1
                                                   (() (func (() -> (i32)) (local () ((i32 const 0)))))
                                                   (() (() -> (i32))))
                                   #f
                                   (list
                                    (derivation `(⊢ ((func (() -> (i32))) (global) (table) (memory) (local) (label (i32)) (return (i32)))
                                                    ((i32 const 0))
                                                    (() -> (i32)))
                                                #f
                                                (list)))))

  ;; Tests that a module with a single, well-typed, simple function is well-typed
  (test-judgment-holds ⊢-module
                       (derivation `(⊢-module (module ((() (func (() -> (i32)) (local () ((i32 const 0)))))) () () ()) ,context1)
                                   #f
                                   (list
                                    (derivation `(⊢-module-func ,context1
                                                                (() (func (() -> (i32)) (local () ((i32 const 0)))))
                                                                (() (() -> (i32))))
                                                #f
                                                (list
                                                 (derivation `(⊢ ((func (() -> (i32))) (global) (table) (memory) (local) (label (i32)) (return (i32)))
                                                                 ((i32 const 0))
                                                                 (() -> (i32)))
                                                             #f
                                                             (list)))))))
  
  ;; Tests that a module with two well-typed functions that refer to each other, is well-typed
  (test-judgment-holds ⊢-module
                       (derivation `(⊢-module (module ((() (func (() -> (i32)) (local () ((call 1)))))
                                                       (() (func (() -> (i32)) (local () ((call 0)))))) () () ()) ,context2)
                                   #f
                                   (list
                                    (derivation `(⊢-module-func ,context2
                                                                             (() (func (() -> (i32)) (local () ((call 1)))))
                                                                             (() (() -> (i32))))
                                                             #f
                                                             (list
                                                              (derivation `(⊢ ((func (() -> (i32)) (() -> (i32))) (global) (table) (memory) (local) (label (i32)) (return (i32)))
                                                                              ((call 1))
                                                                              (() -> (i32)))
                                                                          #f
                                                                          (list))))
                                    (derivation `(⊢-module-func ,context2
                                                                (() (func (() -> (i32)) (local () ((call 0)))))
                                                                (() (() -> (i32))))
                                                #f
                                                (list
                                                 (derivation `(⊢ ((func (() -> (i32)) (() -> (i32))) (global) (table) (memory) (local) (label (i32)) (return (i32)))
                                                                 ((call 0))
                                                                 (() -> (i32)))
                                                             #f
                                                             (list)))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Tests that a module with a single well-typed global is well-typed
  (test-judgment-holds ⊢-module
                       (derivation `(⊢-module (module () ((() (global (const i32) ((i32 const 0))))) () ()) ,context3)
                                   #f
                                   (list
                                    (derivation `(⊢-module-global ((func) (global) (table) (memory) (local) (label) (return))
                                                                  (() (global (const i32) ((i32 const 0))))
                                                                  (() (const i32)))
                                                #f
                                                (list
                                                 (derivation `(⊢ ((func) (global) (table) (memory) (local) (label) (return))
                                                                 ((i32 const 0))
                                                                 (() -> (i32)))
                                                             #f
                                                             (list)))))))
  
  ;; Tests that a module with a well-typed list of globals, where the second global refers to the first, is well-typed
  (test-judgment-holds
   ⊢-module
   (derivation `(⊢-module (module
                            ()
                            ((((export "test")) (global (const i32) ((i32 const 0))))
                             (((export "test2")) (global (const i32) ((get-global 0)))))
                            ()
                            ())
                          ,context4)
               #f
               (list
                (derivation `(⊢-module-global ((func) (global) (table) (memory) (local) (label) (return))
                                              (() (global (const i32) ((i32 const 0))))
                                              (() (const i32)))
                            #f
                            (list
                             (derivation `(⊢ ((func) (global) (table) (memory) (local) (label) (return))
                                             ((i32 const 0))
                                             (() -> (i32)))
                                         #f
                                         (list))))
                (derivation `(⊢-module-global ((func) (global (const i32)) (table) (memory) (local) (label) (return))
                                              (() (global (const i32) ((get-global 0))))
                                              (() (const i32)))
                            #f
                            (list
                             (derivation `(⊢ ((func) (global (const i32)) (table) (memory) (local) (label) (return))
                                             ((get-global 0))
                                             (() -> (i32)))
                                         #f
                                         (list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

)
