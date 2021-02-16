#lang racket
(module+ test
  (require redex/reduction-semantics
           "TestUtilities.rkt"
           "../ModuleTyping.rkt"
           "../InstructionTyping.rkt"
           "../Typechecking.rkt"
           rackunit)

  ;; test of empty instructions
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '()
    '() '()))

  ;; basic constants
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 const 0) (i64 const 0))
    '() '(i32 i64)))

  ;; constants wrong type
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 0) (f64 const 0.0))
    '() '(i32 i64)))

  ;; binop
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 const 0) (i32 const 1) (i32 add))
    '() '(i32)))
  
  ;; binop wrong type
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 0) (i64 const 1) (i32 add))
    '() '(i32)))

  ;; test of empty module
  (test-judgment-holds
   ⊢-module
   (typecheck-module
    '(module () () () ())))

  ;; test of basic globals
  (test-judgment-holds
   ⊢-module
   (typecheck-module
    '(module
         ()
       ((() (global (var i64) ((i64 const 0)))))
       ()
       ())))

  ;; exporting a global
  (test-judgment-holds
   ⊢-module
   (typecheck-module
    '(module
         ()
       ((((export "test")) (global (const i64) ((i64 const 0)))))
       ()
       ())))

  ;; global instructions have wrong type
  (check-false
   (typecheck-module
    '(module
         ()
       ((() (global (var i64) ((i32 const 0)))))
       ()
       ())))

  ;; exporting a variable global
  (check-false
   (typecheck-module
    '(module
         ()
       ((((export "test")) (global (var i64) ((i64 const 0)))))
       ()
       ())))

  ;; test of globals where the second refers to the first
  (test-judgment-holds
   ⊢-module
   (typecheck-module
    '(module
         ()
       ((() (global (const i32) ((i32 const 0)
                                 (i32 const 1)
                                 (i32 add))))
        (() (global (const i32) ((get-global 0)))))
       ()
       ())))

  )