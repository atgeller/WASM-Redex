#lang racket
(module+ test
  (require redex/reduction-semantics
           "TestUtilities.rkt"
           "../ModuleTyping.rkt"
           "../InstructionTyping.rkt"
           "../Typechecking.rkt"
           rackunit)

  ;; Instruction typing tests

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

  
  ;; Global typing tests

  ;; simple
  (test-judgment-holds
   ⊢-module-global
   (typecheck-global
    empty-context
    '(((export "thing")) (global (const i32) ((i32 const 0))))))

  ;; variable
  (test-judgment-holds
   ⊢-module-global
   (typecheck-global
    empty-context
    '(() (global (var i32) ((i32 const 0))))))

  ;; exported mutable global doesn't typecheck
  (check-false
   (typecheck-global
    empty-context
    '(((export "thing")) (global (var i32) ((i32 const 0))))))

  ;; imported global
  (test-judgment-holds
   ⊢-module-global
   (typecheck-global
    empty-context
    '(() (global (const i64) (import "thing" "thing")))))

  ;; imported mutable global doesn't typecheck
  (check-false
   (typecheck-global
    empty-context
    '(() (global (var i32) (import "thing" "thing")))))


  ;; Table typing tests

  ;; empty table
  (test-judgment-holds
   ⊢-module-table
   (typecheck-table
    empty-context
    '(() (table 0))))

  ;; basic table
  (test-judgment-holds
   ⊢-module-table
   (typecheck-table
    '((func ((i32) -> (i32))
            ((i64) -> (f64))
            ((i32 i32) -> (i64))
            ((f64 f64) -> (f32 f32)))
      (global) (table 3) (memory) (local) (label) (return))
    '(() (table 3 2 3 1))))

  ;; table wrong length
  (check-false
   (typecheck-table
    '((func ((i32) -> (i32))
            ((i64) -> (f64))
            ((i32 i32) -> (i64))
            ((f64 f64) -> (f32 f32)))
      (global) (table 3) (memory) (local) (label) (return))
    '(() (table 3 0))))

  ;; table invalid index
  (check-false
   (typecheck-table
    '((func ((i32) -> (i32))
            ((i64) -> (f64))
            ((i32 i32) -> (i64))
            ((f64 f64) -> (f32 f32)))
      (global) (table 3) (memory) (local) (label) (return))
    '(() (table 3 2 4 1))))

  ;; imported table
  (test-judgment-holds
   ⊢-module-table
   (typecheck-table
    '((func) (global) (table 3) (memory) (local) (label) (return))
    '(() (table 3 (import "thing" "thing")))))


  ;; Memory derivation tests
  (test-judgment-holds
   ⊢-module-memory
   (memory-derivation
    empty-context
    '(() (memory 4))))

  ;; imported memory
  (test-judgment-holds
   ⊢-module-memory
   (memory-derivation
    empty-context
    '(() (memory 4 (import "thing" "thing")))))


  ;; Module typing tests

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

  ;; test of functions in module
  (test-judgment-holds
   ⊢-module
   (typecheck-module
    '(module
         ((() (func ((i32) -> (i32)) (local () ((get-local 0))))))
       ()
       ()
       ())))

  ;; non-distinct exports should fail
  (check-false
   (typecheck-module
    '(module
         ((((export "thing")) (func ((i32) -> (i32)) (local () ((get-local 0))))))
       ((((export "thing")) (global (const i32) ((i32 const 1)))))
       ()
       ())))

  )