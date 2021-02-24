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

  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 0))
    '(i32) '(i32)))

  ;; unop
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 popcnt))
    '(i32) '(i32)))

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

  ;; testop
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i64 const 1) (i64 eqz))
    '() '(i32)))

  ;; relop
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i64 const 0) (i64 const 1) (i64 gt-u))
    '() '(i32)))

  ;; convert

  ;; promote f32
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((f32 convert f64))
    '(f64) '(f32)))

  ;; promote f32 sx
  (check-false
   (typecheck-ins
    empty-context
    '((f32 convert f64 signed))
    '(f64) '(f32)))

  ;; demote i64
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 convert i64))
    '(i64) '(i32)))

  ;; promote i64
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i64 convert i32 unsigned))
    '(i32) '(i64)))

  ;; promote i64 no sx
  (check-false
   (typecheck-ins
    empty-context
    '((i64 convert i32))
    '(i32) '(i64)))

  ;; convert same type
  (check-false
   (typecheck-ins
    empty-context
    '((i32 convert i32))
    '(i32) '(i32)))

  ;; convert same type sx
  (check-false
   (typecheck-ins
    empty-context
    '((i32 convert i32 signed))
    '(i32) '(i32)))

  ;; reinterpret

  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i64 reinterpret f64))
    '(f64) '(i64)))

  ;; reinterpret same type
  (check-false
   (typecheck-ins
    empty-context
    '((i64 reinterpret i64))
    '(i64) '(i64)))

  ;; reinterpret different size
  (check-false
   (typecheck-ins
    empty-context
    '((i64 reinterpret f32))
    '(f32) '(i64)))

  ;; reinterpret sx
  (check-false
   (typecheck-ins
    empty-context
    '((i64 reinterpret f64 signed))
    '(f64) '(i64)))

  ;; unreachable
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '(unreachable)
    '(i32) '(f64 i64)))

  ;; drop from an unreachable
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '(unreachable
      drop)
    '(i32) '(f64 i64)))

  ;; select from an unreachable
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '(unreachable
      (i32 const 0)
      select
      (f64 add))
    '(i32) '(f64)))

  ;; select hell
  (test-judgment-holds
   ⊢
  (typecheck-ins
   empty-context
   '(unreachable
     select
     (i32 const 0)
     select
     (i64 const 0)
     (i64 add))
   '() '(i64)))

  ;; type error after unreachable
  (check-false
   (typecheck-ins
    empty-context
    '(unreachable
      (i64 const 0)
      (i32 add))
    '(i32) '(i32)))


  ;; block
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 const 2)
      (block ((i32) -> (i32))
             ((i32 const 1)
              (i32 add))))
    '(i64) '(i64 i32)))

  ;; br inside block checks postcondition
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 const 2)
      (block ((i32) -> (i64))
             ((block ((i32) -> (i64))
                     ((i64 convert i32 unsigned)
                      (br 1))))))
    '() '(i64)))

  ;; block type wrong
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 2)
      (block ((i32) -> (i32))
             ((i64 const 1)
              (i64 add))))
    '(i64) '(i64 i32)))

  ;; loop
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 const 2)
      (loop ((i32) -> (i64))
            ((i64 convert i32 unsigned))))
    '() '(i64)))

  ;; loop wrong internal type
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 2)
      (loop ((i32) -> (i64))
            ((i32 convert i64))))
    '() '(i64)))

  ;; br inside loop checks precondition
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 const 2)
      (loop ((i32) -> (i64))
            ((block ((i32) -> (i64))
                    ((br 1))))))
    '() '(i64)))

  ;; if
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((if ((i64) -> (i64))
          ((i64 const 1)
           (i64 add))
          else
          ()))
    '(i64 i32) '(i64)))

  ;; if then case doesn't type
  (check-false
   (typecheck-ins
    empty-context
    '((if ((i64) -> (i64))
          ((i32 const 1)
           (i32 add))
          else
          ()))
    '(i64 i32) '(i64)))

  ;; if else case doesn't type
  (check-false
   (typecheck-ins
    empty-context
    '((if ((i64) -> (i64))
          ((i64 const 1)
           (i64 add))
          else
          ((i64 const 2))))
    '(i64 i32) '(i64)))

  ;; branching

  ;; br-if
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 const 0)
      (block ((i32) -> (i32))
             ((i32 const 1)
              (br-if 0)
              (i32 const 1)
              (i32 add))))
    '() '(i32)))

  ;; br-table
  (test-judgment-holds
   ⊢
   (typecheck-ins
    empty-context
    '((i32 const 0)
      (block ((i32) -> (i32))
             ((block ((i32) -> ())
                     ((block ((i32) -> ())
                             ((br-table 0 1 0 1)))
                      (i32 const 0)
                      (br 1)))
              (i32 const 1))))
    '() '(i32)))

  ;; can't branch in an empty context
  ;; checking that this fails gracefully
  (check-false
   (typecheck-ins
    empty-context
    '((br 0))
    '() '()))
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 0) (br-if 0))
    '() '()))
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 0) (br-table 0))
    '() '()))

  ;; br wrong label type
  (check-false
   (typecheck-ins
    empty-context
    '((block (() -> (i32))
             ((i64 const 0)
              (br 0))))
    '() '(i32)))
  
  ;; same for br-if
  (check-false
   (typecheck-ins
    empty-context
    '((i64 const 1)
      (block ((i64) -> (i32))
             ((i32 const 0)
              (br-if 0))))
    '() '(i32)))

  ;; same for br-table
  (check-false
   (typecheck-ins
    empty-context
    '((i64 const 1)
      (block ((i64) -> (i32))
             ((block ((i64) -> (i32))
                     ((i32 const 0)
                      (br-table 0 1))))))
    '() '(i32)))

  ;; not all br-table branches equal
  (check-false
   (typecheck-ins
    empty-context
    '((i64 const 1)
      (block ((i64) -> (i32))
             ((block ((i64) -> (i64))
                     ((i32 const 0)
                      (br-table 0 1)))
              drop
              (i32 const 0))))
    '() '(i32)))

  ;; function calls and return

  ;; basic return
  (test-judgment-holds
   ⊢
   (typecheck-ins
    '((func) (global) (table) (memory) (local) (label) (return (i32)))
    '((i32 const 0)
      (i32 const 1)
      return)
    '() '()))

  ;; return wrong type
  (check-false
   (typecheck-ins
    '((func) (global) (table) (memory) (local) (label) (return (i32)))
    '((i64 const 1)
      return)
    '() '()))

  ;; return from empty context doesn't type
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 0)
      return)
    '() '()))

  ;; function call
  (test-judgment-holds
   ⊢
   (typecheck-ins
    '((func ((i64) -> (i32))) (global) (table) (memory) (local) (label) (return))
    '((i64 const 1)
      (call 0))
    '() '(i32)))

  ;; call invalid index
  (check-false
   (typecheck-ins
    empty-context
    '((call 0))
    '() '()))

  ;; call wrong type
  (check-false
   (typecheck-ins
    '((func ((i64) -> (i32))) (global) (table) (memory) (local) (label) (return))
    '((i32 const 1)
      (call 0))
    '() '(i32)))

  ;; call-indirect
  (test-judgment-holds
   ⊢
   (typecheck-ins
    '((func) (global) (table 2) (memory) (local) (label) (return))
    '((i32 const 0)
      (call-indirect ((i32) -> (i32 i64))))
    '(i32) '(i32 i64)))

  ;; call-indirect no table
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 0)
      (call-indirect ((i32) -> (i32 i64))))
    '(i32) '(i32 i64)))

  ;; local variables

  ;; get-local
  (test-judgment-holds
   ⊢
   (typecheck-ins
    '((func) (global) (table) (memory) (local i32 i64) (label) (return))
    '((get-local 1))
    '() '(i64)))

  ;; get-local index out of bounds
  (check-false
   (typecheck-ins
    empty-context
    '((get-local 0))
    '() '(i32)))

  ;; set-local
  (test-judgment-holds
   ⊢
   (typecheck-ins
    '((func) (global) (table) (memory) (local i32 i64) (label) (return))
    '((i64 const 3)
      (set-local 1))
    '() '()))

  ;; set-local wrong type
  (check-false
   (typecheck-ins
    '((func) (global) (table) (memory) (local i32 i64) (label) (return))
    '((i64 const 1)
      (set-local 0))
    '() '()))

  ;; set-local index out of bounds
  (check-false
   (typecheck-ins
    empty-context
    '((set-local 0))
    '(i32) '()))

  ;; tee-local
  (test-judgment-holds
   ⊢
   (typecheck-ins
    '((func) (global) (table) (memory) (local i32 i64) (label) (return))
    '((i64 const 3)
      (tee-local 1))
    '() '(i64)))

  ;; tee-local index out of bounds
  (check-false
   (typecheck-ins
    empty-context
    '((tee-local 0))
    '(i32) '(i32)))

  ;; global variables

  ;; get-global
  (test-judgment-holds
   ⊢
   (typecheck-ins
    '((func) (global (var i32) (const i64)) (table) (memory) (local) (label) (return))
    '((get-global 1))
    '() '(i64)))

  ;; get-global index out of bounds
  (check-false
   (typecheck-ins
    empty-context
    '((get-global 1))
    '() '(i64)))

  ;; set-global
  (test-judgment-holds
   ⊢
   (typecheck-ins
    '((func) (global (var i32) (const i64)) (table) (memory) (local) (label) (return))
    '((i32 const 3)
      (set-global 0))
    '() '()))

  ;; set-global wrong type
  (check-false
   (typecheck-ins
    '((func) (global (var i32) (const i64)) (table) (memory) (local) (label) (return))
    '((i64 const 3)
      (set-global 0))
    '() '()))

  ;; set-global immutable
  (check-false
   (typecheck-ins
    '((func) (global (var i32) (const i64)) (table) (memory) (local) (label) (return))
    '((i64 const 3)
      (set-global 1))
    '() '()))

  ;; set-global index out of bounds
  (check-false
   (typecheck-ins
    empty-context
    '((set-global 1))
    '(i32) '()))

  
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


  ;; Function typing tests

  ;; imported function
  (test-judgment-holds
   ⊢-module-func
   (typecheck-func
    empty-context
    '(() (func ((i32) -> (i32)) (import "thing" "thing")))))

  ;; basic function
  (test-judgment-holds
   ⊢-module-func
   (typecheck-func
    empty-context
    '(((export "test"))
      (func ((i32) -> (i32))
            (local ()
              (nop
               (get-local 0)
               nop))))))

  ;; function fails to typecheck
  (check-false
   (typecheck-func
    empty-context
    '(() (func ((i32) -> (i32)) (local () ())))))


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