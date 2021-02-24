#lang racket
(module+ test
  (require redex/reduction-semantics
           "TestUtilities.rkt"
           "../ModuleTyping.rkt"
           "../InstructionTyping.rkt"
           "../Typechecking.rkt"
           rackunit)

  (define-check (check-typecheck-ins C ins pre post)
    (match (typecheck-ins C ins pre post)
      [#f (fail-check "failed to find derivation")]
      [deriv
       (match-let ([(derivation `(⊢ ,C-a ,ins-a (,pre-a -> ,post-a)) _ _) deriv])
         (unless (equal? C C-a)
           (fail-check (format "derivation has wrong context\nexpected: ~a\nactual: ~a" C C-a)))
         (unless (and (equal? pre pre-a) (equal? post post-a))
           (fail-check (format "derivation has wrong type\nexpected: ~a\nactual: ~a"
                               `(,pre -> ,post) `(,pre-a -> ,post-a))))
         (unless (equal? ins ins-a)
           (fail-check (format "derivation is for the wrong instructions\nexpected: ~a\nactual: ~a" ins ins-a)))
         (unless (judgment-holds ⊢ deriv)
           (fail-check (format "derivation does not satisfy ⊢\n~a" deriv))))]))

  (define-check (check-typecheck-global C glob)
    (match (typecheck-global C glob)
      [#f (fail-check "failed to find derivation")]
      [deriv
       (match-let ([(derivation `(⊢-module-global ,C-a ,glob-a ,_) _ _) deriv])
         (unless (equal? C C-a)
           (fail-check (format "derivation has wrong context\nexpected: ~a\nactual: ~a" C C-a)))
         (unless (equal? glob glob-a)
           (fail-check (format "derivation is for the wrong global\nexpected: ~a\nactual: ~a" glob glob-a)))
         (unless (judgment-holds ⊢-module-global deriv)
           (fail-check (format "derivation does not satisfy ⊢-module-global\n~a" deriv))))]))

  (define-check (check-typecheck-table C tab)
    (match (typecheck-table C tab)
      [#f (fail-check "failed to find derivation")]
      [deriv
       (match-let ([(derivation `(⊢-module-table ,C-a ,tab-a ,_) _ _) deriv])
         (unless (equal? C C-a)
           (fail-check (format "derivation has wrong context\nexpected: ~a\nactual: ~a" C C-a)))
         (unless (equal? tab tab-a)
           (fail-check (format "derivation is for the wrong table\nexpected: ~a\nactual: ~a" tab tab-a)))
         (unless (judgment-holds ⊢-module-table deriv)
           (fail-check (format "derivation does not satisfy ⊢-module-table\n~a" deriv))))]))

  (define-check (check-memory-derivation C mem)
    (let ([deriv (memory-derivation C mem)])
      (match-let ([(derivation `(⊢-module-memory ,C-a ,mem-a ,_) _ _) deriv])
         (unless (equal? C C-a)
           (fail-check (format "derivation has wrong context\nexpected: ~a\nactual: ~a" C C-a)))
         (unless (equal? mem mem-a)
           (fail-check (format "derivation is for the wrong memory\nexpected: ~a\nactual: ~a" mem mem-a)))
         (unless (judgment-holds ⊢-module-memory deriv)
           (fail-check (format "derivation does not satisfy ⊢-module-memory\n~a" deriv))))))

  (define-check (check-typecheck-func C func)
    (match (typecheck-func C func)
      [#f (fail-check "failed to find derivation")]
      [deriv
       (match-let ([(derivation `(⊢-module-func ,C-a ,func-a ,_) _ _) deriv])
         (unless (equal? C C-a)
           (fail-check (format "derivation has wrong context\nexpected: ~a\nactual: ~a" C C-a)))
         (unless (equal? func func-a)
           (fail-check (format "derivation is for the wrong function\nexpected: ~a\nactual: ~a" func func-a)))
         (unless (judgment-holds ⊢-module-func deriv)
           (fail-check (format "derivation does not satisfy ⊢-module-func\n~a" deriv))))]))

  (define-check (check-typecheck-module mod)
    (match (typecheck-module mod)
      [#f (fail-check "failed to find derivation")]
      [deriv
       (match-let ([(derivation `(⊢-module ,mod-a) _ _) deriv])
         (unless (equal? mod mod-a)
           (fail-check (format "derivation is for the wrong module\nexpected: ~a\nactual: ~a" mod mod-a)))
         (unless (judgment-holds ⊢-module deriv)
           (fail-check (format "derivation does not satisfy ⊢-module\n~a" deriv))))]))

  ;; Instruction typing tests

  ;; test of empty instructions
  (check-typecheck-ins empty-context '() '() '())

  ;; basic constants
  (check-typecheck-ins
   empty-context
   '((i32 const 0) (i64 const 0))
   '() '(i32 i64))

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
  (check-typecheck-ins
   empty-context
   '((i32 popcnt))
   '(i32) '(i32))

  ;; binop
  (check-typecheck-ins
   empty-context
   '((i32 const 0) (i32 const 1) (i32 add))
   '() '(i32))
  
  ;; binop wrong type
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 0) (i64 const 1) (i32 add))
    '() '(i32)))

  ;; testop
  (check-typecheck-ins
   empty-context
   '((i64 const 1) (i64 eqz))
   '() '(i32))

  ;; relop
  (check-typecheck-ins
   empty-context
   '((i64 const 0) (i64 const 1) (i64 gt-u))
   '() '(i32))

  ;; convert

  ;; promote f32
  (check-typecheck-ins
   empty-context
   '((f32 convert f64))
   '(f64) '(f32))

  ;; promote f32 sx
  (check-false
   (typecheck-ins
    empty-context
    '((f32 convert f64 signed))
    '(f64) '(f32)))

  ;; demote i64
  (check-typecheck-ins
   empty-context
   '((i32 convert i64))
   '(i64) '(i32))

  ;; promote i64
  (check-typecheck-ins
   empty-context
   '((i64 convert i32 unsigned))
   '(i32) '(i64))

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

  (check-typecheck-ins
   empty-context
   '((i64 reinterpret f64))
   '(f64) '(i64))

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
  (check-typecheck-ins
   empty-context
   '(unreachable)
   '(i32) '(f64 i64))

  ;; drop from an unreachable
  (check-typecheck-ins
   empty-context
   '(unreachable
     drop)
   '(i32) '(f64 i64))

  ;; select from an unreachable
  (check-typecheck-ins
   empty-context
   '(unreachable
     (i32 const 0)
     select
     (f64 add))
   '(i32) '(f64))

  ;; select hell
  (check-typecheck-ins
   empty-context
   '(unreachable
     select
     (i32 const 0)
     select
     (i64 const 0)
     (i64 add))
   '() '(i64))

  ;; type error after unreachable
  (check-false
   (typecheck-ins
    empty-context
    '(unreachable
      (i64 const 0)
      (i32 add))
    '(i32) '(i32)))


  ;; block
  (check-typecheck-ins
   empty-context
   '((i32 const 2)
     (block ((i32) -> (i32))
            ((i32 const 1)
             (i32 add))))
   '(i64) '(i64 i32))

  ;; br inside block checks postcondition
  (check-typecheck-ins
   empty-context
   '((i32 const 2)
     (block ((i32) -> (i64))
            ((block ((i32) -> (i64))
                    ((i64 convert i32 unsigned)
                     (br 1))))))
   '() '(i64))

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
  (check-typecheck-ins
   empty-context
   '((i32 const 2)
     (loop ((i32) -> (i64))
           ((i64 convert i32 unsigned))))
   '() '(i64))

  ;; loop wrong internal type
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 2)
      (loop ((i32) -> (i64))
            ((i32 convert i64))))
    '() '(i64)))

  ;; br inside loop checks precondition
  (check-typecheck-ins
   empty-context
   '((i32 const 2)
     (loop ((i32) -> (i64))
           ((block ((i32) -> (i64))
                   ((br 1))))))
   '() '(i64))

  ;; if
  (check-typecheck-ins
   empty-context
   '((if ((i64) -> (i64))
         ((i64 const 1)
          (i64 add))
         else
         ()))
   '(i64 i32) '(i64))

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
  (check-typecheck-ins
   empty-context
   '((i32 const 0)
     (block ((i32) -> (i32))
            ((i32 const 1)
             (br-if 0)
             (i32 const 1)
             (i32 add))))
   '() '(i32))

  ;; br-table
  (check-typecheck-ins
   empty-context
   '((i32 const 0)
     (block ((i32) -> (i32))
            ((block ((i32) -> ())
                    ((block ((i32) -> ())
                            ((br-table 0 1 0 1)))
                     (i32 const 0)
                     (br 1)))
             (i32 const 1))))
   '() '(i32))

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
  (check-typecheck-ins
   '((func) (global) (table) (memory) (local) (label) (return (i32)))
   '((i32 const 0)
     (i32 const 1)
     return)
   '() '())

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
  (check-typecheck-ins
   '((func ((i64) -> (i32))) (global) (table) (memory) (local) (label) (return))
   '((i64 const 1)
     (call 0))
   '() '(i32))

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
  (check-typecheck-ins
   '((func) (global) (table 2) (memory) (local) (label) (return))
   '((i32 const 0)
     (call-indirect ((i32) -> (i32 i64))))
   '(i32) '(i32 i64))

  ;; call-indirect no table
  (check-false
   (typecheck-ins
    empty-context
    '((i32 const 0)
      (call-indirect ((i32) -> (i32 i64))))
    '(i32) '(i32 i64)))

  ;; local variables

  ;; get-local
  (check-typecheck-ins
   '((func) (global) (table) (memory) (local i32 i64) (label) (return))
   '((get-local 1))
   '() '(i64))

  ;; get-local index out of bounds
  (check-false
   (typecheck-ins
    empty-context
    '((get-local 0))
    '() '(i32)))

  ;; set-local
  (check-typecheck-ins
   '((func) (global) (table) (memory) (local i32 i64) (label) (return))
   '((i64 const 3)
     (set-local 1))
   '() '())

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
  (check-typecheck-ins
   '((func) (global) (table) (memory) (local i32 i64) (label) (return))
   '((i64 const 3)
     (tee-local 1))
   '() '(i64))

  ;; tee-local index out of bounds
  (check-false
   (typecheck-ins
    empty-context
    '((tee-local 0))
    '(i32) '(i32)))

  ;; global variables

  ;; get-global
  (check-typecheck-ins
   '((func) (global (var i32) (const i64)) (table) (memory) (local) (label) (return))
   '((get-global 1))
   '() '(i64))

  ;; get-global index out of bounds
  (check-false
   (typecheck-ins
    empty-context
    '((get-global 1))
    '() '(i64)))

  ;; set-global
  (check-typecheck-ins
   '((func) (global (var i32) (const i64)) (table) (memory) (local) (label) (return))
   '((i32 const 3)
     (set-global 0))
   '() '())

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
  (check-typecheck-global
   empty-context
   '(((export "thing")) (global (const i32) ((i32 const 0)))))

  ;; variable
  (check-typecheck-global
   empty-context
   '(() (global (var i32) ((i32 const 0)))))

  ;; exported mutable global doesn't typecheck
  (check-false
   (typecheck-global
    empty-context
    '(((export "thing")) (global (var i32) ((i32 const 0))))))

  ;; imported global
  (check-typecheck-global
   empty-context
   '(() (global (const i64) (import "thing" "thing"))))

  ;; imported mutable global doesn't typecheck
  (check-false
   (typecheck-global
    empty-context
    '(() (global (var i32) (import "thing" "thing")))))


  ;; Table typing tests

  ;; empty table
  (check-typecheck-table
   empty-context
   '(() (table 0)))

  ;; basic table
  (check-typecheck-table
   '((func ((i32) -> (i32))
           ((i64) -> (f64))
           ((i32 i32) -> (i64))
           ((f64 f64) -> (f32 f32)))
     (global) (table 3) (memory) (local) (label) (return))
   '(() (table 3 2 3 1)))

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
  (check-typecheck-table
   '((func) (global) (table 3) (memory) (local) (label) (return))
   '(() (table 3 (import "thing" "thing"))))


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
  (check-typecheck-func
   empty-context
   '(() (func ((i32) -> (i32)) (import "thing" "thing"))))

  ;; basic function
  (check-typecheck-func
   empty-context
   '(((export "test"))
     (func ((i32) -> (i32))
           (local ()
             (nop
              (get-local 0)
              nop)))))

  ;; function fails to typecheck
  (check-false
   (typecheck-func
    empty-context
    '(() (func ((i32) -> (i32)) (local () ())))))


  ;; Module typing tests

  ;; test of empty module
  (check-typecheck-module
   '(module () () () ()))

  ;; test of basic globals
  (check-typecheck-module
   '(module
        ()
      ((() (global (var i64) ((i64 const 0)))))
      ()
      ()))

  ;; exporting a global
  (check-typecheck-module
   '(module
        ()
      ((((export "test")) (global (const i64) ((i64 const 0)))))
      ()
      ()))

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
  (check-typecheck-module
   '(module
        ()
      ((() (global (const i32) ((i32 const 0)
                                (i32 const 1)
                                (i32 add))))
       (() (global (const i32) ((get-global 0)))))
      ()
      ()))

  ;; test of functions in module
  (check-typecheck-module
   '(module
        ((() (func ((i32) -> (i32)) (local () ((get-local 0))))))
      ()
      ()
      ()))

  ;; non-distinct exports should fail
  (check-false
   (typecheck-module
    '(module
         ((((export "thing")) (func ((i32) -> (i32)) (local () ((get-local 0))))))
       ((((export "thing")) (global (const i32) ((i32 const 1)))))
       ()
       ())))

  )
