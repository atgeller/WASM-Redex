#lang racket

(module+ test
  (require redex
           "TestUtilities.rkt"
           "../InstructionTyping.rkt"
           rackunit)

  ;; Binop and const
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i32 const 0) (i32 const 1) (i32 add)) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i32 const 0) (i32 const 1)) (() -> (i32 i32)))
                                                #f
                                                (list
                                                 (derivation `(⊢ ,empty-context ((i32 const 0)) (() -> (i32)))
                                                             #f
                                                             (list))
                                                 (derivation `(⊢ ,empty-context ((i32 const 1)) ((i32) -> (i32 i32)))
                                                             #f
                                                             (list
                                                              (derivation `(⊢ ,empty-context ((i32 const 1)) (() -> (i32)))
                                                                          #f
                                                                          (list))))))
                                    (derivation `(⊢ ,empty-context ((i32 add)) ((i32 i32) -> (i32)))
                                                #f
                                                (list)))))
  
  ;; testop
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i64 const 0) (i64 eqz)) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i64 const 0)) (() -> (i64)))
                                                #f
                                                (list))
                                    (derivation `(⊢ ,empty-context ((i64 eqz)) ((i64) -> (i32)))
                                                #f
                                                (list)))))

  ;; relop
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i64 const 0) (i64 const 1) (i64 gt)) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i64 const 0) (i64 const 1)) (() -> (i64 i64)))
                                                #f
                                                (list
                                                 (derivation `(⊢ ,empty-context ((i64 const 0)) (() -> (i64))) #f (list))
                                                 (derivation `(⊢ ,empty-context ((i64 const 1)) ((i64) -> (i64 i64)))
                                                             #f
                                                             (list
                                                              (derivation `(⊢ ,empty-context ((i64 const 1)) (() -> (i64))) #f (list))))))
                                    (derivation `(⊢ ,empty-context ((i64 gt)) ((i64 i64) -> (i32)))
                                                #f
                                                (list)))))

  ;; unreachable allows any following
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((unreachable) (i64 gt)) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((unreachable)) (() -> (i64 i64)))
                                                #f
                                                (list))
                                    (derivation `(⊢ ,empty-context ((i64 gt)) ((i64 i64) -> (i32)))
                                                #f
                                                (list)))))

  ;; unreachable allows any proceeding
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i32 const 0) (unreachable) (i64 gt)) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i32 const 0) (unreachable)) (() -> (i64 i64)))
                                                #f
                                                (list
                                                 (derivation `(⊢ ,empty-context ((i32 const 0)) (() -> (i32)))
                                                             #f
                                                             (list))
                                                 (derivation `(⊢ ,empty-context ((unreachable)) ((i32) -> (i64 i64)))
                                                             #f
                                                             (list))))
                                    (derivation `(⊢ ,empty-context ((i64 gt)) ((i64 i64) -> (i32)))
                                                #f
                                                (list)))))

  ;; nop
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i64 const 0) (nop) (i64 eqz)) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i64 const 0) (nop)) (() -> (i64)))
                                                #f
                                                (list
                                                 (derivation `(⊢ ,empty-context ((i64 const 0)) (() -> (i64)))
                                                             #f
                                                             (list))
                                                 (derivation `(⊢ ,empty-context ((nop)) ((i64) -> (i64)))
                                                             #f
                                                             (list
                                                              (derivation `(⊢ ,empty-context ((nop)) (() -> ()))
                                                                          #f
                                                                          (list))))))
                                    (derivation `(⊢ ,empty-context ((i64 eqz)) ((i64) -> (i32)))
                                                #f
                                                (list)))))

  ;; drop
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i32 const 0) (drop)) (() -> ()))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i32 const 0)) (() -> (i32)))
                                                #f
                                                (list))
                                    (derivation `(⊢ ,empty-context ((drop)) ((i32) -> ()))
                                                #f
                                                (list)))))

  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i64 const 0) (drop)) (() -> ()))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i64 const 0)) (() -> (i64)))
                                                #f
                                                (list))
                                    (derivation `(⊢ ,empty-context ((drop)) ((i64) -> ()))
                                                #f
                                                (list)))))

  ;; select
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i32 const 0) (i32 const 1) (i32 const 0) (select)) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i32 const 0) (i32 const 1) (i32 const 0)) (() -> (i32 i32 i32)))
                                                #f
                                                (list
                                                 (derivation `(⊢ ,empty-context ((i32 const 0) (i32 const 1)) (() -> (i32 i32)))
                                                             #f
                                                             (list
                                                              (derivation `(⊢ ,empty-context ((i32 const 0)) (() -> (i32)))
                                                                          #f
                                                                          (list))
                                                              (derivation `(⊢ ,empty-context ((i32 const 1)) ((i32) -> (i32 i32)))
                                                                          #f
                                                                          (list
                                                                           (derivation `(⊢ ,empty-context ((i32 const 1)) (() -> (i32)))
                                                                                       #f
                                                                                       (list))))))
                                                 (derivation `(⊢ ,empty-context ((i32 const 0)) ((i32 i32) -> (i32 i32 i32)))
                                                                          #f
                                                                          (list
                                                                           (derivation `(⊢ ,empty-context ((i32 const 0)) (() -> (i32)))
                                                                                       #f
                                                                                       (list))))))
                                    (derivation `(⊢ ,empty-context ((select)) ((i32 i32 i32) -> (i32)))
                                                #f
                                                (list)))))
  
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i64 const 0) (i64 const 1) (i32 const 0) (select)) (() -> (i64)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i64 const 0) (i64 const 1) (i32 const 0)) (() -> (i64 i64 i32)))
                                                #f
                                                (list
                                                 (derivation `(⊢ ,empty-context ((i64 const 0) (i64 const 1)) (() -> (i64 i64)))
                                                             #f
                                                             (list
                                                              (derivation `(⊢ ,empty-context ((i64 const 0)) (() -> (i64)))
                                                                          #f
                                                                          (list))
                                                              (derivation `(⊢ ,empty-context ((i64 const 1)) ((i64) -> (i64 i64)))
                                                                          #f
                                                                          (list
                                                                           (derivation `(⊢ ,empty-context ((i64 const 1)) (() -> (i64)))
                                                                                       #f
                                                                                       (list))))))
                                                 (derivation `(⊢ ,empty-context ((i32 const 0)) ((i64 i64) -> (i64 i64 i32)))
                                                                          #f
                                                                          (list
                                                                           (derivation `(⊢ ,empty-context ((i32 const 0)) (() -> (i32)))
                                                                                       #f
                                                                                       (list))))))
                                    (derivation `(⊢ ,empty-context ((select)) ((i64 i64 i32) -> (i64)))
                                                #f
                                                (list)))))

  ;; block
  (test-judgment-holds ⊢
                       (derivation `(⊢ ((func ()) (global ()) (table) (memory) (local ()) (label ((i32))) (return))
                                                    ((i32 const 0))
                                                    (() -> (i32)))
                                                #f
                                                (list)))
  
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((block (() -> (i32)) ((i32 const 0)))) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ((func ()) (global ()) (table) (memory) (local ()) (label ((i32))) (return))
                                                    ((i32 const 0))
                                                    (() -> (i32)))
                                                #f
                                                (list)))))
  
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((block ((i64) -> (i32)) ((drop) (i32 const 0)))) ((i64) -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ((func ()) (global ()) (table) (memory) (local ()) (label ((i32))) (return))
                                                    ((drop) (i32 const 0))
                                                    ((i64) -> (i32)))
                                                #f
                                                (list
                                                 (derivation `(⊢ ((func ()) (global ()) (table) (memory) (local ()) (label ((i32))) (return))
                                                                 ((drop))
                                                                 ((i64) -> ()))
                                                             #f
                                                             (list))
                                                 (derivation `(⊢ ((func ()) (global ()) (table) (memory) (local ()) (label ((i32))) (return))
                                                                 ((i32 const 0))
                                                                 (() -> (i32)))
                                                             #f
                                                             (list)))))))
  
  (test-judgment-holds ⊢
                       (derivation `(⊢ ,empty-context ((i64 const 0) (block ((i64) -> (i32)) ((drop) (i32 const 0)))) (() -> (i32)))
                                   #f
                                   (list
                                    (derivation `(⊢ ,empty-context ((i64 const 0)) (() -> (i64)))
                                                #f
                                                (list))
                                    (derivation `(⊢ ,empty-context ((block ((i64) -> (i32)) ((drop) (i32 const 0)))) ((i64) -> (i32)))
                                                #f
                                                (list
                                                 (derivation `(⊢ ((func ()) (global ()) (table) (memory) (local ()) (label ((i32))) (return))
                                                                 ((drop) (i32 const 0))
                                                                 ((i64) -> (i32)))
                                                             #f
                                                             (list
                                                              (derivation `(⊢ ((func ()) (global ()) (table) (memory) (local ()) (label ((i32))) (return))
                                                                              ((drop))
                                                                              ((i64) -> ()))
                                                                          #f
                                                                          (list))
                                                              (derivation `(⊢ ((func ()) (global ()) (table) (memory) (local ()) (label ((i32))) (return))
                                                                              ((i32 const 0))
                                                                              (() -> (i32)))
                                                                          #f
                                                                          (list)))))))))

  (test-judgment-holds ⊢
                       (derivation `(⊢ ((func ()) (global ()) (table) (memory 4096) (local ()) (label ()) (return))
                                       ((i32 load 0 0))
                                       ((i32) -> (i32)))
                                   #f
                                   (list)))

  (test-judgment-holds ⊢
                       (derivation `(⊢ ((func ()) (global ()) (table) (memory 4096) (local ()) (label ()) (return))
                                       ((i64 store 0 0))
                                       ((i32 i64) -> ()))
                                   #f
                                   (list))))
