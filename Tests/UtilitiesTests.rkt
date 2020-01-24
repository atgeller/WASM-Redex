#lang racket

(module+ test
  (require redex/reduction-semantics
           "../Utilities.rkt"
           rackunit)

  ;; do-get and do-set tests
  (test-equal (term (do-get (do-set ((i32 const 0) (i32 const 1) (i32 const 2)) 1 (i32 const 11)) 1))
              (term (i32 const 11)))

  ;; Function-lookup test
  (test-equal (term (function-lookup ((() () (table) (memory))
                                      (((0 (() (func ((i32) -> ()) (local () ((get-local 0) (return))))))
                                        (1 (() (func ((i32 i32) -> ()) (local () ((get-local 1) (return))))))
                                        (2 (() (func ((i32 i32 i32) -> ()) (local () ((get-local 2) (return))))))) () (table) (memory))
                                      (() () (table) (memory)))
                                     1 1))
              (term (1 (() (func ((i32 i32) -> ()) (local () ((get-local 1) (return))))))))

  ;; setup-call test
  (test-equal (term (setup-call ((i32 const 1) (i32 const 2) (i32 const 3))
                                (0 (() (func ((i32 i32) -> (i32)) (local (i32) ((return))))))
                                ((nop) (nop))))
              (term ((i32 const 1) (local (0 ((i32 const 2) (i32 const 3) (i32 const 0)))
                                     ((block (() -> (i32)) ((return))))) (nop) (nop))))

  ;; handle-call-indirect
  (test-equal (term (handle-call-indirect (((() () (table 1) (memory)))
                                           (((0 (() (func ((i32) -> ()) (local () ((get-local 0) (return))))))
                                             (1 (() (func ((i32 i32) -> ()) (local () ((get-local 1) (return))))))
                                             (2 (() (func ((i32 i32 i32) -> ()) (local () ((get-local 2) (return)))))))
                                            ((3 (() (func ((i32) -> ()) (local () ((get-local 0) (return))))))
                                             (4 (() (func ((i32 i32) -> ()) (local () ((get-local 1) (return))))))
                                             (5 (() (func ((i32 i32 i32) -> ()) (local () ((get-local 2) (return))))))))
                                           ())
                                          0 1 ((i32 i32) -> ())))
              (term (call (4 (() (func ((i32 i32) -> ()) (local () ((get-local 1) (return)))))))))

  (test-equal (term (handle-call-indirect (((() () (table 1) (memory)))
                                           (((0 (() (func ((i32) -> ()) (local () ((get-local 0) (return))))))
                                             (1 (() (func ((i32 i32) -> ()) (local () ((get-local 1) (return))))))
                                             (2 (() (func ((i32 i32 i32) -> ()) (local () ((get-local 2) (return)))))))
                                            ((3 (() (func ((i32) -> ()) (local () ((get-local 0) (return))))))
                                             (4 (() (func ((i32 i32) -> ()) (local () ((get-local 1) (return))))))
                                             (5 (() (func ((i32 i32 i32) -> ()) (local () ((get-local 2) (return))))))))
                                           ())
                                          0 1 ((i64) -> ())))
              (term (trap)))
  )
