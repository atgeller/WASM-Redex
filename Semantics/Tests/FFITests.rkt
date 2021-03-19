#lang racket

(module+ test
  (require rackunit
           racket/flonum
           redex/reduction-semantics
           "../RacketFFI.rkt"
           "../Semantics.rkt"
           "../StoreUtilities.rkt")

  (define (racket-add a b)
    (+ a b))

  (define (racket-sub1 a)
    (sub1 a))

  ;; basic ffi usage
  (test-->>E (-> 0)
             (term ((((((host-func ((i32 i32) -> (i32)) ,racket-add))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const 0) (i32 const 1) (i32 const 2) (call 0))))
             (term ((((((host-func ((i32 i32) -> (i32)) ,racket-add))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const 0) (i32 const 3)))))

  ;; ffi coercion i64
  (test-->>E (-> 0)
             (term ((((((host-func ((i32 i32) -> (i64)) ,racket-add))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const 0) (i32 const 1) (i32 const 2) (call 0))))
             (term ((((((host-func ((i32 i32) -> (i64)) ,racket-add))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const 0) (i64 const 3)))))

  ;; ffi coercion f64
  (test-->>E (-> 0)
             (term ((((((host-func ((i32 i32) -> (f64)) ,racket-add))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const 0) (i32 const 1) (i32 const 2) (call 0))))
             (term ((((((host-func ((i32 i32) -> (f64)) ,racket-add))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const 0) (f64 const 3.0)))))

  ;; ffi coercion f32
  (test-->>E (-> 0)
             (term ((((((host-func ((f64 f64) -> (f32)) ,racket-add))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const 0) (f64 const 0.1) (f64 const 0.2) (call 0))))
             (term ((((((host-func ((f64 f64) -> (f32)) ,racket-add))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const 0) (f32 const ,(flsingle 0.3))))))

  ;; ffi coercion -1 i32
  (test-->>E (-> 0)
             (term ((((((host-func ((i32) -> (i32)) ,racket-sub1))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const 0) (call 0))))
             (term ((((((host-func ((i32) -> (i32)) ,racket-sub1))
                       () () ()))
                     ()
                     ()) ; store
                    () ; locals
                    ((i32 const #xFFFFFFFF)))))

  (define (make-memory size)
    (make-bytes (* (memory-page-size) size) 0))
  
  (define (store-integer mem offset width value)
    (integer->integer-bytes value width #f #f mem offset))

  (define (racket-store-test)
    (let ([mem (wasm-lookup-export "mem")])
      (wasm-mem-write-integer! mem 0 4 -1)))

  ;; ffi writes a value into memory
  (parameterize ([memory-page-size 64])
    (test-->>E (-> 0 `(("mem" . ,(wasm-memory 0))))
               (term ((((((host-func (() -> ()) ,racket-store-test))
                         () () (0)))
                       ()
                       (,(make-memory 1)))
                      ()
                      ((call 0))))
               (term ((((((host-func (() -> ()) ,racket-store-test))
                         () () (0)))
                       ()
                       (,(store-integer (make-memory 1) 0 4 #xFFFFFFFF)))
                      ()
                      ())))
    )

  ;; racket produced incorrect number of arguments
  (check-exn
   #rx".*"
   (thunk
    (apply-reduction-relation*
     (-> 0)
     (term ((((((host-func ((i32 i32) -> ()) ,racket-add))
               () () ()))
             ()
             ()) ; store
            () ; locals
            ((i32 const 0) (i32 const 1) (i32 const 2) (call 0)))))))

  ;; racket produced a value that can't be coerced
  (check-exn
   #rx".*"
   (thunk
    (apply-reduction-relation*
     (-> 0)
     (term ((((((host-func (() -> (i32)) ,(λ (s) (values s (list "hello world")))))
               () () ()))
             ()
             ()) ; store
            () ; locals
            ((i32 const 0) (call 0)))))))

  ;; racket produces an inexact when it expects an i32
  (check-exn
   #rx".*"
   (thunk
    (apply-reduction-relation*
     (-> 0)
     (term ((((((host-func (() -> (i32)) ,(λ (s) (values s (list 0.5)))))
               () () ()))
             ()
             ()) ; store
            () ; locals
            ((call 0)))))))

  ;; racket produces a value outside i32 range
  (check-exn
   #rx".*"
   (thunk
    (apply-reduction-relation*
     (-> 0)
     (term ((((((host-func (() -> (i32)) ,(λ (s) (values s (list (expt 2 32))))))
               () () ()))
             ()
             ()) ; store
            () ; locals
            ((call 0)))))))

  ;; racket produces an inexact when it expects an i64
  (check-exn
   #rx".*"
   (thunk
    (apply-reduction-relation*
     (-> 0)
     (term ((((((host-func (() -> (i64)) ,(λ (s) (values s (list 0.5)))))
               () () ()))
             ()
             ()) ; store
            () ; locals
            ((call 0)))))))

  ;; racket produces a value outside i64 range
  (check-exn
   #rx".*"
   (thunk
    (apply-reduction-relation*
     (-> 0)
     (term ((((((host-func (() -> (i64)) ,(λ (s) (values s (list (expt 2 64))))))
               () () ()))
             ()
             ()) ; store
            () ; locals
            ((call 0)))))))
  )