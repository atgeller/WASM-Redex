#lang racket

(require racket/flonum
         redex/reduction-semantics
         "AdministrativeSyntax.rkt"
         "../Utilities.rkt"
         "SizedOps.rkt")

(provide (all-defined-out))

; Converts a boolean value to the WebAssembly representation
(define-metafunction WASM-Admin
  bool : boolean -> c
  [(bool #f) 0]
  [(bool #t) 1])

; Metafunctions to handle converting constants values to bitstrings and vice versa, primarily used for memory operations
(define-metafunction WASM-Admin
  signed : t n -> integer
  [(signed i32 n) ,(to-signed-sized 32 (term n))]
  [(signed i64 n) ,(to-signed-sized 64 (term n))])

(define-metafunction WASM-Admin
  const->bstr : t c -> bstr
  [(const->bstr i32 c) ,(integer->integer-bytes (term c) 4 #f #f)]
  [(const->bstr i64 c) ,(integer->integer-bytes (term c) 8 #f #f)]
  [(const->bstr f32 c) ,(real->floating-point-bytes (term c) 4 #f)]
  [(const->bstr f64 c) ,(real->floating-point-bytes (term c) 8 #f)])

(define-metafunction WASM-Admin
  const->packed-bstr : t natural c -> bstr
  [(const->packed-bstr inn (name width natural) c)
   ,(integer->integer-bytes (modulo (term c) (expt 2 (term width))) (/ (term width) 8) #f #f)])

(define-metafunction WASM-Admin
  bstr->const : t bstr -> c
  [(bstr->const inn bstr) ,(integer-bytes->integer (term bstr) #f #f)]
  [(bstr->const f32 bstr) ,(floating-point-bytes->real (term bstr) #f)]
  [(bstr->const f64 bstr) ,(floating-point-bytes->real (term bstr) #f)])

(define-metafunction WASM-Admin
  packed-bstr->const : t sx bstr -> c
  [(packed-bstr->const inn signed bstr) ,(to-unsigned-sized (term (bit-width inn)) (integer-bytes->integer (term bstr) #t #f))]
  [(packed-bstr->const inn unsigned bstr) ,(integer-bytes->integer (term bstr) #f #f)])
