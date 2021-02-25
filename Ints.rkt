#lang racket

(provide (all-defined-out))

(define (max-unsigned-int size) (sub1 (expt 2 size)))

(define (unsigned-int-size? size i)
  (and (exact-integer? i)
       (<= 0 i (max-unsigned-int size))))

(define (max-signed-int size) (sub1 (expt 2 (sub1 size))))
(define (min-signed-int size) (* -1 (expt 2 (sub1 size))))

(define (signed-int-size? size i)
  (and (exact-integer? i)
       (<= (min-signed-int size) i (max-signed-int size))))


(define u32? (curry unsigned-int-size? 32))
(define u64? (curry unsigned-int-size? 64))

(define s32? (curry signed-int-size? 32))
(define s64? (curry signed-int-size? 64))