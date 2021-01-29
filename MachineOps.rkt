#lang racket

(provide (all-defined-out))

(define (max-unsigned-int size) (sub1 (expt 2 size)))

(define (unsigned-int-size? size i)
  (and (exact-integer? i)
       (<= 0 i (max-unsigned-int size))))

(define (to-unsigned-sized size x)
  (modulo x (expt 2 size)))


(define (max-signed-int size) (sub1 (expt 2 (sub1 size))))
(define (min-signed-int size) (* -1 (expt 2 (sub1 size))))

(define (signed-int-size? size i)
  (and (exact-integer? i)
       (<= (min-signed-int size) i (max-signed-int size))))

(define (to-signed-sized size x)
  (let ([unsigned (to-unsigned-sized size x)])
    (if (<= unsigned (max-signed-int size))
        unsigned
        (- unsigned (expt 2 size)))))


(define u32? (curry unsigned-int-size? 32))
(define u64? (curry unsigned-int-size? 64))

(define s32? (curry signed-int-size? 32))
(define s64? (curry signed-int-size? 64))


(define (sized-add size n1 n2)
  (to-unsigned-sized size (+ n1 n2)))

(define (sized-mul size n1 n2)
  (to-unsigned-sized size (* n1 n2)))

(define (sized-sub size n1 n2)
  (to-unsigned-sized size (- n1 n2)))

(define (sized-unsigned-div size n1 n2)
  (to-unsigned-sized size (quotient n1 n2)))

(define (sized-signed-div size n1 n2)
  (to-unsigned-sized size (quotient n1 (to-signed-sized size n2))))

(define (sized-unsigned-rem size n1 n2)
  (to-unsigned-sized size (modulo n1 n2)))

(define (sized-signed-rem size n1 n2)
  (to-unsigned-sized size (modulo n1 (to-signed-sized size n2))))

(define (sized-shl size n1 n2)
  (to-unsigned-sized size (arithmetic-shift n1 n2)))

(define (sized-unsigned-shr size n1 n2)
  (to-unsigned-sized size (arithmetic-shift n1 (- n2))))

(define (sized-signed-shr size n1 n2)
  (to-unsigned-sized size (arithmetic-shift (to-signed-sized size n1) (- n2))))

(define (sized-rotl size n1 n2)
  (to-unsigned-sized size (bitwise-ior (arithmetic-shift n1 (modulo n2 size))
                                       (arithmetic-shift n1 (- (modulo n2 size) size)))))

(define (sized-rotr size n1 n2)
  (to-unsigned-sized size (bitwise-ior (arithmetic-shift n1 (- (modulo n2 size)))
                                       (arithmetic-shift n1 (- size (modulo n2 size))))))

(define (sized-clz size n)
  (cond
    [(= size 0) 0]
    [(>= n (expt 2 (sub1 size))) 0]
    [else (add1 (sized-clz (sub1 size) n))]))

(define (sized-ctz size n)
  (cond
    [(= size 0) 0]
    [(= 1 (modulo n 2)) 0]
    [else (add1 (sized-ctz (sub1 size) (arithmetic-shift n -1)))]))

(define (sized-popcnt size n)
  (cond
    [(= size 0) 0]
    [(>= n (expt 2 (sub1 size)))
     (add1 (sized-popcnt (sub1 size) (- n (expt 2 (sub1 size)))))]
    [else
     (sized-popcnt (sub1 size) n)]))
