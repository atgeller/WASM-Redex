#lang racket

(provide (all-defined-out))

; Racket functions to handle all of the signed and unsigned integer operations on integers of a particular size

(define (to-unsigned-sized size x)
  (modulo x (expt 2 size)))

(define (to-signed-sized size x)
  (let ([unsigned (to-unsigned-sized size x)])
    (if (< unsigned (expt 2 (sub1 size)))
        unsigned
        (- unsigned (expt 2 size)))))

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
