#lang racket

(require redex)

(provide (all-defined-out))

(define empty-context (term ((func ()) (global ()) (table) (memory) (local ()) (label ()) (return))))
(define context1 (term ((func ((() -> (i32))))
                        (global ())
                        (table) (memory) (local ()) (label ()) (return))))
(define context2 (term ((func ((() -> (i32)) (() -> (i32))))
                        (global ())
                        (table) (memory) (local ()) (label ()) (return))))
(define context3 (term ((func ())
                        (global (i32))
                        (table) (memory) (local ()) (label ()) (return))))
(define context4 (term ((func ())
                        (global (i32 i32))
                        (table) (memory) (local ()) (label ()) (return))))