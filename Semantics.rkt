#lang racket

(require redex/reduction-semantics
         "Syntax.rkt"
         "Utilities.rkt")

(provide -> memory-page-size)

(define memory-page-size (make-parameter 65536))

;; TL;DR about stack: the stack is implicit in the stream of instructions being processed.
;; This is because v \subset e, so although we say (e ...) it ends up looking like (v ... e ...).
;; Thus, the next instruction to execute is the head of e ..., and the stack is v ...
(define (-> i)
  (reduction-relation
   WASMrt
   #:domain (s (v ...) (e ...))
   #:codomain (s (v ...) (e ...))
   #:arrow c->
   ;; (s j (v ...) (e ...)) -> (s j (v ...) (e ...))
   ;; s = Store
   ;; j = Current instance (runtime module representation) index
   ;; (v ...) = Local variables
   ;; (e ...) = Code (with implicit stack)

   ;; Every instruction is operating inside of an execution environment.
   ;; We need these contexts to keep track of the stack and next instructions at each level,
   ;; and to handle branching/returning from levels.
   ;; Much of the instructions don't need to know about the environment they are executing in, but some do.
   ;; Todo: It would be nice to reduce the boilerplate for those that don't.

   (c-> (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...)))
        (s (v_l ...) (in-hole L (trap))))

   ;; Due to validation we can be sure we are returning the proper number of values
   (c-> (s (v_l ...) (in-hole L (v_0 ... (t const c) (t unop) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (t const (eval-unop unop t c)) e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (t const c_1) (t const c_2) (t binop) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (t const c) e_0 ...)))
        (where (c) (eval-binop binop t c_1 c_2)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (t const c_1) (t const c_2) (t binop) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...)))
        (where () (eval-binop binop t c_1 c_2)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (t const c) (t testop) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (t const (eval-testop testop t c)) e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (t const c_1) (t const c_2) (t relop) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (i32 const (eval-relop relop t c_1 c_2)) e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 convert t_1) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (t_2 const c_new) e_0 ...)))
        (where (c_new) (do-convert t_1 t_2 () c)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 convert t_1) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...)))
        (where () (do-convert t_1 t_2 () c)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 convert t_1 sx) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (t_2 const c_new) e_0 ...)))
        (where (c_new) (do-convert t_1 t_2 (sx) c)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 convert t_1 sx) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...)))
        (where () (do-convert t_1 t_2 (sx) c)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 reinterpret t_1) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (t_2 const (bstr->const t_2 (const->bstr t_1 c))) e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... unreachable e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... nop e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... v_2 drop e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... v_1 v_2 (i32 const 0) select e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... v_2 e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... v_1 v_2 (i32 const c) select e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... v_1 e_0 ...)))
        (side-condition (> (term c) 0)))

   ;; Control flow!
   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const 0) (if tf (e_1 ...) else (e_2 ...)) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (block tf (e_2 ...)) e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const c) (if tf (e_1 ...) else (e_2 ...)) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (block tf (e_1 ...)) e_0 ...)))
        (side-condition (> (term c) 0)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... v ... (block ((t_1 ...) -> (t_2 ...)) (e ...)) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (label m () (v ... e ...)) e_0 ...)))
        (side-condition (= (length (term (t_1 ...))) (length (term (v ...)))))
        (where m ,(length (term (t_2 ...)))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... v ... (loop ((t_1 ...) -> (t_2 ...)) (e ...)) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (label n ((loop ((t_1 ...) -> (t_2 ...)) (e ...))) (v ... e ...)) e_0 ...)))
        (side-condition (= (length (term (t_1 ...))) (length (term (v ...)))))
        (where n ,(length (term (t_1 ...)))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (label n (e ...) (v ...)) e_0 ...)))
        (s (v_l ...) (v_0 ... v ... e_0 ...)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (label n (e ...) (trap)) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...))))

   (c-> (s (v_l ...) (in-hole L_1 (v_1 ... (label n (e ...) (in-hole L_2 (v_0 ... v ... (br j) e_0 ...))) e_1 ...)))
        (s (v_l ...) (in-hole L_1 (v_1 ... v ... e ... e_1 ...)))
        (where j (context-depth L_2))
        (where n ,(length (term (v ...)))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const 0) (br-if j) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const c) (br-if j) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (br j) e_0 ...)))
        (side-condition (> (term c) 0)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const c) (br-table j ...) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (br (do-get (j ...) c)) e_0 ...)))
        (side-condition (< (term c) (length (term (j ...))))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const c) (br-table j_1 ... j) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (br j) e_0 ...)))
        (side-condition (> (term c) (length (term (j_1 ...))))))

   ;; Locals!
   (c-> (s (v_l ...) (in-hole L (v_0 ... (get-local j) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (do-get (v_l ...) j) e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... v (set-local j) e_0 ...)))
        (s (do-set (v_l ...) j v) (in-hole L (v_0 ... e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... v (tee-local j) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... v v (set-local j) e_0 ...))))

   ;; Globals!
   (c-> (s (v_l ...) (in-hole L (v_0 ... (get-global j) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (store-glob s ,i j) e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... v (set-global j) e_0 ...)))
        (s_new (v_l ...) (in-hole L (v_0 ... e_0 ...)))
        (where s_new (store-with-glob s ,i j v)))

   ;; Function calls!
   (c-> (s (v_l ...) (in-hole L (v_0 ... (call j) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (call (store-func s ,i j)) e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const j) (call-indirect tf) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (call (store-tab s ,i j)) e_0 ...)))
        (where (func tf (local (t ...) (e ...))) (cl-code-opt (store-tab s ,i j))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const j) (call-indirect tf) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...)))
        (side-condition 'otherwise)
        (side-condition/hidden
         (match (term (cl-code-opt (store-tab s ,i j)))
           [#f #t]
           [`(func ,tf-act ,_)
            (not (equal? tf-act (term tf)))])))

   (c-> (s (v_l ...) (in-hole L (v_0 ... v ... (call cl) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ...
                                 (local m
                                   ((cl-inst cl) (v ... (t const 0) ...))
                                   ((block (() -> (t_2 ...)) (e ...))))
                                 e_0 ...)))
        (where (func ((t_1 ...) -> (t_2 ...)) (local (t ...) (e ...))) (cl-code cl))
        (side-condition (= (length (term (v ...))) (length (term (t_1 ...)))))
        (where m ,(length (term (t_2 ...)))))

   ;; Stuff inside functions calls!
   (c-> (s (v_l ...) (in-hole L (v_0 ... (local n (j (v_1 ...)) (v ...)) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... v ... e_0 ...)))
        (where n ,(length (term (v ...)))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (local n (j (v_1 ...)) (trap)) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...))))

   (c-> (s (v_l ...) (in-hole L_1 (v_0 ... (local n (j (v_1 ...)) (in-hole L_2 (v_2 ... v ... return e_2 ...))) e_0 ...)))
        (s (v_l ...) (in-hole L_1 (v_0 ... v ... e_0 ...)))
        (where n ,(length (term (v ...)))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (local n (j (v ...)) (e ...)) e_0 ...)))
        (s_new (v_l ...) (in-hole L (v_0 ... (local n (j (v_new ...)) (e_new ...)) e_0 ...)))
        (where (_ ... (s_new (v_new ...) (e_new ...)) _ ...)
               ,(apply-reduction-relation (-> (term j)) (term (s (v ...) (e ...))))))

   ;; Memory!
   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) (t load a o) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (t const (bstr->const t bstr)) e_0 ...)))
        (where (bstr) (mem-bytes (store-mem s ,i) ,(+ (term k) (term o)) (bit-width t))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) (t load a o) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...)))
        (where () (mem-bytes (store-mem s ,i) ,(+ (term k) (term o)) (bit-width t))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) (t load (tp sx) a o) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (t const (packed-bstr->const t sx bstr)) e_0 ...)))
        (where (bstr) (mem-bytes (store-mem s ,i) ,(+ (term k) (term o)) (packed-bit-width tp))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) (t load (tp sx) a o) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...)))
        (where () (mem-bytes (store-mem s ,i) ,(+ (term k) (term o)) (packed-bit-width tp))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) (t const c) (t store a o) e_0 ...)))
        (s_new (v_l ...) (in-hole L (v_0 ... e_0 ...)))
        (where (meminst) (mem-with-bytes (store-mem s ,i) ,(+ (term k) (term o)) (const->bstr t c)))
        (where s_new (store-with-mem s ,i meminst)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) (t const c) (t store a o) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...)))
        (where () (mem-with-bytes (store-mem s ,i) ,(+ (term k) (term o)) (const->bstr t c))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) (t const c) (t store tp a o) e_0 ...)))
        (s_new (v_l ...) (in-hole L (v_0 ... e_0 ...)))
        (where (meminst) (mem-with-bytes (store-mem s ,i) ,(+ (term k) (term o)) (const->packed-bstr t (packed-bit-width tp) c)))
        (where s_new (store-with-mem s ,i meminst)))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) (t const c) (t store tp a o) e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... trap e_0 ...)))
        (where () (mem-with-bytes (store-mem s ,i) ,(+ (term k) (term o)) (const->packed-bstr t (packed-bit-width tp) c))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... current-memory e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (i32 const ,(/ (bytes-length (term (store-mem s ,i))) (memory-page-size))) e_0 ...))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) grow-memory e_0 ...)))
        (s_new (v_l ...) (in-hole L (v_0 ... (i32 const ,(/ (bytes-length (term (store-mem s ,i))) (memory-page-size))) e_0 ...)))
        (where s_new (store-with-mem s ,i ,(bytes-append (term (store-mem s ,i)) (make-bytes (* (memory-page-size) (term k)) 0)))))

   (c-> (s (v_l ...) (in-hole L (v_0 ... (i32 const k) grow-memory e_0 ...)))
        (s (v_l ...) (in-hole L (v_0 ... (i32 const #xFFFFFFFF) e_0 ...))))))
