#lang racket

(require redex/reduction-semantics
         "Syntax.rkt"
         "Utilities.rkt"
         "Bits.rkt")

(provide ->)

;; TL;DR about stack: the stack is implicit in the stream of instructions being processed.
;; This is because v \subset e, so although we say (e ...) it ends up looking like (v ... e ...).
;; Thus, the next instruction to execute is the head of e ..., and the stack is v ...
(define ->
    (reduction-relation
     WASMrt
     #:domain (s i (v ...) (e ...))
     #:codomain (s i (v ...) (e ...))
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
     
     ;; Due to validation we can be sure we are returning the proper number of values
     (--> (s i (v_l ...) (in-hole L (v_0 ... (t const c) (t unop) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (t const (eval-unop unop t c)) e_0 ...))))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... (t const c_1) (t const c_2) (t binop) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (t const c) e_0 ...)))
          (where (c) (eval-binop binop t c_1 c_2)))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... (t const c_1) (t const c_2) (t binop) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (trap) e_0 ...)))
          (where () (eval-binop binop t c_1 c_2)))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... (t const c) (t testop) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (t const (eval-testop testop t c)) e_0 ...))))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... (t const c_1) (t const c_2) (t relop) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (i32 const (eval-relop relop t c_1 c_2)) e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 convert t_1) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (t_2 const c_new) e_0 ...)))
          (where (c_new) (do-convert t_1 t_2 #f c)))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 convert t_1) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (trap) e_0 ...)))
          (where () (eval-convert t_1 t_2 #f c)))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 convert t_1 sx) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (t_2 const c_new) e_0 ...)))
          (where (c_new) (do-convert t_1 t_2 sx c)))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 convert t_1 sx) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (trap) e_0 ...)))
          (where () (do-convert t_1 t_2 sx c)))
          
     (--> (s i (v_l ...) (in-hole L (v_0 ... (t_1 const c) (t_2 reinterpret t_1) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (t_2 const (bstr->const t_2 (const->bstr t_1 c))) e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (unreachable) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (trap) e_0 ...))))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... (nop) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... e_0 ...))))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... v_2 (drop) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... v_1 v_2 (i32 const 0) (select) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... v_2 e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... v_1 v_2 (i32 const c) (select) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... v_1 e_0 ...)))
          (side-condition (> (term c) 0)))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const 0) (if tf (e_1 ...) else (e_2 ...)) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (block tf (e_2 ...)) e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const c) (if tf (e_1 ...) else (e_2 ...)) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (block tf (e_1 ...)) e_0 ...)))
          (side-condition (> (term c) 0)))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (block ((t_1 ...) -> (t_2 ...)) (e ...)) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_1 ... (label m () (v_2 ... e ...)) e_0 ...)))
          (where (v_1 ...) ,(drop-right (term (v_0 ...)) (length (term (t_1 ...)))))
          (where (v_2 ...) ,(take-right (term (v_0 ...)) (length (term (t_1 ...)))))
          (where m ,(length (term (t_2 ...)))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (loop ((t_1 ...) -> (t_2 ...)) (e ...)) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_1 ... (label n ((loop ((t_1 ...) -> (t_2 ...)) (e ...))) (v_2 ... e ...)) e_0 ...)))
          (where (v_1 ...) ,(drop-right (term (v_0 ...)) (length (term (t_1 ...)))))
          (where (v_2 ...) ,(take-right (term (v_0 ...)) (length (term (t_1 ...)))))
          (where n ,(length (term (t_1 ...)))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (label n (e ...) ((trap))) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (trap) e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (trap) e_0 ...)))
          (s i (v_l ...) (in-hole L ((trap)))))

     ; Knowing about contexts is necessary for this (so can't shortcut the rest :/)!
     (--> (s i (v_l ...) (in-hole L (v_0 ... (br j) e_0 ...)))
          (s i (v_l ...) (decompose L j (v_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const 0) (br-if j) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const c) (br-if j) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (br j) e_0 ...)))
          (side-condition (> (term c) 0)))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const c) (br-table (j ...)) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (br (do-get (j ...) c)) e_0 ...)))
          (side-condition (<= (term c) (length (term (j ...))))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const c) (br-table (j_1 ... j)) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (br j) e_0 ...)))
          (side-condition (> (term c) (length (term (j_1 ...))))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (label n (e ...) (v ...)) e_0 ...)))
          (s i (v_l ...) (v_0 ... v ... e_0 ...)))

     ;; Locals!
     (--> (s i (v_l ...) (in-hole L (v_0 ... (get-local j) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (do-get (v_l ...) j) e_0 ...))))
     
     (--> (s i (v_l ...) (in-hole L (v_0 ... v (set-local j) e_0 ...)))
          (s i (do-set (v_l ...) j v) (in-hole L (v_0 ... e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... v (tee-local j) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... v v (set-local j) e_0 ...))))

     ;; Store stuff!
     (--> (((inst ...) (tabinst ...) (meminst ...)) i (v_l ...) (in-hole L (v_0 ... (get-global j) e_0 ...)))
          (((inst ...) (tabinst ...) (meminst ...)) i (v_l ...) (in-hole L (v_0 ... (do-global-get (inst ...) i j) e_0 ...))))

     (--> (((inst ...) (tabinst ...) (meminst ...)) i (v_l ...) (in-hole L (v_0 ... v (set-global j) e_0 ...)))
          (((do-global-set (inst ...) i j v) (tabinst ...) (meminst ...)) i (v_l ...) (in-hole L (v_0 ... e_0 ...))))

     ; Function calls
     (--> (((inst ...) (tabinst ...) (meminst ...)) i (v_l ...) (in-hole L (v_0 ... (call j) e_0 ...)))
          (((inst ...) (tabinst ...) (meminst ...)) i (v_l ...) (in-hole L (v_0 ... (call (function-lookup (inst ...) i j)) e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (call cl) e_0 ...)))
          (s i (v_l ...) (in-hole L (setup-call (v_0 ...) cl (e_0 ...)))))

     ; Stuff inside functions calls
     ;; NOTE: This is fun decomposition...
     (--> (s i (v_l ...) (in-hole L_1 (v_0 ... (local n (j (v_1 ...)) (in-hole L_2 (v_2 ... (return) e_2 ...))) e_0 ...)))
          (s i (v_l ...) (in-hole L_1 (v_0 ... v_3 ... e_0 ...)))
          (where (v_3 ...) ,(take-right (term (v_2 ...)) (term n))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (local n (j (v_1 ...)) ((trap))) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (trap) e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (local n (j (v_1 ...)) (v_2 ...)) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... v_2 ... e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (local n (j (v ...)) (e ...)) e_0 ...)))
          (s_new i (v_l ...) (in-hole L (v_0 ... (local n (j (v_new ...)) (e_new ...)) e_0 ...)))
          (where (_ ... (s_new j (v_new ...) (e_new ...)) _ ...)
                 ,(apply-reduction-relation -> (term (s j (v ...) (e ...))))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const j) (call-indirect tf) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (handle-call-indirect s i j tf) e_0 ...))))

     ; Memory instructions
     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const k) (t load a o) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (do-load s i t a ,(+ (term o) (term k))) e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const k) (t load (tp sx) a o) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (do-load-packed s i t a ,(+ (term o) (term k)) tp sx) e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const k) (t const c) (t store a o) e_0 ...)))
          (s_new i (v_l ...) (in-hole L (v_0 ... e_new ... e_0 ...)))
          (where (s_new (e_new ...)) (do-store s i t a ,(+ (term o) (term k)) c)))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const k) (t const c) (t store (tp) a o) e_0 ...)))
          (s_new i (v_l ...) (in-hole L (v_0 ... e_new ... e_0 ...)))
          (where (s_new (e_new ...)) (do-store-packed s i t a ,(+ (term o) (term k)) c tp)))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (current-memory) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (i32 const ,(/ (bytes-length (term (store-mem s i))) (memory-page-size))) e_0 ...))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const k) (grow-memory) e_0 ...)))
          (s_new i (v_l ...) (in-hole L (v_0 ... (i32 const ,(/ (bytes-length (term (store-mem s i))) (memory-page-size))) e_0 ...)))
          (where s_new (with-mem s i ,(bytes-append (term (store-mem s i)) (make-bytes (* (memory-page-size) (term k)) 0)))))

     (--> (s i (v_l ...) (in-hole L (v_0 ... (i32 const k) (grow-memory) e_0 ...)))
          (s i (v_l ...) (in-hole L (v_0 ... (i32 const #xFFFFFFFF) e_0 ...))))))
