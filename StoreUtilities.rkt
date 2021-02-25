#lang racket

(require redex/reduction-semantics
         "Syntax.rkt"
         "ConstUtilities.rkt")

; store and inst field accessor functions
;; implements s_func(i,j)
(define-metafunction WASMrt
  store-func : s i j -> cl
  [(store-func ((inst ...) _ _) i j)
   (inst-func (do-get (inst ...) i) j)])

;; implements inst_func(j)
(define-metafunction WASMrt
  inst-func : inst j -> cl
  [(inst-func ((cl ...) _ _ _) j)
   (do-get (cl ...) j)])

;; implements s_tab(i,j)
(define-metafunction WASMrt
  store-tab : s i j -> any
  [(store-tab ((inst ...) (tabinst ...) _) i j)
   (tab-func (do-get (tabinst ...) (inst-tab (do-get (inst ...) i))) j)])

;; implements tab_func(j)
(define-metafunction WASMrt
  tab-func : tabinst j -> any
  [(tab-func (cl ...) j)
   (do-get (cl ...) j)
   (side-condition (< (term j) (length (term (cl ...)))))
   or
   #f])

;; implements inst_tab
(define-metafunction WASMrt
  inst-tab : inst -> i
  [(inst-tab (_ _ (i) _)) i])

;; implements inst_mem
(define-metafunction WASMrt
  inst-mem : inst -> i
  [(inst-mem (_ _ _ (i))) i])

;; implements inst_glob(j)
(define-metafunction WASMrt
  inst-glob : inst j -> v
  [(inst-glob (_ (v ...) _ _) j)
   (do-get (v ...) j)])

;; implements "inst with glob(j)=v"
(define-metafunction WASMrt
  inst-with-glob : inst j v -> inst
  [(inst-with-glob ((cl ...) (v_g ...) (i_t ...) (i_m ...)) j v)
   ((cl ...) (do-set (v_g ...) j v) (i_t ...) (i_m ...))])

;; implements s_glob(i,j)
(define-metafunction WASMrt
  store-glob : s i j -> v
  [(store-glob ((inst ...) _ _) i j)
   (inst-glob (do-get (inst ...) i) j)])

;; implements "s with glob(i,j)=v"
(define-metafunction WASMrt
  store-with-glob : s i j v -> s
  [(store-with-glob ((inst ...) (tabinst ...) (meminst ...)) i j v)
   ((do-set (inst ...) i (inst-with-glob (do-get (inst ...) i) j v)) (tabinst ...) (meminst ...))])

;; implements s_mem(i)
(define-metafunction WASMrt
  store-mem : s i -> meminst
  [(store-mem ((inst ...) _ (meminst ...)) i)
   (do-get (meminst ...) (inst-mem (do-get (inst ...) i)))])

;; implements "s with mem(i, ...)=meminst_new"
(define-metafunction WASMrt
  store-with-mem : s i meminst -> s
  [(store-with-mem ((inst ...) (tabinst ...) (meminst ...)) i meminst_new)
   ((inst ...) (tabinst ...) (do-set (meminst ...) (inst-mem (do-get (inst ...) i)) meminst_new))])

;; implements s_mem(i,index,width)
(define-metafunction WASMrt
  mem-bytes : meminst natural natural -> (bstr ...)
  [(mem-bytes meminst (name offset natural_1) (name width natural_2))
   (,(subbytes (term meminst) (term offset) (+ (term offset) (/ (term width) 8))))
   (side-condition (<= (+ (term offset) (/ (term width) 8)) (bytes-length (term meminst))))
   or
   ()])

;; implements "s with mem(i,offset,width)=bstr" (where width=|bstr| for simplicity)
(define-metafunction WASMrt
  mem-with-bytes : meminst natural bstr -> (meminst ...)
  [(mem-with-bytes meminst (name offset natural) bstr)
   (,(bytes-append (subbytes (term meminst) 0 (term offset))
                   (term bstr)
                   (subbytes (term meminst) (+ (term offset) (bytes-length (term bstr))))))
   (side-condition (<= (+ (term offset) (bytes-length (term bstr))) (bytes-length (term meminst))))
   or
   ()])