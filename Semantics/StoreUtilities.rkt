#lang racket

(require redex/reduction-semantics
         "../Utilities.rkt"
         "AdministrativeSyntax.rkt")

(provide (all-defined-out))

; closure accessors
;; implements cl_code
(define-metafunction WASM-Admin
  cl-code : cl -> any
  [(cl-code (i (func tf (local (t ...) (e ...)))))
   (func tf (local (t ...) (e ...)))]
  [(cl-code (-1 (func tf (racket any))))
   (func tf (racket any))])

;; returns the cl_code field if the input is a well-defined cl
;; otherwise returns #f
(define-metafunction WASM-Admin
  cl-code-opt : any -> any
  [(cl-code-opt (i (func tf (local (t ...) (e ...)))))
   (func tf (local (t ...) (e ...)))]
  [(cl-code-opt _) #f])

;; implements cl_inst
(define-metafunction WASM-Admin
  cl-inst : cl -> i
  [(cl-inst (i (func tf (local (t ...) (e ...)))))
   i])

; store and inst field accessor functions
;; implements s_func(i,j)
(define-metafunction WASM-Admin
  store-func : s i j -> cl
  [(store-func ((inst ...) _ _) i j)
   (inst-func (index (inst ...) i) j)])

;; implements inst_func(j)
(define-metafunction WASM-Admin
  inst-func : inst j -> cl
  [(inst-func ((cl ...) _ _ _) j)
   (index (cl ...) j)])

;; implements s_tab(i,j)
(define-metafunction WASM-Admin
  store-tab : s i j -> any
  [(store-tab ((inst ...) (tabinst ...) _) i j)
   (tab-func (index (tabinst ...) (inst-tab (index (inst ...) i))) j)])

;; implements tab_func(j)
(define-metafunction WASM-Admin
  tab-func : tabinst j -> any
  [(tab-func (cl ...) j)
   (index (cl ...) j)
   (side-condition (< (term j) (length (term (cl ...)))))
   or
   #f])

;; implements inst_tab
(define-metafunction WASM-Admin
  inst-tab : inst -> i
  [(inst-tab (_ _ (i) _)) i])

;; implements inst_mem
(define-metafunction WASM-Admin
  inst-mem : inst -> i
  [(inst-mem (_ _ _ (i))) i])

;; implements inst_glob(j)
(define-metafunction WASM-Admin
  inst-glob : inst j -> v
  [(inst-glob (_ (v ...) _ _) j)
   (index (v ...) j)])

;; implements "inst with glob(j)=v"
(define-metafunction WASM-Admin
  inst-with-glob : inst j v -> inst
  [(inst-with-glob ((cl ...) (v_g ...) (i_t ...) (i_m ...)) j v)
   ((cl ...) (with-index (v_g ...) j v) (i_t ...) (i_m ...))])

;; implements s_glob(i,j)
(define-metafunction WASM-Admin
  store-glob : s i j -> v
  [(store-glob ((inst ...) _ _) i j)
   (inst-glob (index (inst ...) i) j)])

;; implements "s with glob(i,j)=v"
(define-metafunction WASM-Admin
  store-with-glob : s i j v -> s
  [(store-with-glob ((inst ...) (tabinst ...) (meminst ...)) i j v)
   ((with-index (inst ...) i (inst-with-glob (index (inst ...) i) j v)) (tabinst ...) (meminst ...))])

;; implements s_mem(i)
(define-metafunction WASM-Admin
  store-mem : s i -> meminst
  [(store-mem ((inst ...) _ (meminst ...)) i)
   (index (meminst ...) (inst-mem (index (inst ...) i)))])

;; implements "s with mem(i, ...)=meminst_new"
(define-metafunction WASM-Admin
  store-with-mem : s i meminst -> s
  [(store-with-mem ((inst ...) (tabinst ...) (meminst ...)) i meminst_new)
   ((inst ...) (tabinst ...) (with-index (meminst ...) (inst-mem (index (inst ...) i)) meminst_new))])

;; implements s_mem(i,index,width)
(define-metafunction WASM-Admin
  mem-bytes : meminst natural natural -> (bstr ...)
  [(mem-bytes meminst (name offset natural_1) (name width natural_2))
   (,(subbytes (term meminst) (term offset) (+ (term offset) (/ (term width) 8))))
   (side-condition (<= (+ (term offset) (/ (term width) 8)) (bytes-length (term meminst))))
   or
   ()])

;; implements "s with mem(i,offset,width)=bstr" (where width=|bstr| for simplicity)
(define-metafunction WASM-Admin
  mem-with-bytes : meminst natural bstr -> (meminst ...)
  [(mem-with-bytes meminst (name offset natural) bstr)
   (,(bytes-append (subbytes (term meminst) 0 (term offset))
                   (term bstr)
                   (subbytes (term meminst) (+ (term offset) (bytes-length (term bstr))))))
   (side-condition (<= (+ (term offset) (bytes-length (term bstr))) (bytes-length (term meminst))))
   or
   ()])