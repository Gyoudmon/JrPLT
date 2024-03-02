#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out math/matrix))

(require digimon/spec)
(require digimon/predicate)

(require math/array)
(require math/matrix)

(require racket/list)
(require racket/function)
(require typed/racket/unsafe)

(require "matrix.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))
  
  (require racket/list)
  (require digimon/ffi)

  (require (submod "matrix.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-ffi-definer define-matrix (digimon-ffi-lib "flmatrix"))

  (define _flonum _double*)

  (define-matrix make_square_flmatrix
    (_fun [src : (_list i _flonum)]
          [_size = (length src)]
          -> _Matrix_ptr)
    #:wrap delete-matrix)

  (define-matrix flmatrix_equal (_fun _Matrix_ptr _Matrix_ptr _double* -> _bool))
  (define-matrix flmatrix_multiply (_fun _Matrix_ptr _Matrix_ptr -> _Matrix_ptr) #:wrap delete-matrix)
  (define-matrix flmatrix_permutation_expand (_fun _Matrix_ptr -> _Matrix_ptr) #:wrap delete-matrix)

  (define-matrix flmatrix_trace
    (_fun [src : (_list i _int)] [_size = (integer-sqrt (length src))]
          [fx : (_ptr o _int64)] [fl : (_ptr o _double)] [dl : (_ptr o _double)]
          -> [ok : _bool]
          -> (values fx fl dl)))

  (define-matrix flmatrix_determinant
    (_fun [src : (_list i _int)] [_size = (integer-sqrt (length src))]
          [fx : (_ptr o _int64)] [fl : (_ptr o _double)] [dl : (_ptr o _double)]
          -> [ok : _bool]
          -> (values fx fl dl)))

  (define-matrix flmatrix_lu_decomposite
    (_fun [src : (_list i _flonum)] [_size = (length src)]
          [L : (_ptr o _Matrix_gc_ptr)]
          [U : (_ptr o _Matrix_gc_ptr)]
          -> [ok : _bool]
          -> (values ok L U)))

  (define-matrix flmatrix_lup_decomposite
    (_fun [src : (_list i _flonum)] [_size = (length src)]
          [L : (_ptr o _Matrix_gc_ptr)]
          [U : (_ptr o _Matrix_gc_ptr)]
          [P : (_ptr o _Matrix_gc_ptr)]
          -> [ok : _bool]
          -> (values ok L U P))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [make_square_flmatrix (-> (Listof Real) CppMatrix)]

 [flmatrix_equal (-> CppMatrix CppMatrix Real Boolean)]
 [flmatrix_multiply (-> CppMatrix CppMatrix CppMatrix)]
 [flmatrix_permutation_expand (-> CppMatrix CppMatrix)]
 
 [flmatrix_trace (-> (Listof Integer) (Values Integer Flonum Flonum))]
 [flmatrix_determinant (-> (Listof Integer) (Values Integer Flonum Flonum))]

 [flmatrix_lu_decomposite (-> (Listof Real) (Values Boolean CppMatrix CppMatrix))]
 [flmatrix_lup_decomposite (-> (Listof Real) (Values Boolean CppMatrix CppMatrix CppMatrix))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define matrix-racket->cpp : (-> (Matrix Real) CppMatrix)
  (lambda [mtx]
    (make_square_flmatrix (matrix->list mtx))))

(define flmatrix-equal : (->* (CppMatrix CppMatrix) (Flonum) Boolean)
  (lambda [m1 m2 [epsilon 0.0001]]
    (flmatrix_equal m1 m2 epsilon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-spec-boolean-expectation (lu-factors [A : CppMatrix] [L : CppMatrix] [U : CppMatrix])
  (flmatrix-equal A (flmatrix_multiply L U)))

(define-spec-boolean-expectation (lup-factors [A : CppMatrix] [L : CppMatrix] [U : CppMatrix] [P : CppMatrix])
  (flmatrix-equal (flmatrix_multiply (flmatrix_permutation_expand P) A)
                  (flmatrix_multiply L U)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-tame-matrix/LUD mtx.rkt)
  (let*-values ([(Lrkt Urkt) (matrix-lu (matrix-map real->double-flonum mtx.rkt) (λ [] #false))])
    #:it
    ["should fail if given ~a" (matrix->list* mtx.rkt)] #:when (not Lrkt)
    ["should return triangular matrices L and U if given ~a" (matrix->list* mtx.rkt)]
    #:do
    (let-values ([(okay? Lpp Upp) (flmatrix_lu_decomposite (matrix->list mtx.rkt))])
      (if (and Lrkt)
          (begin (expect-is flmatrix-equal Lpp (matrix-racket->cpp Lrkt) "failed to construct the lower triangular matrix")
                 (expect-is flmatrix-equal Upp (matrix-racket->cpp Urkt) "failed to construct the upper triangular matrix")
                 (expect-lu-factors (matrix-racket->cpp mtx.rkt) Lpp Upp "A != LU"))
          (expect-false okay?)))))

(define-behavior (it-tame-matrix/LUPD mtx.rkt)
  (let*-values ([(okay? L U P) (flmatrix_lup_decomposite (matrix->list mtx.rkt))])
    #:it
    ["should fail if given ~a" (matrix->list* mtx.rkt)] #:when (not okay?)
    ["should return triangular matrices L and U plus a permutation matrix if given ~a" (matrix->list* mtx.rkt)]
    #:do
    (when (and okay?)
      (expect-lup-factors (matrix-racket->cpp mtx.rkt) L U P "PA != LU"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-tame-matrix/tr pool order)
  (let* ([fxentries (take-right pool (* order order))]
         [flentries (map exact->inexact fxentries)]
         [mtx.rkt (list->matrix order order fxentries)]
         [fxtr (matrix-trace mtx.rkt)]
         [fltr (real->double-flonum fxtr)])
    #:it ["should return ~a if given ~a" fxtr (matrix->list* mtx.rkt)]
    #:do
    (let-values ([(fixnum-tr double-tr float-tr) (flmatrix_trace fxentries)])
      (expect-= fixnum-tr fxtr "we have trouble in finding the trace for fixnum matrix")
      (expect-= double-tr fltr "we have trouble in finding the trace for flonum matrix")
      (expect-= float-tr double-tr "float is as less precise as double"))))

(define-behavior (it-tame-matrix/det pool order)
  (let* ([fxentries (take pool (* order order))]
         [flentries (map exact->inexact fxentries)]
         [mtx.rkt (list->matrix order order fxentries)]
         [fxdet (matrix-determinant mtx.rkt)]
         [fldet (real->double-flonum fxdet)])
    #:it ["should return ~a if given ~a" fxdet (matrix->list* mtx.rkt)]
    #:do
    (let-values ([(fixnum-det float-det double-det) (flmatrix_determinant fxentries)])
      (expect-= fixnum-det fxdet "we have trouble in finding the determinant for fixnum matrix")
      (expect-= double-det fldet "we have trouble in finding the determinant for flonum matrix")
      (expect-= float-det double-det "float is as less precise as double"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define random-entries : (Listof Integer)
  (build-list 64 (λ [i] (random 256))))

(define natural-entries : (Listof Integer) (range 64))

(define mtx4x4.rkt : (Matrix Integer)
  (list->matrix 4 4 (take random-entries 16)))

(define null-mtx : (Matrix Integer)
  (matrix [[0 0 0 0]
           [0 0 0 0]
           [0 0 0 0]
           [0 0 0 0]]))

(define lud-mtx : (Matrix Integer)
  (matrix [[2  3  1  5]
           [6 13  5 19]
           [2 19 10 23]
           [4 10 11 31]]))

(define lup-mtx : (Matrix Flonum)
  (matrix [[ 2.0  0.0 2.0  0.6]
           [ 3.0  3.0 4.0 -2.0]
           [ 5.0  5.0 4.0  2.0]
           [-1.0 -2.0 3.4 -1.0]]))
