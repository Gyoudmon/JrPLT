#lang typed/racket/base

(provide (all-defined-out))

(require digimon/spec)
(require digimon/predicate)

(require racket/string)

(require typed/racket/unsafe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (except-out (all-defined-out) define-matrix))
  (provide (rename-out [cpointer*? cpp_matrix?]))

  (require digimon/ffi)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-ffi-definer define-matrix (digimon-ffi-lib "matrix"))

  (define _Matrix_ptr (_cpointer 'MatrixTop))
  
  (define-matrix matrix_destroy (_fun _Matrix_ptr -> _void) #:wrap (deallocator))
  (define delete-matrix (allocator matrix_destroy))

  (define _Matrix_gc_ptr (make-ctype/release _Matrix_ptr delete-matrix))

  (define-matrix matrix_desc (_fun _Matrix_ptr _bool -> _string))
  (define-matrix matrix_is_fixnum (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_flonum (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_zero (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_square (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_diagonal (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_scalar (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_lower_triangular (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_upper_triangular (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_identity (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_symmetric (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_skew_symmetric (_fun _Matrix_ptr -> _bool))
  
  (define-matrix matrix_is_row_echelon_form (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_row_canonical_form (_fun _Matrix_ptr -> _bool))

  (define-matrix matrix_shape
    (_fun _Matrix_ptr
          [r : (_box _size) = (box 0)]
          [c : (_box _size) = (box 0)]
          -> [s : _size]
          -> (values s (unbox r) (unbox c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [#:opaque CppMatrix cpp_matrix?]
 
 [matrix_desc (-> CppMatrix Boolean String)]
 [matrix_shape (-> CppMatrix (Values Index Index Index))]
 
 [matrix_is_fixnum (-> CppMatrix Boolean)]
 [matrix_is_flonum (-> CppMatrix Boolean)]
 [matrix_is_zero (-> CppMatrix Boolean)]
 [matrix_is_square (-> CppMatrix Boolean)]
 [matrix_is_diagonal (-> CppMatrix Boolean)]
 [matrix_is_scalar (-> CppMatrix Boolean)]
 [matrix_is_lower_triangular (-> CppMatrix Boolean)]
 [matrix_is_upper_triangular (-> CppMatrix Boolean)]
 [matrix_is_identity (-> CppMatrix Boolean)]
 [matrix_is_symmetric (-> CppMatrix Boolean)]
 [matrix_is_skew_symmetric (-> CppMatrix Boolean)]
 [matrix_is_row_echelon_form (-> CppMatrix Boolean)]
 [matrix_is_row_canonical_form (-> CppMatrix Boolean)])


(define matrix-oneline-desc : (-> CppMatrix String)
  (lambda [mtx]
    (matrix_desc mtx #true)))

(define matrix-desc : (->* (CppMatrix) (Boolean) String)
  (lambda [mtx [one-line? #false]]
    (matrix_desc mtx one-line?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define matrix-format : Spec-Issue-Format
  (lambda [para fallback-format]
    (cond [(cpp_matrix? para) (matrix-desc para)]
          [else (fallback-format para)])))
