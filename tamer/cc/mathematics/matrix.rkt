#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out math/matrix))

(require digimon/spec)
(require digimon/format)
(require digimon/predicate)

(require math/array)
(require math/matrix)

(require racket/list)
(require racket/function)
(require typed/racket/unsafe)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))
  (provide (rename-out [cpointer*? fxmatrix?]
                       [cpointer*? flmatrix?]))

  (require racket/list)
  
  (require digimon/ffi)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define matrix.dylib (digimon-ffi-lib "matrix"))
  (define-ffi-definer define-matrix matrix.dylib)

  (define _Matrix_ptr (_cpointer 'MatrixTop))

  (define-matrix destroy_matrix (_fun _Matrix_ptr -> _void) #:wrap (deallocator))
  (define delete-matrix (allocator destroy_matrix))

  (define-matrix make_null_square_fxmatrix
    (_fun -> _Matrix_ptr)
    #:wrap delete-matrix)

  (define-matrix make_square_fxmatrix
    (_fun [src : (_list i _int)]
          [_size = (length src)]
          -> _Matrix_ptr)
    #:wrap delete-matrix)

  (define-matrix make_square_fxmatrix_with_diagonal
    (_fun [src : (_list i _int)]
          [_size = (length src)]
          -> _Matrix_ptr)
    #:wrap delete-matrix)

  (define-matrix make_diagonal_fxmatrix
    (_fun _int -> _Matrix_ptr)
    #:wrap delete-matrix)

  (define make-square-flmatrix
    (let ([λs (make-hasheq)])
      (lambda [entries]
        (define order (length entries))
        
        (define (make-square-flmatrix)
          (define _array2d (_array/list _int #| <= yes, it's int |# order order))
          (define-matrix make_square_flmatrix_via_vector (_fun _array2d [_size = order] -> _Matrix_ptr) #:wrap delete-matrix)
          make_square_flmatrix_via_vector)
        
        ((hash-ref! λs order make-square-flmatrix) entries))))

  (define-matrix matrix_shape
    (_fun _Matrix_ptr
          [r : (_box _size) = (box 0)]
          [c : (_box _size) = (box 0)]
          -> [s : _size]
          -> (values s (unbox r) (unbox c))))

  (define-matrix fxmatrix_data
    (_fun _Matrix_ptr [dest1D : (_list o _int buffer-size)] [buffer-size : _size]
          -> [actual-size : _size]
          -> (take dest1D actual-size)))
  
  (define-matrix flmatrix_data
    (_fun _Matrix_ptr [dest2D : (_list o _float buffer-size)] [buffer-size : _size]
          -> [actual-size : _size]
          -> (take dest2D actual-size)))

  (define matrix-data
    (lambda [mtx]
      (define-values (size R C) (matrix_shape mtx))
      
      (if (matrix_is_fixnum mtx)
          (fxmatrix_data mtx size)
          (flmatrix_data mtx size))))

  (define matrix-data2d
    (lambda [mtx]
      (define-values (S R C) (matrix_shape mtx))
      (define _array2d (_array/list _float R C))
      
      (if (matrix_is_fixnum mtx)
          (let ()
            (define-matrix fxmatrix_data2d_row_by_row (_fun _Matrix_ptr [dest2D : (_ptr o _array2d)] _size -> _size -> dest2D))
            (fxmatrix_data2d_row_by_row mtx R))
          (let ()
            (define-matrix flmatrix_data2d_via_vector (_fun _Matrix_ptr [dest2D : (_ptr o _array2d)] _size -> _size -> dest2D))
            (flmatrix_data2d_via_vector mtx R)))))

  (define-matrix matrix_desc (_fun _Matrix_ptr -> _string))
  (define-matrix fxmatrix_equal (_fun _Matrix_ptr _Matrix_ptr -> _bool))
  (define-matrix flmatrix_equal (_fun _Matrix_ptr _Matrix_ptr -> _bool))
  (define-matrix matrix_is_fixnum (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_flonum (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_zero (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_diagonal (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_scalar (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_lower_triangular (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_upper_triangular (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_identity (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_symmetric (_fun _Matrix_ptr -> _bool))
  (define-matrix matrix_is_skew_symmetric (_fun _Matrix_ptr -> _bool))

  (define-matrix fxmatrix_trace (_fun [src : (_list i _int)] [_size = (integer-sqrt (length src))] -> _int))
  (define-matrix flmatrix_trace (_fun [src : (_list i _float)] [_size = (integer-sqrt (length src))] -> _float))
  (define-matrix fxmatrix_determinant (_fun [src : (_list i _int)] [_size = (integer-sqrt (length src))] -> _int))
  (define-matrix flmatrix_determinant (_fun [src : (_list i _float)] [_size = (integer-sqrt (length src))] -> _float)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CppMatrix (U FxMatrix FlMatrix))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [#:opaque FxMatrix fxmatrix?]
 [#:opaque FlMatrix flmatrix?]
 [make_null_square_fxmatrix (-> FxMatrix)]
 [make_square_fxmatrix (-> (Listof Integer) FxMatrix)]
 [make_square_fxmatrix_with_diagonal (-> (Listof Integer) FxMatrix)]
 [make_diagonal_fxmatrix (-> Integer FxMatrix)]
 [make-square-flmatrix (-> (Listof (Listof Integer)) FlMatrix)]
 [matrix_desc (-> FxMatrix String)]
 
 [fxmatrix_equal (-> FxMatrix FxMatrix Boolean)]
 [flmatrix_equal (-> FlMatrix FlMatrix Boolean)]
 [matrix_is_fixnum (-> CppMatrix Boolean : FxMatrix)]
 [matrix_is_flonum (-> CppMatrix Boolean : FlMatrix)]
 [matrix_is_zero (-> CppMatrix Boolean)]
 [matrix_is_diagonal (-> CppMatrix Boolean)]
 [matrix_is_scalar (-> CppMatrix Boolean)]
 [matrix_is_lower_triangular (-> CppMatrix Boolean)]
 [matrix_is_upper_triangular (-> CppMatrix Boolean)]
 [matrix_is_identity (-> CppMatrix Boolean)]
 [matrix_is_symmetric (-> CppMatrix Boolean)]
 [matrix_is_skew_symmetric (-> CppMatrix Boolean)]

 [matrix-data (-> CppMatrix (Listof Real))]
 [matrix-data2d (-> CppMatrix (Listof (Listof Flonum)))]

 [fxmatrix_trace (-> (Listof Integer) Integer)]
 [fxmatrix_determinant (-> (Listof Integer) Integer)]
 [flmatrix_trace (-> (Listof Flonum) Flonum)]
 [flmatrix_determinant (-> (Listof Flonum) Flonum)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define matrix-format : Spec-Issue-Format
  (lambda [para fallback-format]
    (cond [(fxmatrix? para) (matrix_desc para)]
          [else (fallback-format para)])))

(define matrix-racket->cpp : (-> (Matrix Integer) FxMatrix)
  (lambda [smtx]
    (make_square_fxmatrix (matrix->list smtx))))

(define #:forall (T) matrix-equal : (-> CppMatrix (U CppMatrix (Matrix T) (Listof T)) Boolean)
  (lambda [lhs rhs]
    (equal? (matrix-data lhs)
            (cond [(list? rhs) rhs]
                  [(array? rhs) (matrix->list rhs)]
                  [else (matrix-data rhs)]))))

(define #:forall (T) matrix-equal/2d : (-> CppMatrix (U CppMatrix (Matrix T) (Listof (Listof T))) Boolean)
  (lambda [lhs rhs]
    (equal? (matrix-data2d lhs)
            (cond [(list? rhs) rhs]
                  [(array? rhs) (matrix->list* rhs)]
                  [else (matrix-data2d rhs)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall(T F) array2d-shape : (case-> [(Listof T) -> (Values Index (Listof (Listof T)))]
                                              [(Listof T) (-> T F) -> (Values Index (Listof (Listof F)))])
  (case-lambda
    [(src) ((inst array2d-shape T T) src values)]
    [(src transform)
     (define size : Index (length src))
     (define order : Index (integer-sqrt size))
     
     (values order
             (matrix->list*
              (list->matrix order order
                            (map transform (take src (* order order))))))]))

(define #:forall (T F) array2d-shape* : (case-> [(Listof (Listof T)) Index Index T -> (Listof (Listof T))]
                                                [(Listof (Listof T)) Index Index T (-> T F) -> (Listof (Listof F))])
  (case-lambda
    [(entries order size zero) ((inst array2d-shape* T T) entries order size zero values)]
    [(entries order size zero transform)
     (build-list size
                 (λ [[row : Index]]
                   (map transform
                        (cond [(<= order row) (make-list size zero)]
                              [(< order size) (append (list-ref entries row) (make-list (- size order) zero))]
                              [else (take (list-ref entries row) size)]))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-tame-fxmatrix/diagonal entries)
  (let* ([diff-size (- 4 (length entries))]
         [diagonal (if (> diff-size 0) (append entries (make-list diff-size 0)) (take entries 4))]
         [given (make_square_fxmatrix_with_diagonal entries)]
         [expected (matrix-racket->cpp (diagonal-matrix diagonal))])
    #:it
    "should pad with 0 if given too fewer data" #:when (> diff-size 0)
    "should ignore the rest if given too many data" #:when (< diff-size 0)
    "should contain all given data as diagonal"
    #:do
    (expect-is fxmatrix_equal given expected)))

(define-behavior (it-tame-fxmatrix/entries entries)
  (let* ([diff-size (- 16 (length entries))]
         [expected-data (if (> diff-size 0) (append entries (make-list diff-size 0)) (take entries 16))]
         [given (make_square_fxmatrix entries)]
         [expected (make_square_fxmatrix expected-data)])
    #:it
    "should pad with 0 if given too fewer data" #:when (> diff-size 0)
    "should ignore the rest if given too many data" #:when (< diff-size 0)
    "should contain all given data as entries"
    #:do
    (expect-dissatisfy matrix_is_zero given "we might have trouble in constructing a fixnum matrix")
    (expect-satisfy matrix_is_fixnum given)
    (expect-dissatisfy matrix_is_flonum given)
    (expect-is fxmatrix_equal given expected)
    (expect-satisfy-any (negate zero?) (matrix-data given) "we might have trouble in extracting fixnum matrix data as an 1D-Array")
    (expect-satisfy-any (negate listof-zero?) (matrix-data2d given) "we might have trouble in extracting fixnum matrix data as a 2D-Array")
    (expect-is matrix-equal given expected  "we might have trouble in extracting fixnum matrix data as an 1D-Array")
    (expect-is matrix-equal/2d given expected  "we might have trouble in extracting fixnum matrix data as a 2D-Array row by row")))

(define-behavior (it-tame-flmatrix/entries entries)
  (let*-values ([(order squares) (array2d-shape entries)]
                [(given) (make-square-flmatrix squares)]
                [(expected) (make-square-flmatrix (array2d-shape* squares order 4 0))])
    #:it
    "should pad with 0.0 if given too fewer data" #:when (< order 4)
    "should ignore the rest if given too many data" #:when (> order 4)
    "should contain all given data as entries"
    #:do
    (expect-dissatisfy matrix_is_zero given "we might have trouble in constructing a flonum matrix")
    (expect-dissatisfy matrix_is_fixnum given)
    (expect-satisfy matrix_is_flonum given)
    (expect-is flmatrix_equal given expected)
    (expect-satisfy-any (negate zero?) (matrix-data given) "we might have trouble in extracting flonum matrix data as an 1D-Array")
    (expect-satisfy-any (negate listof-zero?) (matrix-data2d given) "we might have trouble in extracting flonum matrix data as a 2D-Array")
    (expect-is matrix-equal given expected "we might have trouble in extracting flonum matrix data as an 1D-Array row by row")
    (expect-is matrix-equal/2d given expected "we might have trouble in extracting flonum matrix data as a 2D-Array via std::vector")))

(define-behavior (it-tame-matrix/tr entries)
  (let* ([order (integer-sqrt (length entries))]
         [expected (matrix-trace (list->matrix order order entries))]
         [given ((if (exact-integer? (car entries)) fxmatrix_trace flmatrix_trace) entries)])
    #:it ["should be ~a if given ~a" expected entries]
    #:do (expect-= given expected)))

(define-behavior (it-tame-matrix/det entries)
  (let* ([order (integer-sqrt (length entries))]
         [expected (matrix-determinant (list->matrix order order entries))]
         [given ((if (exact-integer? (car entries)) fxmatrix_determinant flmatrix_derterminant) entries)])
    #:it ["should be ~a if given ~a" expected entries]
    #:do (expect-= given expected)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define random-entries : (Listof Integer)
  (build-list 32 (λ [i] (random 256))))

(define mtx4x4 : (Matrix Integer)
  (list->matrix 4 4 (take random-entries 16)))

(define mtx0 (make_null_square_fxmatrix))
(define rmtx (matrix-racket->cpp mtx4x4))
