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
        
        (define (make-flmatrix)
          (define _array2d (_array/list _int #| <= yes, it's int |# order order))
          (define-matrix make_square_flmatrix_via_vector (_fun _array2d [_size = order] -> _Matrix_ptr) #:wrap delete-matrix)
          make_square_flmatrix_via_vector)
        
        ((hash-ref! λs order make-flmatrix) entries))))

  (define make-rectangular-fxmatrix
    (let ([λs (make-hash)])
      (lambda [entries]
        (define R (length entries))
        (define C (length (car entries)))
        
        (define (make-fxmatrix)
          (define _array2d (_array/list _int R C))
          (define-matrix make_rectangular_fxmatrix (_fun _array2d [_size = R] [_size = C] -> _Matrix_ptr) #:wrap delete-matrix)
          make_rectangular_fxmatrix)
        
        ((hash-ref! λs (cons R C) make-fxmatrix) entries))))

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
      
      (cond [(not (= R C))
             (define-matrix fxmatrix_data2d_via_triangle (_fun _Matrix_ptr [dest2D : (_ptr o _array2d)] _size _size -> _size -> dest2D))
             (fxmatrix_data2d_via_triangle mtx R C)]
            [(matrix_is_flonum mtx)
             (define-matrix flmatrix_data2d_via_vector (_fun _Matrix_ptr [dest2D : (_ptr o _array2d)] _size -> _size -> dest2D))
             (flmatrix_data2d_via_vector mtx R)]
            [else
             (define-matrix fxmatrix_data2d_row_by_row (_fun _Matrix_ptr [dest2D : (_ptr o _array2d)] _size -> _size -> dest2D))
             (fxmatrix_data2d_row_by_row mtx R)])))

  (define-matrix matrix_desc (_fun _Matrix_ptr -> _string))
  (define-matrix fxmatrix_equal (_fun _Matrix_ptr _Matrix_ptr -> _bool))
  (define-matrix flmatrix_equal (_fun _Matrix_ptr _Matrix_ptr -> _bool))
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

  (define-matrix matrix_trace
    (_fun [src : (_list i _int)] [_size = (integer-sqrt (length src))]
          [fx : (_ptr o _int64)] [fl : (_ptr o _double)] [dl : (_ptr o _double)]
          -> [ok : _bool]
          -> (values fx fl dl)))

  (define-matrix matrix_determinant
    (_fun [src : (_list i _int)] [_size = (integer-sqrt (length src))]
          [fx : (_ptr o _int64)] [fl : (_ptr o _double)] [dl : (_ptr o _double)]
          -> [ok : _bool]
          -> (values fx fl dl)))

  (define-matrix matrix_overflow_determinant
    (_fun [src : (_list i _int)] [_size = (integer-sqrt (length src))]
          [fx : (_ptr o _int)] [fl : (_ptr o _float)]
          -> [ok : _bool]
          -> (values fx fl))))

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
 [make-rectangular-fxmatrix (-> (Listof (Listof Integer)) FxMatrix)]
 [matrix_desc (-> FxMatrix String)]
 
 [fxmatrix_equal (-> FxMatrix FxMatrix Boolean)]
 [flmatrix_equal (-> FlMatrix FlMatrix Boolean)]
 [matrix_is_fixnum (-> CppMatrix Boolean : FxMatrix)]
 [matrix_is_flonum (-> CppMatrix Boolean : FlMatrix)]
 [matrix_is_zero (-> CppMatrix Boolean)]
 [matrix_is_square (-> CppMatrix Boolean)]
 [matrix_is_diagonal (-> CppMatrix Boolean)]
 [matrix_is_scalar (-> CppMatrix Boolean)]
 [matrix_is_lower_triangular (-> CppMatrix Boolean)]
 [matrix_is_upper_triangular (-> CppMatrix Boolean)]
 [matrix_is_identity (-> CppMatrix Boolean)]
 [matrix_is_symmetric (-> CppMatrix Boolean)]
 [matrix_is_skew_symmetric (-> CppMatrix Boolean)]

 [matrix-data (-> CppMatrix (Listof Real))]
 [matrix-data2d (-> CppMatrix (Listof (Listof Flonum)))]

 [matrix_trace (-> (Listof Integer) (Values Integer Flonum Flonum))]
 [matrix_determinant (-> (Listof Integer) (Values Integer Flonum Flonum))]
 [matrix_overflow_determinant (-> (Listof Integer) (Values Integer Flonum))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define matrix-format : Spec-Issue-Format
  (lambda [para fallback-format]
    (cond [(fxmatrix? para) (matrix_desc para)]
          [else (fallback-format para)])))

(define matrix-racket->cpp : (-> (Matrix Integer) FxMatrix)
  (lambda [mtx]
    (if (square-matrix? mtx)
        (make_square_fxmatrix (matrix->list mtx))
        (make-rectangular-fxmatrix (matrix->list* mtx)))))

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
    (expect-is matrix-equal given expected "we might have trouble in extracting flonum matrix data as an 1D-Array")
    (expect-is matrix-equal/2d given expected "we might have trouble in extracting flonum matrix data as a 2D-Array via std::vector")))

(define-behavior (it-tame-matrix/tr pool order)
  (let* ([fxentries (take-right pool (* order order))]
         [flentries (map exact->inexact fxentries)]
         [mtx.rkt (list->matrix order order fxentries)]
         [fxtr (matrix-trace mtx.rkt)]
         [fltr (real->double-flonum fxtr)])
    #:it ["should return ~a if given ~a" fxtr (matrix->list* mtx.rkt)]
    #:do
    (let-values ([(fixnum-tr double-tr float-tr) (matrix_trace fxentries)])
      (expect-= fixnum-tr fxtr "we have trouble in finding the trace for fixnum matrix")
      (expect-fl= double-tr fltr 0.1 "we have trouble in finding the trace for flonum matrix")
      (expect-fl= float-tr double-tr 0.1 "float is as less precise as double"))))

(define-behavior (it-tame-matrix/det pool order)
  (let* ([fxentries (take pool (* order order))]
         [flentries (map exact->inexact fxentries)]
         [mtx.rkt (list->matrix order order fxentries)]
         [fxdet (matrix-determinant mtx.rkt)]
         [fldet (real->double-flonum fxdet)])
    #:it ["should return ~a if given ~a" fxdet (matrix->list* mtx.rkt)]
    #:do
    (let-values ([(fixnum-det float-det double-det) (matrix_determinant fxentries)])
      (expect-= fixnum-det fxdet "we have trouble in finding the determinant for fixnum matrix")
      (expect-fl= double-det fldet 0.1 "we have trouble in finding the determinant for flonum matrix")
      (expect-fl= float-det double-det 0.1 "float is as less precise as double"))))

(define-context (it-tame-matrix/det/overflow order)
  (let* ([fxentries (build-list (* order order) (λ [i] (random 256 512)))]
         [flentries (map exact->inexact fxentries)]
         [mtx.rkt (list->matrix order order fxentries)]
         [fxdet (matrix-determinant mtx.rkt)]
         [fldet (real->double-flonum fxdet)])
    #:desc ["Overflow, with ~a" (matrix->list* mtx.rkt)]
    #:do
    (let-values/spec ([(fixnum-det flonum-det) (matrix_overflow_determinant fxentries)])
      (it ["can't not hold the intermediate fixnum products (~a != ~a)" fixnum-det fxdet] #:do
        (expect-!= fixnum-det fxdet))
      (it ["can't not hold the intermediate flonum products (~a != ~a)" flonum-det fldet] #:do  
        (expect-fl!= flonum-det fldet 0.1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define random-entries : (Listof Integer)
  (build-list 64 (λ [i] (random 256))))

(define natural-entries : (Listof Integer) (range 64))

(define mtx4x4.rkt : (Matrix Integer)
  (list->matrix 4 4 (take random-entries 16)))

(define mtx4x3.rkt : (Matrix Integer)
  (list->matrix 4 3 (take random-entries 12)))

(define mtx0 (make_null_square_fxmatrix))
(define mtx4x4.cpp (matrix-racket->cpp mtx4x4.rkt))
(define mtx4x3.cpp (matrix-racket->cpp mtx4x3.rkt))
