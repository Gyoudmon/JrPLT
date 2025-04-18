#lang typed/racket/base

(provide (all-defined-out))

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
  (define-ffi-definer define-matrix (digimon-ffi-lib "fxmatrix"))

  (define-matrix make_null_square_fxmatrix
    (_fun -> _Matrix_ptr)
    #:wrap delete-matrix)

  (define-matrix make_square_fxmatrix
    (_fun [src : (_list i _int)]
          [_size = (length src)]
          -> _Matrix_ptr)
    #:wrap delete-matrix)

  (define make-square-fxmatrix
    (let ([λs (make-hasheq)])
      (lambda [entries]
        (define order (length entries))
        
        (define (make-fxmatrix)
          (define _array2d (_array/list _int order order))
          (define-matrix make_square_fxmatrix_via_vector (_fun _array2d [_size = order] -> _Matrix_ptr) #:wrap delete-matrix)
          make_square_fxmatrix_via_vector)
        
        ((hash-ref! λs order make-fxmatrix) entries))))

  (define-matrix make_square_fxmatrix_with_diagonal
    (_fun [src : (_list i _int)]
          [_size = (length src)]
          -> _Matrix_ptr)
    #:wrap delete-matrix)

  (define-matrix make_diagonal_fxmatrix
    (_fun _int -> _Matrix_ptr)
    #:wrap delete-matrix)

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

  (define-matrix fxmatrix_equal (_fun _Matrix_ptr _Matrix_ptr -> _bool))

  (define-matrix fxmatrix_data_row_by_row
    (_fun _Matrix_ptr [dest1D : (_list o _int buffer-size)] [buffer-size : _size]
          -> [actual-size : _size]
          -> (take dest1D actual-size)))

  (define fxmatrix-data
    (lambda [mtx]
      (define-values (size R C) (matrix_shape mtx))
      
      (fxmatrix_data_row_by_row mtx size)))

  (define fxmatrix-data2d
    (lambda [mtx]
      (define-values (S R C) (matrix_shape mtx))
      (define _array2d (_array/list _int R C))
      
      (cond [(not (= R C))
             (define-matrix fxmatrix_rectangular_data2d (_fun _Matrix_ptr [dest2D : (_ptr o _array2d)] _size _size -> _size -> dest2D))
             (fxmatrix_rectangular_data2d mtx R C)]
            [else
             (define-matrix fxmatrix_data2d_via_vector (_fun _Matrix_ptr [dest2D : (_ptr o _array2d)] _size -> _size -> dest2D))
             (fxmatrix_data2d_via_vector mtx R)])))

  (define-matrix fxmatrix_add_subtract
    (_fun _Matrix_ptr _Matrix_ptr _bool -> _Matrix_ptr)
    #:wrap delete-matrix)
  
  (define-matrix fxmatrix_scale
    (_fun _Matrix_ptr _int _bool -> _Matrix_ptr)
    #:wrap delete-matrix)

  (define fxmatrix-multiply
    (let ([λs (make-hash)])
      (lambda [lhs rhs]
        (define M (vector-length lhs))
        (define N (vector-length rhs))
        (define P (vector-length (vector-ref rhs 0)))
        
        (define (multiply)
          (define _array2d/lhs (_array/vector _int M N))
          (define _array2d/rhs (_array/vector _int N P))
          (define-matrix fxmatrix_multiply
            (_fun _array2d/lhs _array2d/rhs
                  [_size = M] [_size = N] [_size = P]
                  -> _Matrix_ptr)
            #:wrap delete-matrix)
          fxmatrix_multiply)
        
        ((hash-ref! λs (list M N P) multiply) lhs rhs))))
  ;;; END for testing
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [make_null_square_fxmatrix (-> CppMatrix)]
 [make_square_fxmatrix (-> (Listof Integer) CppMatrix)]
 [make-square-fxmatrix (-> (Listof (Listof Integer)) CppMatrix)]
 [make_square_fxmatrix_with_diagonal (-> (Listof Integer) CppMatrix)]
 [make_diagonal_fxmatrix (-> Integer CppMatrix)]
 [make-rectangular-fxmatrix (-> (Listof (Listof Integer)) CppMatrix)]

 [fxmatrix_equal (-> CppMatrix CppMatrix Boolean)]

 [fxmatrix-data (-> CppMatrix (Listof Integer))]
 [fxmatrix-data2d (-> CppMatrix (Listof (Listof Integer)))]

 [fxmatrix_add_subtract (-> CppMatrix CppMatrix Boolean CppMatrix)]
 [fxmatrix_scale (-> CppMatrix Integer Boolean CppMatrix)]
 [fxmatrix-multiply (-> (Vectorof (Vectorof Integer)) (Vectorof (Vectorof Integer)) CppMatrix)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fxmatrix-racket->cpp : (-> (Matrix Integer) CppMatrix)
  (lambda [mtx]
    (if (square-matrix? mtx)
        (make_square_fxmatrix (matrix->list mtx))
        (make-rectangular-fxmatrix (matrix->list* mtx)))))

(define rational->integer : (-> Exact-Rational Integer)
  (lambda [datum]
    (cond [(exact-integer? datum) datum]
          [else (floor datum)])))

(define square-fxmatrix-equal : (-> CppMatrix (U CppMatrix (Matrix Integer) (Listof Integer)) Boolean)
  (lambda [lhs rhs]
    (equal? (fxmatrix-data lhs)
            (cond [(list? rhs) rhs]
                  [(array? rhs) (matrix->list* rhs)]
                  [else (fxmatrix-data rhs)]))))

(define fxmatrix-equal/2d : (-> CppMatrix (U CppMatrix (Matrix Integer) (Listof (Listof Integer))) Boolean)
  (lambda [lhs rhs]
    (equal? (fxmatrix-data2d lhs)
            (cond [(list? rhs) rhs]
                  [(array? rhs) (matrix->list* rhs)]
                  [else (fxmatrix-data2d rhs)]))))

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
         [expected (fxmatrix-racket->cpp (diagonal-matrix diagonal))])
    #:it
    "should pad with 0 if given too fewer data" #:when (> diff-size 0)
    "should ignore the rest if given too many data" #:when (< diff-size 0)
    "should contain all given data as diagonal"
    #:do
    (expect-is fxmatrix_equal given expected)))

(define-behavior (it-tame-fxmatrix/1d-entries entries)
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
    (expect-is fxmatrix_equal given expected)))

(define-behavior (it-tame-fxmatrix/2d-entries entries)
  (let*-values ([(order squares) (array2d-shape entries)]
                [(given) (make-square-fxmatrix squares)]
                [(expected) (make-square-fxmatrix (array2d-shape* squares order 4 0))])
    #:it
    "should pad with 0 if given too fewer data" #:when (< order 4)
    "should ignore the rest if given too many data" #:when (> order 4)
    "should contain all given data as entries"
    #:do
    (expect-dissatisfy matrix_is_zero given "we might have trouble in constructing a fixnum matrix")
    (expect-is fxmatrix_equal given expected)
    (expect-satisfy-any (negate zero?) (fxmatrix-data given) "we might have trouble in extracting fixnum matrix data as an 1D-Array row by row")
    (expect-satisfy-any (negate listof-zero?) (fxmatrix-data2d given) "we might have trouble in extracting fixnum matrix data as a 2D-Array")
    (expect-is square-fxmatrix-equal given expected  "we might have trouble in extracting fixnum matrix data as an 1D-Array row by row")
    (expect-is fxmatrix-equal/2d given expected  "we might have trouble in extracting fixnum matrix data as a 2D-Array")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define random-entries : (Listof Integer)
  (build-list 64 (λ [i] (random 256))))

(define natural-entries : (Listof Integer) (range 64))

(define mtx4x4.rkt : (Matrix Integer)
  (list->matrix 4 4 (take random-entries 16)))

(define mtx3x4.rkt : (Matrix Integer)
  (list->matrix 3 4 (take random-entries 12)))

(define mtx0 (make_null_square_fxmatrix))
(define mtx4x4.cpp (fxmatrix-racket->cpp mtx4x4.rkt))
(define mtx3x4.cpp (fxmatrix-racket->cpp mtx3x4.rkt))
