#lang typed/racket/base

(require digimon/spec)

(require "matrix.rkt")

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature prelude #:do
  (describe "Matrix" #:do
    (describe "Square Matrix" #:do
      (describe "Property" #:do
        (context ["Default Constructor: ~a" (matrix_desc mtx0)] #:do
          (it "should be a zero matrix" #:do
            (expect-satisfy matrix_is_zero mtx0))
          (it "should be a diagonal matrix" #:do
            (expect-satisfy matrix_is_diagonal mtx0))
          (it "should not be an identity matrix" #:do
            (expect-dissatisfy matrix_is_identity mtx0))
          (it "should be a scalar matrix" #:do
            (expect-satisfy matrix_is_scalar mtx0))
          (it "should be a lower triangular matrix" #:do
            (expect-satisfy matrix_is_lower_triangular mtx0))
          (it "should be a upper triangular matrix" #:do
            (expect-satisfy matrix_is_upper_triangular mtx0))
          (it "should be a symmetic matrix" #:do
            (expect-satisfy matrix_is_symmetric mtx0))
          (it "should be a skew symmetric matrix" #:do
            (expect-satisfy matrix_is_skew_symmetric mtx0)))

        (context ["Random Matrix: ~a" (matrix_desc rmtx)] #:do
          (it "should not be a diagonal matrix" #:do
            (expect-dissatisfy matrix_is_diagonal rmtx))
          (it "should not be an identity matrix" #:do
            (expect-dissatisfy matrix_is_identity rmtx))
          (it "should not be a scalar matrix" #:do
            (expect-dissatisfy matrix_is_scalar rmtx))
          (it "should not be a lower triangular matrix" #:do
            (expect-dissatisfy matrix_is_lower_triangular rmtx))
          (it "should not be a upper triangular matrix" #:do
            (expect-dissatisfy matrix_is_upper_triangular rmtx))
          (it "should not be a symmetic matrix" #:do
            (expect-dissatisfy matrix_is_symmetric rmtx))
          (it "should not be a skew symmetric matrix" #:do
            (expect-dissatisfy matrix_is_skew_symmetric rmtx)))

        (describe "Diagonal Matrix" #:do
          (let ([lmtx (matrix-racket->cpp (matrix-lower-triangle mtx4x4))])
            (context ["Lower/Left Triangular Matrix: ~a" (matrix_desc lmtx)] #:do
              (it "should not be a diagonal matrix" #:do
                (expect-dissatisfy matrix_is_diagonal lmtx))
              (it "should not be an identity matrix" #:do
                (expect-dissatisfy matrix_is_identity lmtx))
              (it "should not be a scalar matrix" #:do
                (expect-dissatisfy matrix_is_scalar lmtx))
              (it "should be a lower triangular matrix" #:do
                (expect-satisfy matrix_is_lower_triangular lmtx))
              (it "should not be a upper triangular matrix" #:do
                (expect-dissatisfy matrix_is_upper_triangular lmtx))
              (it "should not be a symmetic matrix" #:do
                (expect-dissatisfy matrix_is_symmetric lmtx))
              (it "should not be a skew symmetric matrix" #:do
                (expect-dissatisfy matrix_is_skew_symmetric lmtx))))
                
          (let ([umtx (matrix-racket->cpp (matrix-upper-triangle mtx4x4))])
            (context ["Upper/Right Triangular Matrix: ~a" (matrix_desc umtx)] #:do
              (it "should not be a diagonal matrix" #:do
                (expect-dissatisfy matrix_is_diagonal umtx))
              (it "should not be an identity matrix" #:do
                (expect-dissatisfy matrix_is_identity umtx))
              (it "should not be a scalar matrix" #:do
                (expect-dissatisfy matrix_is_scalar umtx))
              (it "should not be a lower triangular matrix" #:do
                (expect-dissatisfy matrix_is_lower_triangular umtx))
              (it "should be a upper triangular matrix" #:do
                (expect-satisfy matrix_is_upper_triangular umtx))
              (it "should not be a symmetic matrix" #:do
                (expect-dissatisfy matrix_is_symmetric umtx))
              (it "should not be a skew symmetric matrix" #:do
                (expect-dissatisfy matrix_is_skew_symmetric umtx))))
                
          (let ([dmtx1 (make_diagonal_fxmatrix 1)])
            (context ["Diagonal Matrix: ~a" (matrix_desc dmtx1)] #:do
              (it "should not be a zero matrix" #:do
                (expect-dissatisfy matrix_is_zero dmtx1))
              (it "should be a diagonal matrix" #:do
                (expect-satisfy matrix_is_diagonal dmtx1))
              (it "should be an identity matrix" #:do
                (expect-satisfy matrix_is_identity dmtx1))
              (it "should be a scalar matrix" #:do
                (expect-satisfy matrix_is_scalar dmtx1))
              (it "should be a lower triangular matrix" #:do
                (expect-satisfy matrix_is_lower_triangular dmtx1))
              (it "should be a upper triangular matrix" #:do
                (expect-satisfy matrix_is_upper_triangular dmtx1))
              (it "should be a symmetic matrix" #:do
                (expect-satisfy matrix_is_symmetric dmtx1))
              (it "should not be a skew symmetric matrix" #:do
                (expect-dissatisfy matrix_is_skew_symmetric dmtx1))))

          (let ([dmtx2 (make_diagonal_fxmatrix 2)])
            (context ["Diagonal Matrix: ~a" (matrix_desc dmtx2)] #:do
              (it "should not be a zero matrix" #:do
                (expect-isnt fxmatrix_equal dmtx2 mtx0))
              (it "should be a diagonal matrix" #:do
                (expect-satisfy matrix_is_diagonal dmtx2))
              (it "should not be an identity matrix" #:do
                (expect-dissatisfy matrix_is_identity dmtx2))
              (it "should be a scalar matrix" #:do
                (expect-satisfy matrix_is_scalar dmtx2))
              (it "should be a lower triangular matrix" #:do
                (expect-satisfy matrix_is_lower_triangular dmtx2))
              (it "should be a upper triangular matrix" #:do
                (expect-satisfy matrix_is_upper_triangular dmtx2))
              (it "should be a symmetic matrix" #:do
                (expect-satisfy matrix_is_symmetric dmtx2))
              (it "should not be a skew symmetric matrix" #:do
                (expect-dissatisfy matrix_is_skew_symmetric dmtx2))))))

      (describe "Entry Data" #:do
        (context "Make a 4x4 Fixnum Matrix, Giving Diagonal" #:do
          (it-tame-fxmatrix/diagonal (take random-entries 4))
          (it-tame-fxmatrix/diagonal (take random-entries 2))
          (it-tame-fxmatrix/diagonal (take random-entries 6)))

        (context "Make a 4x4 Fixnum Matrix, Giving Entries as 1D Array" #:do
          (it-tame-fxmatrix/entries (matrix->list mtx4x4))
          (it-tame-fxmatrix/entries (take random-entries 4))
          (it-tame-fxmatrix/entries random-entries))
          
        (context "Make a 4x4 Flonum Matrix, Giving Entries as 2D Fixnum Array" #:do
          (it-tame-flmatrix/entries (matrix->list mtx4x4))
          (it-tame-flmatrix/entries (take random-entries 4))
          (it-tame-flmatrix/entries random-entries))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (void (parameterize ([default-spec-issue-format matrix-format])
          (spec-prove prelude))))
