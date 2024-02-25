#lang typed/racket/base

(require digimon/spec)

(require racket/list)

(require "matrix.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature prelude #:do
  (describe "Matrix" #:do
    (describe "Property" #:do
      (context ["Default Constructor: ~a" (matrix_desc mtx0 #true)] #:do
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
          (expect-satisfy matrix_is_skew_symmetric mtx0))
        (it "should be in row echelon form" #:do
          (expect-satisfy matrix_is_row_echelon_form mtx0))
        (it "should be in row canonical form" #:do
          (expect-satisfy matrix_is_row_canonical_form mtx0)))

      (context ["Random Matrix: ~a" (matrix_desc mtx4x4.cpp #true)] #:do
        (it "should not be a diagonal matrix" #:do
          (expect-dissatisfy matrix_is_diagonal mtx4x4.cpp))
        (it "should not be an identity matrix" #:do
          (expect-dissatisfy matrix_is_identity mtx4x4.cpp))
        (it "should not be a scalar matrix" #:do
          (expect-dissatisfy matrix_is_scalar mtx4x4.cpp))
        (it "should not be a lower triangular matrix" #:do
          (expect-dissatisfy matrix_is_lower_triangular mtx4x4.cpp))
        (it "should not be a upper triangular matrix" #:do
          (expect-dissatisfy matrix_is_upper_triangular mtx4x4.cpp))
        (it "should not be a symmetic matrix" #:do
          (expect-dissatisfy matrix_is_symmetric mtx4x4.cpp))
        (it "should not be a skew symmetric matrix" #:do
          (expect-dissatisfy matrix_is_skew_symmetric mtx4x4.cpp))
        (it "should not be in row echelon form" #:do
          (expect-dissatisfy matrix_is_row_echelon_form mtx4x4.cpp))
        (it "should not be in row canonical form" #:do
          (expect-dissatisfy matrix_is_row_canonical_form mtx4x4.cpp)))

      (describe "Diagonal Matrix" #:do
        (let ([lmtx (matrix-racket->cpp (matrix-lower-triangle mtx4x4.rkt))])
          (context ["Lower/Left Triangular Matrix: ~a" (matrix_desc lmtx #true)] #:do
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
              (expect-dissatisfy matrix_is_skew_symmetric lmtx))
            (it "should not be in row echelon form" #:do
              (expect-dissatisfy matrix_is_row_echelon_form lmtx))
            (it "should not be in row canonical form" #:do
              (expect-dissatisfy matrix_is_row_canonical_form lmtx))))
                
        (let ([umtx (matrix-racket->cpp (matrix-upper-triangle mtx4x4.rkt))])
          (context ["Upper/Right Triangular Matrix: ~a" (matrix_desc umtx #true)] #:do
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
              (expect-dissatisfy matrix_is_skew_symmetric umtx))
            (it "should be in row echelon form" #:do
              (expect-satisfy matrix_is_row_echelon_form umtx))
            (it "should not be in row canonical form" #:do
              (expect-dissatisfy matrix_is_row_canonical_form umtx))))
                
        (let ([dmtx1 (make_diagonal_fxmatrix 1)])
          (context ["Diagonal Matrix: ~a" (matrix_desc dmtx1 #true)] #:do
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
              (expect-dissatisfy matrix_is_skew_symmetric dmtx1))
            (it "should be in row echelon form" #:do
              (expect-satisfy matrix_is_row_echelon_form dmtx1))
            (it "should be in row canonical form" #:do
              (expect-satisfy matrix_is_row_canonical_form dmtx1))))

        (let ([dmtx2 (make_diagonal_fxmatrix 2)])
          (context ["Diagonal Matrix: ~a" (matrix_desc dmtx2 #true)] #:do
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
              (expect-dissatisfy matrix_is_skew_symmetric dmtx2))
            (it "should be in row echelon form" #:do
              (expect-satisfy matrix_is_row_echelon_form dmtx2))
            (it "should not be in row canonical form" #:do
              (expect-dissatisfy matrix_is_row_canonical_form dmtx2))))))

    (describe "Entry Data" #:do
      (context "Make a 4x4 Fixnum Matrix, Giving Diagonal" #:do
        (it-tame-fxmatrix/diagonal (take random-entries 4))
        (it-tame-fxmatrix/diagonal (take random-entries 2))
        (it-tame-fxmatrix/diagonal (take random-entries 6)))

      (context "Make a 4x4 Fixnum Matrix, Giving Entries as 1D Array" #:do
        (it-tame-fxmatrix/entries (matrix->list mtx4x4.rkt))
        (it-tame-fxmatrix/entries (take random-entries 4))
        (it-tame-fxmatrix/entries random-entries))
          
      (context "Make a 4x4 Flonum Matrix, Giving Entries as 2D Fixnum Array" #:do
        (it-tame-flmatrix/entries (matrix->list mtx4x4.rkt))
        (it-tame-flmatrix/entries (take random-entries 4))
        (it-tame-flmatrix/entries random-entries))

      (context ["Miscellaneous with ~a" (matrix_desc mtx3x4.cpp #true)] #:do
        (it "should not a square matrix" #:do
          (expect-dissatisfy matrix_is_square mtx3x4.cpp))
        (it "could also be viewed as a diagonal plus two triangles" #:do
          (expect-is (inst matrix-equal/2d Flonum) mtx3x4.cpp (matrix-map exact->inexact mtx3x4.rkt)))))

    (describe "Basic Operation" #:do
      (context "Arithmetic Operators" #:do
        (it "can do addition [+, +=]" #:do
          (expect-is (inst matrix-equal Integer)
                     (matrix_add_subtract mtx3x4.cpp mtx3x4.cpp #true)
                     (matrix+ mtx3x4.rkt mtx3x4.rkt)))
        (it "can do subtract [-, -=]" #:do
          (expect-satisfy matrix_is_zero (matrix_add_subtract mtx3x4.cpp mtx3x4.cpp #false)))
        (it "can do scalar multiplication [*, *=]" #:do
          (expect-satisfy matrix_is_zero (matrix_scale mtx3x4.cpp 0.0 #true))
          (expect-is (inst matrix-equal Flonum) (matrix_scale mtx3x4.cpp 1.0 #true) (matrix-map real->double-flonum mtx3x4.rkt))
          (expect-is (inst matrix-equal Number) (matrix_scale mtx3x4.cpp 2.0 #true) (matrix-scale mtx3x4.rkt 2.0)))
        (it "can do scalar division [/, /=]" #:do
          (expect-is (inst matrix-equal Number) (matrix_scale mtx3x4.cpp 2.0 #false) (matrix-scale mtx3x4.rkt 0.5)))
        (let ([mtx3x2 (list->matrix 3 2 (take random-entries 6))]
              [mtx2x4 (list->matrix 2 4 (take-right random-entries 8))])
          (it "can do multiplication [*]" #:do
            (expect-is (inst matrix-equal Flonum)
                       (matrix-multiply (matrix->vector* mtx3x2) (matrix->vector* mtx2x4))
                       (matrix-map real->double-flonum (matrix* mtx3x2 mtx2x4))))))
      (context "Row Operation" #:do)
      (context "LU Decomposition" #:do
        (it-tame-matrix/LUD (matrix-cpp->racket mtx0))
        (it-tame-matrix/LUD mtx4x4.rkt)
        (it-tame-matrix/LUD (matrix [[2  3  1  5]
                                     [6 13  5 19]
                                     [2 19 10 23]
                                     [4 10 11 31]])))
      (context "Trace" #:do
        (it-tame-matrix/tr random-entries 1)
        (it-tame-matrix/tr random-entries 2)
        (it-tame-matrix/tr random-entries 3)
        (it-tame-matrix/tr random-entries 4)
        (it-tame-matrix/tr random-entries 5)
        (it-tame-matrix/tr random-entries 6))
      (context "Determinant" #:do
        (it-tame-matrix/det natural-entries 1)
        (it-tame-matrix/det natural-entries 2)
        (it-tame-matrix/det natural-entries 3)
        (it-tame-matrix/det natural-entries 4)
        (it-tame-matrix/det natural-entries 5)
        (it-tame-matrix/det natural-entries 6)

        (it-tame-matrix/det random-entries 1)
        (it-tame-matrix/det random-entries 2)
        (it-tame-matrix/det random-entries 3)
        (it-tame-matrix/det random-entries 4)
        (it-tame-matrix/det random-entries 5)
        (it-tame-matrix/det random-entries 6)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (void (parameterize ([default-spec-issue-format matrix-format])
          (spec-prove prelude))))
