#lang typed/racket/base

(require digimon/spec)

(require racket/list)

(require "matrix.rkt")
(require "flmatrix.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature prelude #:do
  (describe "Flonum Matrix" #:do
    (describe "Basic Operation" #:do
      (context "Row Operation" #:do
        (context "LU Decomposition" #:do
          (it-tame-matrix/LUD null-mtx)
          (it-tame-matrix/LUD mtx4x4.rkt)
          (it-tame-matrix/LUD lud-mtx))
        (context "LUP Decomposition" #:do
          (it-tame-matrix/LUPD null-mtx)
          (it-tame-matrix/LUPD mtx4x4.rkt)
          (it-tame-matrix/LUPD lup-mtx)))
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
