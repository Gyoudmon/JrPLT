#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/flowlet)
(require diafun/digitama/avatar/procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define A : (-> Real Real)
  (lambda [x]
    (+ (* 1/3 x) -2/3)))

(define B : (-> Real Real)
  (lambda [x]
    (+ (* 1/4 x) 1/2)))

(define C : (-> Real Real)
  (lambda [x]
    (* x 1/2)))

(define D : (-> Real Real)
  (lambda [x]
    (+ (* 1/2 x) 1/2)))

(define A⁻¹ : (-> Real Real)
  (lambda [x]
    (+ (* 3 x) 2)))

(define B⁻¹ : (-> Real Real)
  (lambda [x]
    (+ (* 4 x) -2)))

(define C⁻¹ : (-> Real Real)
  (lambda [x]
    (* x 2)))

(define D⁻¹ : (-> Real Real)
  (lambda [x]
    (+ (* 2 x) -1)))

(define A+B.dia
  (geo-vl-append ((inst dia-flowlet-function Real Real) A 4)
                 ((inst dia-flowlet-function Real Real) B 4)))

(define A->B.dia
  ((inst dia-flowlet-functions Real) 4 A B))

(define A->C->D.dia
  ((inst dia-flowlet-functions Real) 16 A C D))

(define A->B->C->D.dia
  ((inst dia-flowlet-functions Real) 260 A B C D))

(define A->B->C->D⁻¹.dia
  ((inst dia-flowlet-functions Real) 6 D⁻¹ C⁻¹ B⁻¹ A⁻¹))

(define max+min.dia
  (geo-hc-append #:gapsize 32.0
                 (dia-procedure '∆ #(a b) #(max) #(3  5) (max 3  5))
                 (dia-procedure '∇ #(a b) #(min) #(7 10) (min 7 10))))

#;(define D->C->B->A
  ((inst dia-flowlet-functions Real) (list A B) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  A+B.dia
  A->B.dia
  A->C->D.dia
  A->B->C->D.dia
  A->B->C->D⁻¹.dia
  max+min.dia)
