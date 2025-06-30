#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/flowlet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define A : (-> Real Real)
  (lambda [x]
    (* 4 x)))

(define B : (-> Real Real)
  (lambda [x]
    (+ x 5)))

(define A⁻¹ : (-> Real Real)
  (lambda [x]
    (/ x 4)))

(define B⁻¹ : (-> Real Real)
  (lambda [x]
    (- x 5)))

(define A+B.dia
  (geo-vl-append ((inst dia-flowlet-function Real Real) A 4)
                 ((inst dia-flowlet-function Real Real) B 4)))

(define A->B.dia
  ((inst dia-flowlet-functions Real) 12 A B))

(define A->B⁻¹.dia
  ((inst dia-flowlet-functions Real) 85 B⁻¹ A⁻¹))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  A+B.dia
  A->B.dia
  A->B⁻¹.dia)
