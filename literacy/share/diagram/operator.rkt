#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/flowlet)
(require diafun/digitama/avatar/procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define A : (-> Real Real)
  (lambda [x]
    (* x 5)))

(define B : (-> Real Real)
  (lambda [x]
    (+ x 3)))

(define A+B.dia
  (geo-vl-append ((inst dia-flowlet-function Real Real) A 4)
                 ((inst dia-flowlet-function Real Real) B 4)))

(define A->B.dia
  ((inst dia-flowlet-functions Real) (list A B) 4))

(define max+min.dia
  (geo-hc-append #:gapsize 32.0
                 (dia-procedure '∆ #(a b) #(max) #(3  5) (max 3  5))
                 (dia-procedure '∇ #(a b) #(min) #(7 10) (min 7 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  A+B.dia
  A->B.dia
  max+min.dia)
