#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require geofun/markup)
(require plotfun/cartesian)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define coordinate
  (plot-cartesian #:x-label "F/N" #:y-label "f/N"
                  #:mark-style (make-plot-mark-style #:pin-angle 0.0 #:gap-angle 0.0)
                  #:x-range (cons 0 1) #:y-range (cons 0 1)
                  #:width 500 #:height 500))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  coordinate)
