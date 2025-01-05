#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require plotfun/axis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-timeline-real->sticker : (->* (String Index) (Real) Plot-Axis-Real->Sticker)
  (lambda [func-name n [scale 1.0]]
    (λ [id r obj unit font color]
      (define c (rgb* color (/ (+ r 1.0) (exact->inexact (add1 n)))))
      (define arrow (geo-arrow pi (* unit 0.16) (* pi 0.5) #:fill c #:stroke #false))
      (define gobj (geo-fit (geo-scale (geo-text obj font #:color c) scale scale) unit unit))
      (define func (geo-sandglass (* unit 0.618) #:fill c #:stroke #false))
      (define name (geo-fit (geo-text func-name (desc-font font #:family 'math) #:color 'GhostWhite) func 0.95 0.24))
      (define fdia (geo-pin* 0.5 0.2 0.5 0.5 func name))

      (geo-vc-append #:gapsize 4.0 arrow fdia arrow gobj))))
