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


(module+ main
  (plot-axis #:tick-range (cons 0 8) #:reals sub1
             #:real-position -2.0 #:real-anchor 'ct
             #:real->sticker (make-timeline-real->sticker "x(n)" 8)
             #:real-color 'DarkCyan
             #:real-exclude-zero? #true
             #:axis-label "n"
             400 0.0 42.0)

  (plot-axis #:tick-range (cons 0 7) #:reals #(null (4) (3 4) (5 3 4) (3 5 3 4) (9 3 5 3 4) (3 9 3 5 3 4))
                          #:real-position -2.0 #:real-anchor 'ct
                          #:real->sticker (make-timeline-real->sticker "ID(n)" 7 0.618)
                          #:real-color 'DarkCyan
                          #:real-exclude-zero? #true
                          #:axis-label "n"
                          400 0.0 48.0))
