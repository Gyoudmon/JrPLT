#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require plotfun/axis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dkcyan-style (make-plot-axis-real-style #:position -2.0 #:anchor 'ct #:color 'DarkCyan))

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

(define hh:add1-sticker : Plot-Axis-Real->Sticker
  (lambda [id r datum unit font color]
    (define c (rgb* color (/ (+ r 1.0) 8.0)))
    (define g (geo-vc-append (geo-text "+1" font #:color c)
                             (geo-arc (* unit 0.5) pi 0.0 #:stroke c #:ratio 0.85)))
    
    (cons (if (eq? 'arrow datum)
              (geo-pin* 1.0 0.56 0.5 0.5 g (geo-dart (* unit 0.1) (* pi 0.5) #:fill c #:stroke #false))
              g)
          'lc)))

(define number-axis/add1 : Geo
  (plot-integer-axis #:range (cons 0 7)
                     #:unit-length -0.125
                     #:integer-style (make-plot-axis-real-style #:color 'Orange)
                     #:integer->sticker hh:add1-sticker
                     #:exclude-zero? #false
                     #:label "x"
                     '(0 1 2 3 4 5 6 (7 . arrow))))

(define number-axis/error : Geo
  (plot-integer-axis #:range (cons 0 8)
                     #:style (make-plot-axis-style #:color 'RoyalBlue)
                     #:integer-style (make-plot-axis-real-style #:position 0.618 #:anchor 'cb #:color 'Crimson)
                     #:exclude-zero? #true
                     #:label "n"
                     sub1))

(define timeline/sub1 : Geo
  (plot-integer-axis #:range (cons 0 8)
                     #:integer-style dkcyan-style
                     #:integer->sticker (make-timeline-real->sticker "x(n)" 8)
                     #:exclude-zero? #true
                     #:label "n"
                     sub1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  number-axis/add1
  number-axis/error
  timeline/sub1)
