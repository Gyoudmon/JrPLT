#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require plotfun/axis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dkcyan-style (make-plot-mark-style #:pin-length 0.0 #:gap-length '(200 %) #:gap-angle pi/2 #:anchor 'ct #:color 'DarkCyan))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-timeline-real-desc : (->* (String Index) (Real) Plot-Mark->Description)
  (lambda [func-name N [scale 1.0]]
    (λ [pt datum font color transform]
      (define n (real->double-flonum (real-part pt)))
      (define c (rgb* color (/ (+ n 1.0) (exact->inexact (add1 N)))))
      (define unit (max (real-part (- (transform (+ n 1.0) 0.0) (transform n 0.0))) 0.0))
      (define arrow (geo-arrow pi (* unit 0.16) (* pi 0.5) #:fill c #:stroke #false))
      (define gobj (geo-fit (geo-scale (geo-text datum font #:color c) scale scale) unit unit))
      (define func (geo-sandglass (* unit 0.618) #:fill c #:stroke #false))
      (define name (geo-fit (geo-text func-name (desc-font font #:family 'math) #:color 'GhostWhite) func 0.95 0.24))
      (define fdia (geo-pin* 0.5 0.2 0.5 0.5 func name))

      (geo-vc-append #:gapsize 4.0 arrow fdia arrow gobj))))

(define hh:succ-desc : Plot-Mark->Description
  (lambda [pt datum font color transform]
    (define n (real->double-flonum (real-part pt)))
    (define c (rgb* color (/ (+ n 1.0) 8.0)))
    (define unit (max (real-part (- (transform (+ n 1.0) 0.0) (transform n 0.0))) 0.0))
    (define g (geo-vc-append (geo-text "+1" font #:color c)
                             (geo-arc (* unit 0.5) 3.4 6.0 #:stroke c #:ratio 1.618)))
    
    (make-sticker (if (eq? 'arrow datum)
                      (geo-pin* 0.975 0.58 0.5 0.5 g (geo-dart (* unit 0.1) (* pi 0.375) #:fill c #:stroke #false))
                      g)
                  'lb
                  (make-rectangular 0.0 (* unit 0.25)))))

(define number-axis/succ : Geo
  (plot-integer-axis #:range (cons 0 7)
                     #:unit-length '(12.5 %)
                     #:mark-style (make-plot-mark-style #:color 'Orange #:pin-length 0.0)
                     #:mark-template hh:succ-desc
                     #:exclude-zero? #false
                     #:label "x"
                     (list 0 1 2 3 4 5 6
                           (plot-integer 7 #:gap-length 0.0 #:datum 'arrow))))

(define number-axis/error : Geo
  (plot-integer-axis #:range (cons 0 8)
                     #:style (make-plot-axis-style #:stroke (desc-stroke default-axis-stroke #:color 'RoyalBlue))
                     #:mark-style (make-plot-mark-style #:anchor 'cb #:color 'Crimson #:pin-length 0.0 #:gap-length '(61.8 %))
                     #:exclude-zero? #true
                     #:label "n"
                     sub1))

(define timeline/sub1 : Geo
  (plot-integer-axis #:range (cons 0 8)
                     #:mark-style dkcyan-style
                     #:mark-template (make-timeline-real-desc "x(n)" 8)
                     #:exclude-zero? #true
                     #:label "n"
                     sub1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  number-axis/succ
  number-axis/error
  timeline/sub1)
