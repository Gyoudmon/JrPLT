#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require plotfun/line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dkcyan-style (make-plot-mark-style #:pin-length 0.0 #:gap-length '(200 %) #:gap-angle pi/2 #:color 'DarkCyan))

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
    
    (make-sticker (geo-path #:stroke c #:target-tip default-arrow-tip
                            #:labels (make-geo-path-label "+1" #:font font #:color c)
                            #:scale unit
                            (list (list 0.1 0.5-0.16i 0.9)))
                  'lb
                  (* unit 0.1-0.25i))))

(define number-axis/succ : Geo
  (plot-integer-line #:range (cons 0 7)
                     #:unit-length '(12.5 %)
                     #:mark-style (make-plot-mark-style #:color 'Orange #:pin-length 0.0)
                     #:mark-template hh:succ-desc
                     #:exclude-zero? #false
                     #:label "x"
                     (list 0 1 2 3 4 5 6 7)))

(define number-axis/error : Geo
  (plot-integer-line #:range (cons 0 8)
                     #:style (make-plot-axis-style #:pen (desc-stroke default-axis-pen #:color 'RoyalBlue))
                     #:mark-style (make-plot-mark-style #:color 'Crimson #:pin-length 0.0 #:gap-length '(61.8 %))
                     #:exclude-zero? #true
                     #:label "n"
                     sub1))

(define timeline/sub1 : Geo
  (plot-integer-line #:range (cons 0 8)
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
