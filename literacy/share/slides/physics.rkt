#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require geofun/markup)
(require plotfun/line)

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scale-desc : Plot-Mark->Description
  (lambda [pt datum font color transform]
    (geo-markup #:color color
                (<span> null
                        "10"
                        (<sup> (number->string (real-part pt))))
                font)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define length-scale : Geo
  (let ([rad (degrees->radians -25)]
        [color (rgb* 'GhostWhite)])
    (plot-real-line #:length 800.0
                    ;#:label (cons "微观尺度" "宏观尺度")
                    #:ticks plot-no-ticks
                    #:style (make-plot-axis-style #:stroke (desc-stroke #:width 3.0 #:color color)
                                                  #:font (desc-font #:size 'large)
                                                  #:tip plot-bi-tip)
                    #:mark-style (make-plot-mark-style #:pin-stroke (desc-stroke #:width 2.0 #:color color))
                    #:mark-template (plot-template #:pin-length '(300 %) #:pin-angle (- rad)
                                                   #:shape #false #:desc scale-desc
                                                   '(25 %) (- rad))
                    #:range (cons -15 15)
                    #:rotate rad
                    (list -15 -10 -7 -3 0 7 13))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-frame #:background #x0
             length-scale))
