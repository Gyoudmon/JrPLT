#lang typed/racket/base

(require geofun/vector)
(require plotfun/axis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cols : Byte 10)
(define rows : Byte 10)

(define gomamon : Gomamon (make-gomamon 32))

(for ([i (in-range (add1 rows))])
  (with-gomamon! gomamon
    (jump-to '#:home)
    (jump-up i)
    (move-right cols)))

(for ([i (in-range (add1 cols))])
  (with-gomamon! gomamon
    (jump-to '#:home)
    (jump-right i)
    (move-up rows)))

(define coordinate
  (geo-freeze gomamon))

(define style (make-plot-mark-style #:anchor 'cc #:color 'RoyalBlue #:pin-length 0.0 #:gap-length '(100 %)))

(define axis
  (plot-axis #:ticks (plot-fixed-ticks 0.0 1.0 0.1) #:label "P" #:mark-style style
             #:style (make-plot-axis-style #:tip plot-no-tip)
             null))

(define integer-axis
  (plot-integer-axis #:range (cons 0 100) #:label "%" #:exclude-zero? #false #:mark-style style
                     #:style (make-plot-axis-style #:tip plot-no-tip)
                     null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  coordinate
  integer-axis
  axis)
