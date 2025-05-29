#lang typed/racket/base

(require geofun/vector)
(require plotfun/axis)

(require racket/list)

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

(define style (make-plot-axis-real-style #:anchor 'cc #:color 'RoyalBlue #:dot-radius -2.0))

(define axis
  (plot-axis #:ticks (plot-fixed-ticks 0.0 1.0 0.1) #:label "P" #:real-style style
             (list 0 1)))

(define integer-axis
  (plot-integer-axis #:range (cons 0 100) #:label "%" #:exclude-zero? #false #:integer-style style
                     (list 0 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  coordinate
  integer-axis
  axis)
