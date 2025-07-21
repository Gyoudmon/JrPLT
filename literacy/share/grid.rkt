#lang typed/racket/base

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cols : Byte 5)
(define rows : Byte 5)

(define gomamon : Gomamon (make-gomamon 64))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  coordinate)
