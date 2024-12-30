#lang typed/racket/base

(require mathfun/position)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define n 8)

(define origin (math-positional-digits n #:nslots 3))

(define lshifted
  (let ([arrow (geo-cc-superimpose (geo-arrow 4 8 pi #:stroke #false #:fill 'black)
                                   (geo-square 16 #:stroke #false))])
    (geo-hc-append #:gapsize 4.0
                   (geo-vr-append arrow origin)
                   (geo-vc-append (geo-text (format "~a * 10" n))
                                  (geo-arrow 5 48))
                   (geo-vr-append (geo-ghost arrow)
                                  (math-positional-digits n +1 #:nslots 3)))))

(define rshifted
  (let ([arrow (geo-cc-superimpose (geo-arrow 4 8 0.0 #:stroke #false #:fill 'black)
                                   (geo-square 16 #:stroke #false))])
    (geo-hc-append #:gapsize 4.0
                   (geo-vr-append arrow origin)
                   (geo-vc-append (geo-text (format "~a // 10" n))
                                  (geo-arrow 5 48))
                   (geo-vr-append (geo-ghost arrow)
                                  (math-positional-digits n -1 #:nslots 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  lshifted
  rshifted)
