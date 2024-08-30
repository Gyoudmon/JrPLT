#lang typed/racket/base

(require racket/list)

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-font : Font (desc-font #:family 'decorative #:size 24))
(define math-font : Font (desc-font default-font #:family 'math))

(define fibonacci-sequence : (-> Integer Real)
  (lambda [i]
    (if (> i 2)
        (+ (fibonacci-sequence (- i 1))
           (fibonacci-sequence (- i 2)))
        1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-sequence : (-> Positive-Index (-> Integer Real) [#:unit Positive-Byte] [#:dot-radius Real] [#:arrow-radius Real] [#:gapsize Real] Geo)
  (lambda [x-max map #:unit [unit 64] #:dot-radius [dr 3.0] #:arrow-radius [ar 8.0] #:gapsize [gapsize 8.0]]
    (define axis (geo-arrow ar (* unit (+ x-max 2)) #:stroke #false #:fill 'Black))
    (define λarrow (geo-arrow (* ar 0.618) (* unit 1.618) (/ pi 2.0) #:fill 'Gray #:stroke #false))
    (define x-baseline : Real (* (sub1 (geo-height axis)) 0.5))

    (for/fold ([number-line : Geo (geo-composite #:operator 'over axis 0.0 x-baseline (geo-circle dr #:fill 'GhostWhite) dr dr)])
              ([i (in-range 1 (+ x-max 1))])
      (define fmap : Geo
        (geo-vc-append #:gapsize gapsize
                       (geo-circle dr #:fill 'black)
                       (geo-text i default-font #:color 'Purple)
                       λarrow
                       (geo-text (map i) default-font #:color 'Green)))
      (geo-composite #:operator 'over
                     number-line (* i unit) x-baseline
                     fmap (* (geo-width fmap) 0.5) dr))))

(define geo-sequence-function : (-> String Real Color Color Geo)
  (lambda [fn size color text-color]
    (define border (desc-stroke #:color 'WhiteSmoke #:width 2.0))
    
    (geo-pin* 0.5 0.25 0.5 0.5
              (geo-sandglass size #:neck-width -0.32 #:neck-height -0.0 #:fill color #:stroke border)
              (geo-text (string-append "    " fn "    ") math-font #:color text-color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define seq-diagram (geo-sequence 12 fibonacci-sequence))
(define seq-function-shape (geo-sequence-function "f(n)" 64 'RoyalBlue 'GhostWhite))
(define seq-fibonacci-shape (geo-sequence-function "F(n)" 64 'ForestGreen 'GhostWhite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  seq-diagram
  seq-function-shape
  seq-fibonacci-shape)
