#lang typed/racket/base

(require racket/list)

(require bitmap)

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
(define bitmap-sequence : (-> Positive-Index (-> Integer Real) [#:unit Positive-Byte] [#:dot-radius Real] [#:arrow-radius Real] [#:gapsize Real] Bitmap)
  (lambda [x-max map #:unit [unit 64] #:dot-radius [dr 3.0] #:arrow-radius [ar 8.0] #:gapsize [gapsize 8.0]]
    (define axis (bitmap-arrow ar (* unit (+ x-max 2))))
    (define λarrow (bitmap-arrow (* ar 0.618) (* unit 1.618) (/ pi 2.0) #:fill 'Gray))
    (define x-baseline : Real (* (sub1 (bitmap-height axis)) 0.5))

    (for/fold ([number-line : Bitmap (bitmap-composite 'over axis 0.0 x-baseline (bitmap-circle dr #:fill 'GhostWhite) dr dr)])
              ([i (in-range 1 (+ x-max 1))])
      (define fmap : Bitmap
        (bitmap-vc-append #:gapsize gapsize
                          (bitmap-circle dr #:fill 'black)
                          (bitmap-text i default-font #:color 'Purple)
                          λarrow
                          (bitmap-text (map i) default-font #:color 'Green)))
      (bitmap-composite 'over
                        number-line (* i unit) x-baseline
                        fmap (* (bitmap-width fmap) 0.5) dr))))

(define bitmap-sequence-function : (-> String Real Color Color Bitmap)
  (lambda [fn size color text-color]
    (define border (desc-stroke #:color 'WhiteSmoke #:width 2.0))
    
    (bitmap-pin* 0.5 0.25 0.5 0.5
                 (bitmap-sandglass size #:neck-width -0.32 #:neck-height -0.0 #:fill color #:border border)
                 (bitmap-text (string-append "    " fn "    ") math-font #:color text-color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define seq-diagram (bitmap-sequence 12 fibonacci-sequence))
(define seq-function-shape (bitmap-sequence-function "f(n)" 64 'RoyalBlue 'GhostWhite))
(define seq-fibonacci-shape (bitmap-sequence-function "F(n)" 64 'ForestGreen 'GhostWhite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  seq-diagram
  seq-function-shape
  seq-fibonacci-shape)
