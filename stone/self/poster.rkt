#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require geofun/vector)

(require diafun/digitama/avatar/bacteriophage)
(require diafun/digitama/avatar/procedure)

(require "logo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-interdisciplinary-poster : (->* () (Geo-Pin-Operator) Geo)
  (lambda [[op 'xor]]
    (parameterize ([default-procedure-body-fill 'WhiteSmoke])
      (define symfont (desc-font (default-art-font) #:family 'math #:size 'xx-large))
      (define em : Nonnegative-Flonum (font-metrics-ref symfont 'em))
      (define bacteriophage (bacteriophage-logo 16.0 #:tail.deg 32.0 #;#;#:sheath-length 0.0))
      (define stick : Geo (geo-rectangle (* em 1.618) (* em 0.382) #:fill 'WhiteSmoke #:stroke #false))
      (define characters (list #\科 #\逻 #\化 #\英 #\作 #\数 #\技 #\物 #\理 #\工))
      (define scale-span 0.6)
      (define symbols : (Listof Geo)
        (for/list ([sym (in-list characters)])
          (geo-scale (geo-symbol sym symfont)
                     (+ (- 1.0 scale-span)
                        (* (random) scale-span)))))
      
      (define bacterium
        (dia-procedure #:iofill (λ [var type] 'GhostWhite) #:io-width (- (geo-width stick) 1.0)
                       #:corner-radius -0.5
                       #:border (desc-stroke (default-procedure-border) #:color 'DarkGrey)
                       (λ [[rem : Nonnegative-Flonum]] : Geo
                         (geo-cc-superimpose (geo-blank (* rem 6.0) rem)
                                             (geo-art-text "编程 for 数学 & 物理" #:fill 'Black)))
                       #() #false))

      (geo-pin* #:operator op
                0.50 0.78 0.07 0.00
                (geo-rotate bacteriophage -20.0 #false)
                (geo-vc-append #:gapsize (- em) #:operator 'over
                               (geo-cb-superimpose #:operator 'atop bacterium stick)
                               (geo-pyramid (shuffle symbols) (* em -0.382) (* em 0.618) '??))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define poster (geo-scale (geo-interdisciplinary-poster) 2.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  poster

  #;(for/list : (Listof Geo) ([op (in-list geo-pin-operators)])
    (geo-rt-superimpose (geo-interdisciplinary-screen op)
                        (geo-text op))))
