#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require geofun/vector)

(require diafun/digitama/avatar/bacteriophage)
(require diafun/digitama/avatar/procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-7-bridge-agent : (->* (Nonnegative-Flonum) (Maybe-Fill-Paint) Geo)
  (lambda [rem [brush 'Gray]]
    (define io:width   : Nonnegative-Flonum (* rem 1.618))
    (define in:gapsize : Nonnegative-Flonum (* rem 0.618))

    (define-gomamon! 7-bridge-agent
      [io:width in:gapsize #:anchor '#:A #:stroke 'Yellow #:fill brush #:fill-rule 'even-odd] #:-
      (move-up 3 '#:B)
      (drift -1.4 '(-0.618+1.5i) '#:C)
      (drift '#:B '(+0.618-1.5i))
      (drift +1.4 '(+0.618+1.5i) '#:D)
      (drift '#:B '(-0.618-1.5i))
      (jump-back)
      (drift '#:A '(-0.618+3.0i))
      (drift '#:C '(-0.618+0.0i)))
    
    7-bridge-agent))

(define geo-symbol : (-> Char Font Geo)
  (lambda [sym font]
    (define color (hsv (random 360) 1.0 1.0))

    (bacteriophage-logo #:sheath-length 0.0 #:symbol sym
                        #:λ-color color #:border-color color #:edge-color color
                        #:fibre-color color #:left-foot-border-color color #:right-foot-border-color color
                        (* (font-size font) 0.618))))

(define geo-slides-screen : (-> Geo)
  (lambda []
    (dia-procedure #:iofill (λ [var type] (case var [(V) 'Red] [(E) 'Green] [(F) 'Blue] [else 'Purple]))
                   geo-7-bridge-agent
                   #(V E F) #(=))))

(define geo-interdisciplinary-screen : (->* () (Geo-Pin-Operator) Geo)
  (lambda [[op 'xor]]
    (parameterize ([default-procedure-body-fill 'SlateGray])
      (define symfont (desc-font (default-art-font) #:family 'math))
      (define em : Nonnegative-Flonum (font-metrics-ref symfont 'em))
      (define bacteriophage (bacteriophage-logo 16.0 #:tail.deg 24.0 #;#;#:sheath-length 0.0))
      (define stick : Geo (geo-rectangle (* em 1.618) (* em 0.382) #:fill 'GhostWhite #:stroke #false))
      (define characters (list #\Σ #\Ψ #\∃ #\∀ #\Γ #\→ #\∆ #\∞ #\⊗ #\⋀))
      (define symbols : (Listof Geo)
        (for/list ([sym (in-list characters)])
          (geo-scale (geo-rotate (geo-symbol sym symfont) (* (random) 2pi))
                     (+ 0.30 (* (random) 0.7)))))
      
      (define bacterium
        (dia-procedure #:iofill (λ [var type] 'GhostWhite) #:io-width (- (geo-width stick) 1.0)
                       #:corner-radius -0.5
                       #:border (desc-stroke (default-procedure-border) #:color 'DarkGrey)
                       (λ [[rem : Nonnegative-Flonum]] : Geo
                         (geo-cc-superimpose (geo-blank (* rem 6.0) rem)
                                             (geo-7-bridge-agent rem 'LightSteelBlue)))
                       #() #false))

      (geo-vc-append #:gapsize (- em)
                     (geo-cb-superimpose #:operator 'atop
                                         (geo-pin* #:operator op
                                                   0.5 0.72 0.14 0.0 (geo-rotate bacteriophage -20.0 #false) bacterium)
                                         stick)
                     (geo-pyramid (shuffle symbols) (* em -0.5) (* em 0.5) '??)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define logo (bacteriophage-logo 128.0))
(define icon (geo-inset logo))
(define figure (geo-inset (bacteriophage-logo 128.0 #:tail-color 'SteelBlue #:tail-alpha 1.0)))
(define mini-icon (geo-inset (bacteriophage-logo 128.0 #:sheath-length 0.0)))

(define splash:PPT (geo-slides-screen))
(define splash:STEM (geo-interdisciplinary-screen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  logo
  figure
  (geo-frame logo #:background 'Aquamarine)
  mini-icon
  splash:PPT

  splash:STEM

  #;(for/list : (Listof Geo) ([op (in-list geo-pin-operators)])
    (geo-rt-superimpose (geo-interdisciplinary-screen op)
                        (geo-text op))))
