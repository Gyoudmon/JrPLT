#lang typed/racket/base

(require bitmap)
(require geofun)
(require geofun/digitama/avatar/bacteriophage)
(require geofun/digitama/schematic/procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-splash-screen : (-> Bitmap)
  (lambda []
    (bitmap-procedure #:iofill
                      (λ [var] (case var
                                 [(V) 'Red]
                                 [(E) 'Green]
                                 [(F) 'Blue]
                                 [else 'Purple]))
                      
                      (λ [[em : Nonnegative-Flonum]] : Bitmap
                        (define io:width : Flonum (* em 1.618))
                        (define in:gapsize : Flonum (* em 0.618))

                        (define-dryland-wani! 7-bridge-agent [io:width in:gapsize #:anchor '#:A] #:-
                          (step-up 3 '#:B)
                          (drift -1.4 '(-0.618+1.5i) '#:C)
                          (drift '#:B '(+0.618-1.5i))
                          (drift +1.4 '(+0.618+1.5i) '#:D)
                          (drift '#:B '(-0.618-1.5i))
                          (jump-back)
                          (drift '#:A '(-0.618+3.0i))
                          (drift '#:C '(-0.618+0.0i)))
                        
                        (geo-freeze 7-bridge-agent #:color 'Yellow #:fill 'Gray #:fill-style 'odd-even))
                      '(V E F) '(=))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define logo (bacteriophage-logo 128.0))
(define icon (bitmap-inset logo))
(define figure (bitmap-inset (bacteriophage-logo 128.0 #:tail-color 'SteelBlue #:tail-alpha 1.0)))
(define mini-icon (bitmap-inset (bacteriophage-logo 128.0 #:sheath-length 0.0)))

(define splash (bitmap-splash-screen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  logo
  figure
  (bitmap-frame logo #:fill 'Aquamarine)
  mini-icon
  splash)
