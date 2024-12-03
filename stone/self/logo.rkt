#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require geofun/digitama/avatar/bacteriophage)
(require geofun/digitama/schematic/procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-7-bridge-agent : (-> Nonnegative-Real Geo)
  (lambda [rem]
    (define flem : Nonnegative-Flonum (real->double-flonum rem))
    (define io:width   : Nonnegative-Flonum (* flem 1.618))
    (define in:gapsize : Nonnegative-Flonum (* flem 0.618))

    (define-gomamon! 7-bridge-agent
      [io:width in:gapsize #:anchor '#:A #:stroke 'Yellow #:fill 'Gray #:fill-rule 'even-odd] #:-
      (move-up 3 '#:B)
      (drift -1.4 '(-0.618+1.5i) '#:C)
      (drift '#:B '(+0.618-1.5i))
      (drift +1.4 '(+0.618+1.5i) '#:D)
      (drift '#:B '(-0.618-1.5i))
      (jump-back)
      (drift '#:A '(-0.618+3.0i))
      (drift '#:C '(-0.618+0.0i)))
    
    7-bridge-agent))

(define geo-splash-screen : (-> Geo)
  (lambda []
    (geo-procedure #:iofill (λ [var] (case var [(V) 'Red] [(E) 'Green] [(F) 'Blue] [else 'Purple]))
                   geo-7-bridge-agent
                   '(V E F) '(=))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define logo (bacteriophage-logo 128.0))
(define icon (geo-inset logo))
(define figure (geo-inset (bacteriophage-logo 128.0 #:tail-color 'SteelBlue #:tail-alpha 1.0)))
(define mini-icon (geo-inset (bacteriophage-logo 128.0 #:sheath-length 0.0)))

(define splash (geo-splash-screen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  logo
  figure
  (geo-frame logo #:background 'Aquamarine)
  mini-icon
  splash)
