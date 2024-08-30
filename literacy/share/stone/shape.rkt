#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define math-font (desc-font #:size 14.0 #:family 'math))
(define cs-line (desc-stroke #:color 'gray #:dash 'short-dash))
(define arrow-color 'lightsteelblue)
(define ch (font-metrics-ref math-font 'ch))
(define x-axis-length 384.0)
(define y-axis-length (* x-axis-length 0.75))
(define ar 4.0)

(define (cs-xyline [x : Real] [y : Real]) : Geo
  (geo-rb-superimpose
   (geo-hline x 0.5 #:stroke cs-line)
   (geo-vline 0.5 y #:stroke cs-line)))

(define (cs-dot [x : Real] [y : Real] [c : Color 'black]) : Geo
  (geo-pin* 1.0 1.0 0.5 0.5 (cs-xyline x y)
               (geo-circle 2.0 #:stroke #false #:fill c)))

(define (cs-rect [x : Real] [y : Real] [w : Real] [h : Real] [c : Color 'black]) : Geo
  (geo-pin* 1.0 1.0 0.0 0.0 (cs-xyline x y)
               (geo-ct-superimpose (geo-rc-superimpose
                                       (geo-rectangle w h #:stroke c)
                                       (geo-text "h " math-font))
                                      (geo-text "w " math-font))))

(define (cs-circle [x : Real] [y : Real] [r : Real] [c : Color 'black]) : Geo
  (geo-pin-over (cs-xyline x y)
                   x y
                   (geo-hc-append (geo-rc-superimpose
                                      (geo-circle r #:stroke c)
                                      (geo-arrow ar (- r ar) #:fill arrow-color #:stroke #false))
                                     (geo-text 'R math-font))
                   r r))

(define (cs-ellipse [x : Real] [y : Real] [a : Real] [b : Real] [c : Color 'black]) : Geo
  (geo-pin-over (cs-xyline x y)
                   x y
                   (geo-vc-append
                    (geo-hc-append
                     (geo-cb-superimpose (geo-rc-superimpose
                                             (geo-ellipse (* a 2) (* b 2) #:stroke c)
                                             (geo-arrow ar (- a ar) #:fill arrow-color #:stroke #false))
                                            (geo-arrow ar (- b ar) pi/2 #:fill arrow-color #:stroke #false))
                     (geo-text 'a math-font))
                    (geo-text 'b math-font))
                   a b))

(define (cs-regular-polygon [n : Byte] [x : Real] [y : Real] [r : Real] [c : Color 'black]) : Geo
  (geo-pin-over (cs-xyline x y)
                   x y
                   (geo-hc-append (geo-rc-superimpose
                                   (geo-cc-superimpose
                                    (geo-circle r #:stroke 'grey)
                                    (geo-regular-polygon n r #:stroke c))
                                   (geo-arrow ar (- r ar) #:fill arrow-color #:stroke #false))
                                  (geo-text 'R math-font))
                   r r))

(define (cartesian-pin [cs : Geo] [shapes : (Listof Geo)]) : Geo
  (for/fold ([bmp : Geo cs])
            ([shape (in-list shapes)])
    (geo-pin-over bmp (+ (* ch 2) 2) 2 shape)))

(define (make-cartesian-demo [scale : Nonnegative-Flonum 1.0])
  (geo-scale
   (cartesian-pin
    (geo-hb-append #:gapsize ch
                   (geo-text "y " math-font)
                   (geo-lt-superimpose
                    (geo-vr-append
                     (geo-arrow ar x-axis-length #:stroke #false #:fill 'Black)
                     (geo-text "x" math-font))
                    (geo-arrow ar y-axis-length pi/2 #:stroke #false #:fill 'Black)))
    
    (list (cs-dot 30 40 'seagreen)
          (cs-rect 60 60 60 42 'forestgreen)
          (cs-circle 200 120 24 'royalblue)
          (cs-ellipse 140 200 32 24 'crimson)

          (cs-regular-polygon 3 320 50 32 'orange)
          (cs-regular-polygon 4 320 150 32 'green)
          (cs-regular-polygon 5 320 250 32 'blue)))
   scale))

(define shape-demo (make-cartesian-demo 2.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  shape-demo)
