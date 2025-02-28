#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define layer-width : Nonnegative-Flonum 400.0)
(define layer-height : Nonnegative-Flonum 50.0)
(define layer-subheight : Nonnegative-Flonum 28.0)
(define layer-font : Font (desc-font (default-font) #:size 'x-large))
(define layer-subfont : Font (default-font))

(define geo-arch-layer : (-> (Listof String) [#:fgcolor Color] [#:bgcolor (Option Color)] [#:border Color] Geo)
  (lambda [coms #:fgcolor [fgcolor 'Black] #:bgcolor [bgcolor #false] #:border [border (default-stroke)]]
    (define n : Index (length coms))
    (define cwidth (if (> n 1) (/ layer-width n) layer-width))
    
    (geo-hc-append*
     (for/list ([c (in-list coms)])
       (geo-cc-superimpose
        (geo-rectangle #:fill bgcolor #:stroke border
                       cwidth layer-height)
        (geo-text c layer-font #:color fgcolor))))))

(define geo-arch-sublayer : (-> String (Listof String) [#:fgcolor Color] [#:bgcolor (Option Color)] [#:border Color] Geo)
  (lambda [desc coms #:fgcolor [fgcolor 'Black] #:bgcolor [bgcolor #false] #:border [border (default-stroke)]]
    (define n : Index (length coms))
    (define gapsize : Nonnegative-Flonum 8.0)
    (define cwidth (if (> n 1) (/ (- layer-width (* (+ n 1) gapsize)) n) (- layer-width (* 2.0 gapsize))))
    
    (geo-hc-append*
     (for/list ([c (in-list coms)])
       (geo-cc-superimpose
        (geo-rectangle #:fill bgcolor #:stroke border
                       cwidth layer-subheight)
        (geo-text c layer-subfont #:color fgcolor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define architecture.dia : Geo
  (geo-vc-append (geo-arch-sublayer "第三方库" (list "SDL2"))
                 (geo-arch-layer (list "操作系统(Windows/macOS/Linux)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  architecture.dia)
