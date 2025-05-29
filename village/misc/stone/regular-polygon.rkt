#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define R : Flonum 64.0)
(define gap : Flonum 32.0)

(define regular-polygon : (->* (Index Flonum) (Byte) Geo)
  (lambda [n pi-m [step 1]]
    (geo-cc-superimpose (geo-circle R #:stroke 'silver)
                        (geo-star-polygon n step R (* pi pi-m))
                        (geo-circle 1 #:fill 'black))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define polygons
  (geo-table* (list (list (regular-polygon 3 -0.5) (regular-polygon 4 -0.25)   (regular-polygon 5 -0.5)   (regular-polygon 8 0.0))
                    (list                   #false                    #false (regular-polygon 5 -0.5 2) (regular-polygon 8 0.0 3)))
              'cc 'cc gap gap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  polygons)