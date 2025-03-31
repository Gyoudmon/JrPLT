#lang typed/racket/base

(require geofun/vector)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define layer-brick-number : (-> Integer Natural)
  (lambda [layer]
    (cond [(<= layer 0) 0]
          [(= layer 1) 1]
          [else (+ layer (layer-brick-number (sub1 layer)))])))

(define layer-brick-number/eqdiff : (-> Integer Byte Natural)
  (lambda [layer diff]
    (cond [(<= layer 0) 0]
          [(= layer 1) 1]
          [else (+ 1 (* diff (sub1 layer)))])))

(define brick-heap : (-> Index (Option Byte) Geo)
  (let ([block (geo-rectangle 32 16 #:stroke 'orangered #:fill 'firebrick)])
    (lambda [layer diff]
      (cond [(<= layer 0) (geo-blank)]
            [else (geo-vc-append*
                   (for/list ([l (in-range (add1 layer))])
                     (geo-hc-append* (if (not diff)
                                         (make-list (layer-brick-number l) block)
                                         (make-list (layer-brick-number/eqdiff l diff) block)))))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bricks/hard (brick-heap 5 #false))
(define bricks/easy (brick-heap 5 2))

(module+ main
  bricks/hard
  bricks/easy)
