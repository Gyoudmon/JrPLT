#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-2d-array : (->* (Positive-Byte Positive-Byte) ((Listof (List Byte Byte Color)) #:size Real #:gapsize Nonnegative-Real) Geo)
  (lambda [row col [cells null] #:size [size 24] #:gapsize [gap 8.0]]
    (geo-table* (cons (cons (geo-blank)
                            (for/list : (Listof Geo) ([c (in-range col)])
                              (geo-text (format "第~a列" c) #:color 'green)))
                      (for/list : (Listof (Listof Geo)) ([r (in-range row)])
                        (cons (geo-text (format "第~a行" r) #:color 'blue)
                              (for/list : (Listof Geo) ([c (in-range col)])
                                (define specell (assq r cells))
                                (if (and specell (eq? (cadr specell) c))
                                    (geo-square size #:fill (caddr specell))
                                    (geo-square size))))))
                'cc 'cc gap gap)))

(define 2d-array
  (make-2d-array 8 8
                 '((2 3 cyan)
                   (4 4 purple))))

(module+ main
  2d-array)
