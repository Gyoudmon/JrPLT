#lang racket

(require plot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-linear-function
  (lambda [k b]
    (λ [x] ; f(x) = kx + b
      (+ (* k x) b))))

(module+ main
  (parameterize ([plot-pen-color-map 'pastel2])
    (define kb-data
      (list (cons 2 4)
            (cons 2 -4)
            (cons 2 0)
            (cons 3 4)))
    
    (plot #:title "一次函数图像"
          #:x-label "x" #:y-label "f(x)"
          #:x-min -8 #:x-max 8 #:y-min -8 #:y-max 8
          #:width 400 #:height 400
          
          (list* (y-axis)
                 (x-axis)
                 (for/list ([kb (in-list kb-data)]
                            [c (in-naturals)])
                   (define-values (k b) (values (car kb) (cdr kb)))
                   (define fx (make-linear-function k b))
                   (function #:label (cond [(> b 0) (format "f(x) = ~ax + ~a" k b)]
                                           [(< b 0) (format "f(x) = ~ax - ~a" k (- b))]
                                           [else    (format "f(x) = ~ax" k)])
                             #:color c
                             fx))))))
