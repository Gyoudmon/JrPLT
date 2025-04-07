#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

(require plot/pict)
(require pict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-rough-hookes-law : (-> (Vector Real Real) (Values Real (-> Real Real)))
  (lambda [xy]
    (define k (/ (vector-ref xy 1) (vector-ref xy 0)))

    (values k
            (λ [[x : Real]] : Real
              (* k x)))))

(define hookes-fit/least-square-method : (-> (Listof (Vector Real Real)) (Values Real Real Real Real (-> Real Real)))
  (lambda [data]
    (define N : Real (add1 (length data)))
  
    (define-values (Σx Σy Σxy Σx² xmax ymax)
      (for/fold ([Σx : Real 0.0]
                 [Σy : Real 0.0]
                 [Σxy : Real 0.0]
                 [Σx² : Real 0.0]
                 [xmax : Real 0]
                 [ymax : Real 0])
                ([datum (in-list data)])
        (define x : Real (vector-ref datum 0))
        (define y : Real (vector-ref datum 1))
        
        (values (+ Σx x)
                (+ Σy y)
                (+ Σxy (* x y))
                (+ Σx² (* x x))
                (max x xmax)
                (max y ymax))))
    
    (define xbar : Real (/ Σx N))
    (define ybar : Real (/ Σy N))
    (define k : Real (/ (- Σxy (* N xbar ybar))
                        (- Σx² (* N xbar xbar))))
    (define b : Real (- ybar (* k xbar)))
    
    (values k b xmax ymax
            (λ [[x : Real]] : Real
              (+ (* k x) b)))))

(define hookes-law-plot : (->* ((Listof (Vector Real Real))) (Real) pict)
  (lambda [data [sfactor 1.0]]
    (define-values (k b xmax ymax fx) (hookes-fit/least-square-method data))

    (parameterize ([plot-pen-color-map 'pastel2])
      (scale (plot ;#:title "胡克定律探究实验"
                   #:x-label "砝码质量(m/g)" #:y-label "弹簧伸长长度(ΔL/cm)"
                   #:x-min 0 #:x-max xmax #:y-max (ceiling ymax)
                   #:width 350 #:height 350
                   
                   (list* (points #:sym 'fullcircle1 #:color 'black data)
                          (function fx #:label (format "回归直线(k = ~a)" (~r k #:precision 4)) #:color 'ForestGreen)
                          
                          (for/list : (Listof renderer2d) ([xy (in-list data)]
                                                           [c (in-naturals 4)])
                            (define-values (k f) (make-rough-hookes-law xy))
                            (function #:label (and (even? c) (format "ΔL = ~am" (~r k #:precision 3)))
                                      #:color c #:alpha 0.5
                                      f))))
             sfactor))))

(define data : (Listof (Vector Real Real))
  (list #(100 8.9)
        #(90 7.5)
        #(80 6.5)
        #(70 5.2)
        #(60 3.9)
        #(50 2.9)
        #(40 1.7)
        #(30 0.6)
        #(20 0.1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (hookes-law-plot data 0.72))
