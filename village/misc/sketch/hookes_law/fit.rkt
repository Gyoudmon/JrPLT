#lang typed/racket

(require plotfun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 粗糙的函数
(define make-rough-hookes-law : (-> Complex (Values Real (-> Real Real)))
  (lambda [xy]
    (define k (/ (imag-part xy) (real-part xy)))

    (values k
            (λ [[x : Real]] : Real
              (* k x)))))

; 函数拟合/最小二乘法
(define hookes-fit/least-square-method : (-> (Listof Complex) (Values Real Real (-> Real Real)))
  (lambda [data]
    (define N : Real (add1 (length data)))
  
    (define-values (Σx Σy Σxy Σx²)
      (for/fold ([Σx : Real 0.0]
                 [Σy : Real 0.0]
                 [Σxy : Real 0.0]
                 [Σx² : Real 0.0])
                ([datum (in-list data)])
        (define x : Real (real-part datum))
        (define y : Real (imag-part datum))
        
        (values (+ Σx x)
                (+ Σy y)
                (+ Σxy (* x y))
                (+ Σx² (* x x)))))
    
    (define xbar : Real (/ Σx N))
    (define ybar : Real (/ Σy N))
    (define k : Real (/ (- Σxy (* N xbar ybar))
                        (- Σx² (* N xbar xbar))))
    (define b : Real (- ybar (* k xbar)))
    
    (values k b
            (λ [[x : Real]] : Real
              (+ (* k x) b)))))

(module+ main
  (define data : (Listof Complex)
    (list 0+0i
          20+3i
          40+7i
          60+10i
          80+13i
          100+16i))
  
  (define-values (k b fx) (hookes-fit/least-square-method data))

  (parameterize ([default-plot-visualizer-label-position 1.0])
    (plot-cartesian #:x-label "砝码质量" #:y-label "重力大小"
                    #:x-desc "m/g" #:y-desc "G/N"
                    #:mark-style (make-plot-mark-style #:pin-angle 0.0 #:gap-angle 0.0)
                    #:x-range (cons 0 1000) #:y-range (cons 0 10)
                    #:width 600 #:height 600
                    
                    (list #;(lines #:color 'black
                                 data)
                          #;(function #:label (format "最小二乘法线性拟合(k = ~a)" (~r k #:precision 4))
                                    #:color 'ForestGreen
                                    fx)
                          #;(for/list : (Listof Plot-Visualizer) ([xy (in-list (cdr data))])
                              (define-values (k f) (make-rough-hookes-law xy))
                              (function #:label (format "ΔL = ~am" (~r k #:precision 4))
                                        #:opacity 0.25
                                        f))))))
