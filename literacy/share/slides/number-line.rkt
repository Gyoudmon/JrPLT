#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require geofun/markup)
(require plotfun/line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bi-style (make-plot-axis-style #:tip plot-bi-tip))
(define no-pin (plot-template '(61.8 %) -pi/2 #:pin? #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rational-desc : Plot-Mark->Description
  (lambda [pt datum font color transform]
    (define unit (real-part (- (transform 1.0+0.0i) (transform 0.0+0.0i))))
    (define offset (* unit 0.1-0.01i))
    (define n (real-part pt))

    (if (>= n 0)
        (when (< n 4)
          (make-sticker (geo-path #:stroke 'ForestGreen #:target-tip default-arrow-tip
                                  #:labels (make-geo-path-label "+1" #:font font #:color 'ForestGreen)
                                  #:scale unit
                                  (list (list 0.1 0.5-0.16i 0.9)))
                        'lb offset))

        (make-sticker (geo-path #:stroke 'Crimson #:target-tip default-arrow-tip
                                #:labels (make-geo-path-label "-1" #:font font #:color 'Crimson)
                                #:scale (* unit -1.0+1.0i)
                                (list (list 0.1 0.5-0.16i 0.9)))
                      'lb offset))))

(define frational-desc : (-> Positive-Byte Plot-Mark->Description)
  (lambda [d]
    (λ [pt datum font color transform]
      (define unit (real-part (- (transform 1.0+0.0i) (transform 0.0+0.0i))))
      
      (when (and (real? pt) (exact? pt))
        (define scale (/ unit (exact->inexact d)))
        
        (for/list : (Listof Geo-Sticker) ([idx (in-range 0 d)])
                (if (> idx 0)
                    (make-sticker (geo-path #:stroke 'RoyalBlue #:target-tip default-arrow-tip
                                            #:labels (make-geo-path-label #:font font #:color 'RoyalBlue #:rotate? #false
                                                                          (format "×~a" (add1 idx)) 0.618)
                                            #:scale (* scale idx)
                                            (list (list 0.0 0.16-0.4i 0.975)))
                                  'lb)
                    (make-sticker (geo-path #:stroke 'Crimson #:target-tip default-arrow-tip
                                            #:labels (make-geo-path-label #:font font #:color 'Crimson #:rotate? #false
                                                                          "×0" 0.618)
                                            #:scale (* scale (- idx 1) 1.0-1.0i)
                                            (list (list 0.0 0.16-0.4i 0.95)))
                                  'rb)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define robot-grasping/initial : Geo
  (plot-line #:unit-length '(12.5 %)
             #:origin 0.5
             #:style (make-plot-axis-style #:tip plot-bi-tip)
             #:ticks (plot-fixed-ticks '(0) '("O"))
             #:label (cons "西" "东")))

(define robot-grasping : Geo
  (plot-line #:unit-length '(12.5 %)
             #:origin 0.5
             #:style bi-style
             #:mark-style (make-plot-mark-style #:color 'RoyalBlue)
             #:mark-template (plot-template '(100 %) -pi/2)
             #:ticks (plot-fixed-ticks '(-3 -2 -1 0 1 2) '(#false #false #false "O" #false #false))
             #:label (cons "西" "东")
             (list (plot-integer -3 #:datum "A")
                   (plot-integer +2 #:datum "B"))))

(define definition : Geo
  (plot-integer-line #:origin 0.5
                     #:range (cons -5 5)))

(define integers : Geo
  (plot-line #:label (cons (<span> null "Z" (<sup> " -"))
                           (<span> null "N" (<sub> "0")))
             #:origin 0.5
             #:ticks (plot-real-ticks* #:minor-count 0)
             #:style bi-style
             #:range (cons -4 4)
             #:mark-template (remake-plot:mark no-pin #:desc rational-desc)
             (list -4 -3 -2 -1 0 1 2 3 4)))

(define fractions : Geo
  (let ([d 5])
    (plot-line #:label (cons (<span> null "Q" (<sup> " -"))
                             (<span> null "Q" (<sup> " +")))
               #:ticks (plot-fixed-ticks (append (list 0) (build-list (sub1 d) (λ [[i : Index]] (/ (+ i 1) d))) (list 1)))
               #:style bi-style
               #:range (cons 0 1)
               #:mark-template (remake-plot:mark no-pin #:desc (frational-desc d))
               (list (/ 1 d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  robot-grasping/initial
  robot-grasping
  definition
  integers
  fractions)
