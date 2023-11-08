#lang racket

(provide (all-defined-out))

(require digimon/format)
(require digimon/git)

(require racket/draw)
(require pict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-loc-time-series
  (lambda [#:date0 [date-start #false] #:daten [date-end #false]
           #:line0 [line-start #false] #:linen [line-end #false]
           #:line-axis-count [axis-count #false] #:line-peak-ahead-factor [peak-factor 1000]
           #:mark-font [mark-font (make-font #:family 'system #:size 10)]
           #:mark-color [mark-color (make-color #x6A #x73 #x7D)]
           #:axis-color [axis-color (make-color #x00 #x00 #x00 0.3)]
           flwidth flheight datasource altcolors date-delta]
    (dc (λ [dc dx dy]
          (define saved-font (send dc get-font))
          (define saved-color (send dc get-text-foreground))
          (define saved-pen (send dc get-pen))
          (define saved-brush (send dc get-brush))

          (send dc set-smoothing 'aligned)
          (send dc set-font mark-font)
        
          (define 1ch (send dc get-char-width))
          (define 1em (send dc get-char-height))
          (define 1ex (* 1em 1/2))
          
          (define-values (locsource peak x0 xn)
            (for/fold ([src null] [all-peak 0] [x0 +inf.0] [xn 0])
                      ([lang-src (in-list datasource)])
              (define lang (git-language-name lang-src))
              (define pen (make-pen #:color (language-rgba lang-src altcolors)))
              (define stats (git-language-content lang-src))
              (define-values (date0 daten)
                (cond [(null? stats) (values x0 xn)]
                      [else (values (caar stats) (car (last stats)))]))
              (define-values (LoCs total peak)
                (for/fold ([LoCs null] [total 0] [peak 0])
                          ([stat (in-list stats)])
                  (define-values (adds dels) (git-numstats->additions+deletions* (list stat)))
                  (define total++ (+ total (- adds dels)))
                  (values (cons (cons (car stat) total++) LoCs)
                          total++
                          (max peak total++))))
              (values (cons (vector lang pen (reverse LoCs) total) src)
                      (max all-peak peak)
                      (min x0 date0)
                      (max xn daten))))
          
          (define-values (sec0 secn line0 linen)
            (values (or date-start x0)
                    (or date-end xn)
                    (or line-start 0)
                    (or line-end (* (max (exact-ceiling (/ peak peak-factor)) 1) peak-factor))))
          
          (define-values (mark-max-width _h _d _s) (send dc get-text-extent (~integer linen) mark-font #true))
          (define x-start (+ dx mark-max-width 1ch))
          (define x-range (- (+ dx flwidth) x-start mark-max-width))
          (define y-start (- (+ dy flheight) 1ex 1em))
          (define y-range (- y-start dy 1ex))
          (define date-interval (- secn sec0))
          (define line-interval (- linen line0))
          (define date-fraction (/ x-range date-interval))
          (define line-fraction (/ y-range line-interval))

          (send dc set-pen axis-color 1 'solid)
          (send dc set-text-foreground mark-color)
          
          (let draw-x-axis ([this-endx 0.0]
                            [this-sec sec0]
                            [last-month 0])
            (if (< this-sec secn)
                (let ([the-date (seconds->date this-sec)])
                  (define-values (year month) (values (date-year the-date) (date-month the-date)))
                  (define-values (x-axis x-mark year?)
                    (cond [(and (= month 1) (not (= month last-month))) (values this-sec (number->string year) #true)]
                          [(= this-sec sec0) (values this-sec (~day (date-day the-date)) #false)]
                          [(= last-month month) (values this-sec (~day (date-day the-date)) #false)]
                          [else (values this-sec (~month month) #false)]))

                  (when (and year?)
                    (let ([self-x (+ x-start (* (- x-axis sec0) date-fraction))])
                      (send dc set-pen axis-color 1 'short-dash)
                      (send dc draw-line self-x y-start self-x (- y-start y-range))
                      (send dc set-pen axis-color 1 'solid)))
                  
                  (draw-x-axis (draw-x dc (- x-axis sec0) x-mark this-endx
                                       mark-font x-start y-start
                                       date-fraction 1ex)
                               (+ this-sec date-delta)
                               month))
                (draw-x dc (- secn sec0)
                        (~day (date-day (seconds->date secn)))
                        this-endx mark-font x-start y-start
                        date-fraction 1ex)))
          
          (define adjust-count
            (cond [(and (integer? axis-count) (> axis-count 1)) axis-count]
                  [else (exact-ceiling (/ y-range (* 1em 2.0)))]))

          (if (<= adjust-count 1)
              (send* dc
                (draw-text "0" (- x-start 1ch 1ch) (- y-start 1ex) #true)
                (draw-line x-start y-start (+ x-start x-range) y-start))
              (for ([y-axis (in-range line0 (+ linen 1) (/ line-interval (- adjust-count 1)))])
                (define y (- y-start (* (- y-axis line0) line-fraction)))
                (define y-mark (if (zero? y-axis) "0" (if (exact-integer? y-axis) (~integer y-axis) (number->string y-axis))))
                (define-values (y-width _w _d _s) (send dc get-text-extent y-mark mark-font #true))
                (send dc draw-text y-mark (- x-start 1ch y-width) (- y 1ex) #true)
                (send dc draw-line x-start y (+ x-start x-range) y)))
          
          (for ([loc-src (in-list locsource)])
            (send dc set-pen (vector-ref loc-src 1))
            (send dc set-text-foreground (send (vector-ref loc-src 1) get-color))
            (define this-y-axis (vector-ref loc-src 3))
            (define y (- y-start (* (- this-y-axis line0) line-fraction)))
            (send dc draw-text (number->string this-y-axis) (+ x-start x-range 1ch) (- y 1ex) #true)
            (send dc draw-lines
                  (for/list ([date.LoC (in-list (vector-ref loc-src 2))])
                    (define-values (x-axis y-axis) (values (car date.LoC) (cdr date.LoC)))
                    (cons (+ x-start (* (- x-axis sec0) date-fraction))
                          (- y-start (* (- y-axis line0) line-fraction))))))
        
          (send* dc
            (set-font saved-font)
            (set-text-foreground saved-color)
            (set-pen saved-pen)
            (set-brush saved-brush)))
        flwidth flheight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-x dc x-axis x-mark last-mark-endx mark-font x-start y-start date-fraction 1ex)
  (define x (+ x-start (* x-axis date-fraction)))
  (define-values (x-width x-height _d _s) (send dc get-text-extent x-mark mark-font #true))
  (let ([mark-x (- x (/ x-width 2))])
    (cond [(> mark-x last-mark-endx)
           (send dc draw-line x y-start x (+ y-start 1ex))
           (send dc draw-text x-mark mark-x (+ y-start 1ex) #true)
           (+ mark-x x-width)]
          [else mark-x])))

(define language-rgba
  (lambda [lang altcolors]
    (define maybe-color
      (or (assoc (language-name (git-language-name lang)) altcolors)
          (git-language-color lang)))
    
    (cond [(not maybe-color) (make-color 0 0 0 1.0)]
          [(pair? maybe-color) (symbol->string (cdr maybe-color))]
          [(keyword? maybe-color)
           (let ([rgb (string->number (keyword->string maybe-color) 16)])
             (make-color (bitwise-and (arithmetic-shift rgb -16) #xFF)
                         (bitwise-and (arithmetic-shift rgb -8) #xFF)
                         (bitwise-and rgb #xFF)
                         1.0))]
          [else (make-color 0 0 0 1.0)])))

(define language-name
  (lambda [lang]
    (define maybe-name-parts (regexp-match #px"(\\w+)\\s*[(]\\w+[)]" lang))
    (if (not maybe-name-parts) lang (cadr maybe-name-parts))))

(define ~month
  (lambda [m]
    (cond [(< m 10) (string-append "0" (number->string m))]
          [else (number->string m)])))

(define ~day
  (lambda [d]
    (format "~~~a" d)))
