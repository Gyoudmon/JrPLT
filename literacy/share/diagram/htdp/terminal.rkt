#lang typed/racket/base

(require expfun/position)
(require geofun/vector)
(require diafun/flowchart)
(require plotfun/line)

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define monofont (desc-font #:family 'monospace #:size 24))
(define scale 2.5)

(define cell : Geo
  (geo-rectangle #:stroke 'GhostWhite #:fill #x444444
                 (* (font-metrics-ref monofont 'ch) scale)
                 (* (font-metrics-ref monofont 'em) scale)))

(define terminal : (->* (Index Index) ((Listof String)) Geo)
  (lambda [col row [outs null]]
    (define rmax : Index (length outs))
    
    (geo-table*
     (build-list row (λ [[r : Index]]
                       (if (< r rmax)
                           (build-list col
                                       (λ [[c : Index]]
                                         (define out (list-ref outs r))

                                         (if (< c (string-length out))
                                             (geo-cc-superimpose cell
                                                                 (geo-text #:color 'GhostWhite
                                                                           (string (string-ref out c)) monofont))
                                             cell)))
                           (make-list col cell)))))))

(define-values (col row) (values 20 5))

(define col-line
  (plot-integer-line #:mark-style (make-plot-mark-style #:pin-length 0.0)
                     #:exclude-zero? #false
                     #:range (cons 0 col)
                     #:ticks (plot-fixed-ticks 0 col 1)))

(define col-line5
  (plot-integer-line #:mark-style (make-plot-mark-style #:pin-length 0.0)
                     #:exclude-zero? #false
                     #:length 150.0
                     #:range (cons 0 9)))

(define std-terminal
  (terminal col row
            (list "Terminal Console"
                  (format "size: ~a x ~a" col row))))

(define hollow-rhombus
  (terminal 9 9
            (list "    *"
                  "   * *"
                  "  *   *"
                  " *     *"
                  "*       *"
                  " *     *"
                  "  *   *"
                  "   * *"
                  "    *")))

(define-flowchart! rhombus.dia [#:start-name "Hollow Rhombus" #:background 'White
                                #:node-desc #hasheq((|λDisplay Hollow Line1| . "Display Hollow Line")
                                                    (|λDisplay Hollow Line2| . "Display Hollow Line"))] #:-
  (move-down 1 '>>|Read n|)

  [#:tree (move-down 1 '#:|1 <= l <= n|?)
   [=> (move-down 1 '|λDisplay Hollow Line1|)
       (move-left 1)
       (L-step '#:|1 <= l <= n|? #false "for")]

   [=> (move-right 1 #false "N")
       (move-down '|λDisplay Hollow Line1|)
       (move-down 0.75)
       (move-left '#:home)

       [#:tree (move-down 0.75 '#:|n - 1 >= l >= 1|?)
        [=> (move-down 1 '|λDisplay Hollow Line2|)
            (move-left 1)
            (L-step '#:|n - 1 >= l >= 1|? #false "for")]
        
        [=> (move-right 1 #false "N")
            (move-down '|λDisplay Hollow Line2|)
            (move-down 0.75)
            (move-left '#:home)
            (move-down 0.75 'End$)]]]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  std-terminal
  hollow-rhombus
  col-line
  col-line5
  rhombus.dia)
