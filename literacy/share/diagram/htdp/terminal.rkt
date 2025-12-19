#lang typed/racket/base

(require expfun/position)
(require geofun/vector)
(require geofun/markup)
(require diafun/flowchart)
(require plotfun/line)

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define monofont (desc-font #:family 'monospace #:size 24))
(define scale 2.5)

(define terminal : (->* (Index Index) ((Listof String)) Geo)
  (lambda [col row [outs null]]
    (define rmax : Index (length outs))

    (define cell : Geo
      (geo-rectangle #:stroke 'GhostWhite #:fill #x444444
                     (* (font-metrics-ref monofont 'ch) scale)
                     (* (font-metrics-ref monofont 'em) scale)))
    
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

(define std-terminal
  (terminal col row
            (list "Terminal Console"
                  (format "size: ~a x ~a" col row))))

(define-flowchart! ASCII-Art.dia [#:start-name "Filled ASCII Shape" #:background 'White
                                  #:block-desc #hasheq((|&Display Line| . "Display\nLine")
                                                       (|&Display Line.| . "Display\nLine"))] #:-
  (move-down 1 '>>|Read n|)

  [#:tree (move-down 1 '#:|1 <= L <= n|?)
   [=> (move-down 1 '|&Display Line|)
       (move-left 1)
       (L-step '#:|1 <= L <= n|? #false "for")]

   [=> (move-right 1 #false "N")
       (move-down '|&Display Line|)
       (move-down 0.75)
       (move-left '#:home)
       (move-down 0.75 'End$)

       (jump-down 1 '|&Display Line.|)
       (move-down 1 '<<|Display WhiteSpaces|)
       (move-down 1 '<<|Display Characters|)
       (move-down 1 '<<|New Line|)]])

(define-flowchart! standalone-ASCII-Art.dia [#:start-name "Filled ASCII Shape" #:background 'White] #:-
  (move-down 1 '>>|Read n|)

  [#:tree (move-down 1 '#:|1 <= L <= n|?)
   [=> (move-down 1 '<<|Display WhiteSpaces|)
       (move-down 1 '<<|Display Characters|)
       (move-down 1 '<<|New Line|)
       (move-left 1)
       (L-step '#:|1 <= L <= n|? #false "for")]

   [=> (move-right 1 #false "N")
       (move-down '<<|New Line|)
       (move-down 0.75)
       (move-left '#:home)
       (move-down 0.75 'End$)]])

(define-flowchart! display-detailed-line.dia #:start '|&Display Line.| [#:background 'White
                                                                        #:block-desc #hasheq((|&Display Line.| . "Display\nLine"))] #:-
  [#:tree (move-down 1 '#:|1 <= C < pos(L)|?)
   [=> (move-down 1 '<<|Display WhiteSpace|)
       (move-left 1)
       (L-step '#:|1 <= C < pos(L)|? #false "for")]

   [=> (move-right 1 #false "N")
       (move-down '<<|Display WhiteSpace|)
       (move-down 0.75)
       (move-left '#:home)

       [#:tree (move-down 0.75 '#:|1 <= C <= span(L)|?)
        [=> (move-down 1 '<<|Display Character|)
            (move-left 1)
            (L-step '#:|1 <= C <= span(L)|? #false "for")]
        
        [=> (move-right 1 #false "N")
            (move-down '<<|Display Character|)
            (move-down 0.75)
            (move-left '#:home)
            (move-down 0.75 '<<newline)]]]])

(define-flowchart! mirror.dia [#:start-name "Mirrored Shape" #:background 'White
                               #:block-desc #hasheq((|λDisplay Line1| . "Display Line")
                                                    (|λDisplay Line2| . "Display Line"))] #:-
  (move-down 1 '>>|Read n|)

  [#:tree (move-down 1 '#:|1 <= l <= n|?)
   [=> (move-down 1 '|λDisplay Line1|)
       (move-left 1)
       (L-step '#:|1 <= l <= n|? #false "for")]

   [=> (move-right 1 #false "N")
       (move-down '|λDisplay Line1|)
       (move-down 0.75)
       (move-left '#:home)

       [#:tree (move-down 0.75 '#:|n - 1 >= l >= 1|?)
        [=> (move-down 1 '|λDisplay Line2|)
            (move-left 1)
            (L-step '#:|n - 1 >= l >= 1|? #false "for")]
        
        [=> (move-right 1 #false "N")
            (move-down '|λDisplay Line2|)
            (move-down 0.75)
            (move-left '#:home)
            (move-down 0.75 'End$)]]]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  std-terminal
  col-line
  ASCII-Art.dia
  standalone-ASCII-Art.dia
  display-detailed-line.dia
  mirror.dia)
