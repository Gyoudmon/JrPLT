#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/string)

(require geofun/vector)
(require geofun/markup)

(require diafun/flowchart)
(require diafun/flowlet)

(require diafun/digitama/avatar/procedure)

(require "../aoc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-rnr-helper.dia : (->* () (Any Any) Geo)
  (lambda [[P1 '|Puzzel 1|] [P2 '|Puzzel 2|]]
    (define-values (p1-label p1-anchor) (aoc-extract-flow-connect-to-info P1))
    (define-values (p2-label p2-anchor) (aoc-extract-flow-connect-to-info P2))

    (parameterize ([default-diaflow-arrow-label-inline? #false])
      (define-flowchart! rnr-helper.dia [#:start-name "Read Reports" #:background 'White] #:-
        (move-down 1 '|Initialize Reports|!)
        (move-down 1 '>>|read line|)
        (move-down 1 '#:|is line a string|?)
        
        (move-left 1 #false "Yes")
        (move-down 1 '|split line|)
        (move-down 1 'λ|convert levels|)
        (move-down 1 '|cons reports|)
        (move-down 1 #false)
        (move-left 1 #false "Loop")
        (L-step '>>|read line|)
        
        (jump-back)
        (move-right 1 #false "No")
        (move-down 1 '<<reports)
        (move-down 1 '#:-=)
        (move-down 0.5)
        (move-left 0.75 #false p1-label)
        (move-down 1 p1-anchor)
        
        (jump-back)
        (move-down 0.5)
        (move-right 0.75 #false p2-label)
        (move-down 1 p2-anchor))
      
      rnr-helper.dia)))

(define make-rnr-p1.dia : (->* () (Any) Geo)
  (lambda [[h 'Helper]]
    (define-values (h-label h-anchor) (aoc-extract-flow-connect-from-info h))
    
    (define-flowchart! hh-p1.dia #:start h-anchor [#:start-name "Find Total Distance" #:background 'White] #:-
      (move-down 1 '>>|input|)
      (move-down 1 '|sort X|)
      (move-down 1 '|sort Y|)
      (move-down 1 'λsum)
      (move-down 1 '<<|output|)
      (move-down 1 'done$))

    hh-p1.dia))

(define make-rnr-p2.dia : (->* () (Any) Geo)
  (lambda [[h 'Helper]]
    (define-values (h-label h-anchor) (aoc-extract-flow-connect-from-info h))
    
    (define-flowchart! hh-p2.dia #:start h-anchor [#:start-name "Find Similarity Score" #:background 'White] #:-
      (move-down 1 'definition!)
      (move-down 1 '>>|input|)
      (move-down 1 'λ|weighted sum|)
      (move-down 1 '<<|output|)
      (move-down 1 'done$))

    hh-p2.dia))

(define make-split.dia : (->* (String) (Real) Geo)
  (lambda [line [gapsize 8.0]]
    (define (string-box [s : String])
      (geo-frame #:border (desc-stroke (default-stroke) #:color 'DarkOrange)
                 #:background (rgb* 'DarkOrange 0.16)
                 #:padding 4.0
                 (geo-trim (geo-art-text s))))

    (define src (string-box line))
    (geo-vc-append #:gapsize gapsize
                   src
                   (geo-arrow 8.0 32.0 pi/2)
                   (geo-h?-append* #:gapsize gapsize
                                   (cons (geo-blank 1.0 (* (geo-height src) 1.618))
                                         (map string-box (string-split line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define name-map.dia
  (parameterize ([default-diaflow-block-width 100.0])
    (make-flowchart! #:start '.home [#:background 'White] #:-
                     (jump-down 1 (string->keyword "任务名称\nread reports"))
                     (move-down 1.2)
                     (move-left 0.8)
                     (move-down 1.2 (string->symbol "碎片名称\nRead Reports"))
                     
                     (jump-back)
                     (move-down 1.2)
                     (move-right 0.8)
                     (move-down 1.2 (string->symbol "算法名称\nRead Reports"))
                     (move-down 2.0 (string->symbol "函数名称\nread-reports")))))


(define read.dia
  (dia-flowlet-read #:reader read #:grid-width -1.0 #:grid-height -1.0
                    #:peek-size 20 #:output-desc (aoc-assignment-desc 'line)
                    (open-input-bytes #"7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5" 'rptin) 6))

(define read-char.dia
  (dia-flowlet-read #:reader read-char #:grid-width -1.0 #:grid-height -1.0
                    #:peek-size 20 #:output-desc (aoc-assignment-desc 'line)
                    (open-input-bytes #"7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5" 'rptin) 6))

(define read-line.dia
  (dia-flowlet-read #:reader read-line #:grid-width -1.0 #:grid-height -1.0
                    #:peek-size 20 #:output-desc (aoc-assignment-desc 'line)
                    (open-input-bytes #"7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9" 'rptin) 6))

(define read-char-for-line.dia
  (dia-flowlet-join #:downward? #false #:and? #false
                    #:output-desc (list geo-s:small)
                    read-char (list (open-input-bytes #"\n" 'rptin)
                                    (open-input-bytes #"" 'rptin))))
  
(define map.dia
  (let* ([levels (string-split "7 6 4 2 1")]
         [x->y (procedure-rename string->number '|x→y|)]
         [|f(x)| (geo-scale (aoc-text (object-name x->y)) 0.5)]
         [geo-as (map aoc-text levels)])
    (dia-procedure #:body-fill 'Lavender
                   (geo-scale (geo-hc-append* #:gapsize 2.0
                                              (for/list : (Listof Geo) ([y (in-list levels)]
                                                                        [g (in-list geo-as)])
                                                (dia-procedure #:body-fill 'LightBlue #:iofill (λ [v t] 'AliceBlue)
                                                               |f(x)| (list #false) #false
                                                               (list g) (aoc-text (x->y y)))))
                              0.36)
                   (list '|f(x)| 'list)
                   '(list)
                   (list |f(x)| (geo-vc-append* (reverse geo-as)))
                   (geo-vc-append* (reverse (map aoc-text (map x->y levels)))))))

(define pipeline.dia
  (let ([? (geo-art-text "?" #:stroke 'Crimson)])
    (define map.dia : Geo
      (geo-scale (dia-procedure 'map
                                (list "f(x)" "s") (list "maybe\nlevels")
                                (map aoc-text (map string->symbol (list "-> String\n(U Complex\n  False)" "Listof\nString")))
                                (aoc-text (string->symbol "Listof\n(U Complex\n  False)")))
                 1.00))
    (define width (geo-width map.dia))
    (define height (geo-height ?))
    (dia-procedure #:body-fill 'Lavender
                   (geo-vc-append #:gapsize 16.0
                                  map.dia
                                  (geo-cc-superimpose (geo-rectangle #:stroke (desc-stroke #:color 'Crimson #:dash 'long-dash)
                                                                     width (* height 1.618) -0.125)
                                                      ?))
                   (list "s") (list "levels")
                   (list (aoc-text (string->symbol "Listof\nString"))) (aoc-text (string->symbol "Listof\nNatural")))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (make-rnr-helper.dia "Puzzel 1" "Puzzel 2")
  (make-rnr-p1.dia)
  (make-rnr-p2.dia)
  name-map.dia
  (make-split.dia "Red-Nosed Reindeer nuclear fusion/fission plant")
  read-char-for-line.dia

  read.dia
  read-char.dia
  read-line.dia

  map.dia
  pipeline.dia)
