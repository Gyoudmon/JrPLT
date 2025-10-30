#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/math)

(require geofun/vector)
(require geofun/markup)

(require diafun/flowchart)
(require diafun/flowlet)

(require diafun/digitama/avatar/procedure)

(require "../style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-rnr-helper.dia : (->* () (Any Any) Geo)
  (lambda [[P1 '|Puzzel 1|] [P2 '|Puzzel 2|]]
    (define-values (p1-label p1-anchor) (aoc-extract-flow-connect-to-info P1))
    (define-values (p2-label p2-anchor) (aoc-extract-flow-connect-to-info P2))

    (parameterize ([default-diaflow-arrow-label-inline? #false])
      (define-flowchart! rnr-helper.dia [#:start-name "Read Reports" #:background 'White] #:-
        (move-down 1 '|Initialize Reports|!)
        (move-down 1 '>>|read line|)

        [#:tree (move-down 1 '#:|is line a string|?)

         [=> (move-left 1 #false "Yes")
             (move-down 1 '|split line|)
             (move-down 1 'λ|convert levels|)
             (move-down 1 '|cons reports|)
             (move-down 1 #false)
             (move-left 1 #false "Loop")
             (L-step '>>|read line|)]
        
         [=> (move-right 1 #false "No")
             (move-down 1 '<<reports)
             (move-down 1 '#:-=)

             [=> (move-down 0.5)
                 (move-left 0.75 #false p1-label)
                 (move-down 1 p1-anchor)]
             
             [=> (move-down 0.5)
                 (move-right 0.75 #false p2-label)
                 (move-down 1 p2-anchor)]]])
      
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

                     [=> (move-down 1.2)
                         (move-left 0.8)
                         (move-down 1.2 (string->symbol "碎片名称\nRead Reports"))]
                     
                     [=> (move-down 1.2)
                         (move-right 0.8)
                         (move-down 1.2 (string->symbol "算法名称\nRead Reports"))
                         (move-down 2.0 (string->symbol "函数名称\nread-reports"))])))

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
                    #:output-desc (list <s:small>)
                    read-char (list (open-input-bytes #"\n" 'rptin)
                                    (open-input-bytes #"" 'rptin))))
  
(define map.dia
  (let* ([levels (string-split "7 6 4 2 1")]
         [name '|x→y|])
    (dia-procedure #:body-fill aoc-outer-body-fill #:input-format "~s"
                   (geo-scale (geo-hc-append* #:gapsize 2.0
                                              (for/list : (Listof Geo) ([l (in-list levels)])
                                                (dia-procedure #:body-fill aoc-inner-body-fill #:iofill aoc-inner-iofill
                                                               #:output-format " ~a " #:input-format "~s"
                                                               (aoc-text name) #(#false) #false
                                                               (list l) (string->number l))))
                              0.36)
                   #(|f(x)| list) 'list
                   (list name levels) (map string->number levels))))

(define filter.dia
  (parameterize ([print-boolean-long-form #false])
    (let ([levels (map string->number (string-split "9 -7i 6< 2" #;1))])
      (dia-procedure #:body-fill aoc-outer-body-fill
                     (geo-scale (geo-hc-append* #:gapsize 4.0
                                                (for/list : (Listof Geo) ([l (in-list levels)])
                                                  (dia-procedure #:io-datum-width -2.0
                                                                 #:body-fill aoc-inner-body-fill #:iofill aoc-inner-iofill
                                                                 (geo-scale (aoc-text natural?) 0.75)
                                                                 #(#false) #false
                                                                 (list l) (natural? l))))
                                0.36)
                     #(pred? list) 'list
                     (list natural? (reverse levels)) (reverse (filter natural? levels))))))
  
(define pipeline.dia
  (let ([? (geo-art-text "?" #:stroke 'Crimson)]
        [geo-var (λ [[x : String]] : Geo (aoc-text (string->symbol x)))])
    (define map.dia : Geo
      (geo-scale (dia-procedure 'map
                                `("f(x)" ,(geo-var "s"))
                                (geo-var "Maybe\nLevels")
                                #("String ->\n(U Complex\n  False)" "Listof\nString")
                                "Listof\n(U Complex\n  False)")
                 1.00))
    (define width (geo-width map.dia))
    (define height (geo-height ?))
    (dia-procedure #:body-fill aoc-outer-body-fill
                   (geo-vc-append #:gapsize 16.0
                                  map.dia
                                  (geo-cc-superimpose (geo-rectangle #:stroke (desc-stroke #:color 'Crimson #:dash 'long-dash)
                                                                     width (* height 1.618) -0.125)
                                                      ?))
                   `(,(geo-var "s"))
                   (geo-var "Just\nLevels")
                   #("Listof\nString")
                   "Listof\nNatural")))
  
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

  pipeline.dia
  map.dia
  filter.dia)
