#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/list)

(require geofun/vector)
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
      (define-flowchart! hh-helper.dia [#:start-name "Read Location IDs" #:background 'White] #:-
        (move-down 1 'initialization!)
        (move-down 1 '>>|read IDs|)
        ;(jump-right 1.5 '/doc/locin)
        ;(move-left 1.5 #false "from")
        (move-down 1 '#:predicate?)
        
        (move-left 1 #false "Yes")
        (move-down 1 '|cons X|)
        (move-down 1 '|cons Y|)
        (move-down 1)
        (move-left 1 #false "Loop")
        (L-step '>>|read IDs|)
        
        (jump-back)
        (move-right 1 #false "No")
        (move-down 1 '<<result)
        (move-down 1 '#:-=)
        (move-down 0.5)
        (move-left 0.75 #false p1-label)
        (move-down 1 p1-anchor)
        
        (jump-back)
        (move-down 0.5)
        (move-right 0.75 #false p2-label)
        (move-down 1 p2-anchor))
      
      hh-helper.dia)))

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
                     (move-down 2 (string->symbol "函数名称\nread-reports")))))
  
(define read.dia
  (let ([sep (geo-hline 240.0 8.0 #:stroke 'LightGrey)]
        [make-desc (λ [a] (λ [v] (format "~a" v)))])
    (geo-vl-append (dia-flowlet-read #:peek-size 5 #:output-desc (make-desc 'a)
                                     (open-input-bytes #"3 4\n4 3\n2 5\n1 3\n3 9\n3 3" 'locin))
                   sep
                   (dia-flowlet-read #:peek-size 5 #:output-desc (make-desc 'b)
                                     (open-input-bytes  #" 4\n4 3\n2 5\n1 3\n3 9\n3 3" 'locin))
                   ;sep
                   #;(dia-flowlet-read #:peek-size 5 #:output-desc (make-desc 'c)
                                     (open-input-bytes      #"4 3\n2 5\n1 3\n3 9\n3 3" 'locin))
                   ;sep
                   #;(dia-flowlet-read #:peek-size 5 #:output-desc (make-desc 'd)
                                     (open-input-bytes       #" 3\n2 5\n1 3\n3 9\n3 3" 'locin)))))
  
(define predicate.dia
  (dia-flowlet-andmap #:input-desc '("a" "b") #:downward? #false
                      natural? (list 3 5)))

(define count.dia
  (let* ([addends (list 3 9 3 5 3 4)]
         [|x = y| (λ [v] (eq? v 3))]
         [pred? (geo-scale (aoc-art-text (object-name |x = y|)) 0.5)]
         [geo-as (map aoc-art-text addends)])
    (dia-procedure #:body-fill 'Lavender
                   (geo-scale (geo-hc-append* #:gapsize 2.0
                                              (for/list : (Listof Geo) ([y (in-list addends)]
                                                                        [g (in-list geo-as)])
                                                (dia-procedure #:body-fill 'LightBlue #:iofill (λ [v] 'AliceBlue)
                                                               pred? (list #false) #false
                                                               (list g) (aoc-art-text (if (|x = y| y) "1" "0")))))
                              0.36)
                   (list 'pred? 'list)
                   '(count)
                   (list pred? (geo-vc-append* #:gapsize -16.0 (reverse geo-as)))
                   (aoc-art-text (count |x = y| addends)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (make-rnr-helper.dia "Puzzel 1" "Puzzel 2")
  (make-rnr-p1.dia)
  (make-rnr-p2.dia)
  name-map.dia
  read.dia
  predicate.dia
  
  count.dia)
