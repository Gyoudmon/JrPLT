#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require geofun/vector)
(require diafun/flowchart)
(require diafun/flowlet)

(require "../aoc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-hh-helper.dia : (->* () (Any Any) Geo)
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

(define make-hh-p1.dia : (->* () (Any) Geo)
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

(define make-hh-p2.dia : (->* () (Any) Geo)
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
(define IPO.dia
  ((inst dia-flowlet-function String) #:downward? #false #:output-desc "Output"
                                      'Process "Input"))

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

(define hh-mutate.dia
  (dia-flowlet-assignment #:read-desc "读取" #:write-desc "写入"
                          'x '|+ 1| '(span ("x + " (span ([style . normal]) ("1"))))))

(define values.dia
  (let ([sep (geo-hline 240.0 8.0 #:stroke 'LightGrey)])
    (geo-vl-append ((inst dia-flowlet-function Any Any) values pi #:input-desc "π" #:output-desc "π")
                   sep
                   ((inst dia-flowlet-function Any Any) values null #:input-desc "null" #:output-desc "null")
                   sep
                   ((inst dia-flowlet-function Any Any) values '#:key))))

(define sort.dia
  (dia-flowlet-function #:input-desc  (λ [[v : Any]] `(small () (,(format "~a" v))))
                        #:output-desc (λ [[v : Any]] `(small () (,(format "~a" v))))
                        (procedure-rename (λ [[xs : (Listof Natural)]] : (Listof Natural)
                                            (sort xs <))
                                          'sort)
                        (list 3 9 3 5 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (make-hh-helper.dia "Puzzel 1" "Puzzel 2")
  (make-hh-p1.dia)
  (make-hh-p2.dia)
  IPO.dia
  read.dia
  (dia-flowlet-read #:peek-size 5 #:downward? #true
                    (open-input-bytes #"3 4\n4 3\n2 5\n1 3\n3 9\n3 3" 'locin) 4)
  predicate.dia
  values.dia
  sort.dia
  hh-mutate.dia)
