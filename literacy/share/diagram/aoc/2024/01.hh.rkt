#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/flowchart)

(require "../aoc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-sandglass : DiaFlow-Id->Node-Shape
  (lambda [master id label style width height direction hint]
    (case id
      [(|+1|)
       (let ([fbox (geo-sandglass #:fill (dia-node-select-fill-paint style)
                                  #:stroke (dia-node-select-stroke-paint style)
                                  #:neck-width (* width 0.16) #:neck-height (* width 0.05)
                                  (* width 0.25))])
         (if (or (not direction) (not (zero? direction)))
             (create-dia-node #:id id #:fit-ratio 0.95 0.4 #:position 0.5 0.25 fbox label)
             (create-dia-node #:id id (geo-rotate fbox (- direction (* pi 0.5))) label)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-hh-sp1.dia : (->* () (Any Any) Geo)
  (lambda [[P1 '|Puzzel 1|] [P2 '|Puzzel 2|]]
    (define-values (p1-label p1-anchor) (aoc-extract-flow-connector-info P1))
    (define-values (p2-label p2-anchor) (aoc-extract-flow-connector-info P2))
    
    (define-flowchart! hh-sp1.dia [#:start-name "Read Location IDs" #:background 'White] #:-
      (move-down 1 'initialization!)
      (move-down 1 '>>|read IDs|)
      (jump-right 1.5 '/doc/locin)
      (move-left 1.5 #false "from")
      (move-down 1 '#:predicate?)
      
      (move-left 1 #false "Yes")
      (move-down 1 '|cons x|)
      (move-down 1 '|cons y|)
      (move-down 1)
      (move-left 1 #false "Loop")
      (L-step '>>|read IDs|)
      
      (jump-back)
      (move-right 1 #false "No")
      (move-down 1 '<<result)
      (move-down 1 '#:-=)
      (move-down 0.5)
      (move-left 0.5 #false p1-label)
      (move-down 1 p1-anchor)
      
      (jump-back)
      (move-down 0.5)
      (move-right 0.5 #false p2-label)
      (move-down 1 p2-anchor))

    hh-sp1.dia))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! IPO.dia #:start '/in [#:background 'White #:λnode aoc-dataflow-node-construct] #:-
  (move-right 0.75 'Process "Input")
  (move-right 0.75 '/out "Output"))

(define-flowchart! hh-mutate.dia #:start '/proc/x
  [#:background 'White #:λnode aoc-dataflow-node-construct] #:-
  (move-right 0.8 '|+1| '("读取" . "x"))
  (move-right 0.5 #false '(span ("x + " (span ([style . normal]) ("1")))))
  (move-down 0.9)
  (T-step '/proc/x #false "写入"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read.dia
  (let ([read/1st.dia
         (make-flowchart! #:start '/in [#:background 'White #:λnode aoc-dataflow-node-construct] #:-
                          (move-right 0.75 'read "3  4\n2  5")
                          (move-right 0.75 '/a "a <span size=\"x-large\">=</span> 3"))]
        [read/2nd.dia
         (make-flowchart! #:start '/in [#:background 'White #:λnode aoc-dataflow-node-construct] #:-
                          (move-right 0.75 'read "4\n2  5")
                          (move-right 0.75 '/b "b <span size=\"x-large\">=</span> 4"))])
    (geo-vl-append read/1st.dia read/2nd.dia)))

(define predicate.dia
  (parameterize ([default-diaflow-node-label-string #hasheq((nat1 . "nat?")
                                                            (nat2 . "nat?"))])
    (make-flowchart! #:start '/ain [#:background 'White #:λnode aoc-dataflow-node-construct #:λarrow aoc-dataflow-arrow-identify] #:-
                     (move-right 0.5 'nat1 "a")
                     (move-right 0.7 #false "#true")
                     (move-down 0.5 '=*)
                     (move-right 0.3 '/and)

                     (jump-to +i '/bin)
                     (move-right 0.5 'nat2 "b")
                     (move-right 0.7 #false "#true")
                     (move-up 0.5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (make-hh-sp1.dia "Puzzel 1" "Puzzel 2")
  IPO.dia
  read.dia
  predicate.dia
  hh-mutate.dia)
