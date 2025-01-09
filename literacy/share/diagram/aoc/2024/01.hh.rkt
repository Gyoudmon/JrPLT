#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-sandglass : DiaFlow-Id->Node-Shape
  (lambda [master id label style width height direction hint]
    (case id
      [(|+1|) (let ([fbox (geo-sandglass #:fill (dia-node-select-fill-paint style)
                                         #:stroke (dia-node-select-stroke-paint style)
                                         #:neck-width (* width 0.16) #:neck-height (* width 0.05)
                                         (* width 0.25))])
                (if (not direction)
                    (create-dia-node #:id id #:fit-ratio 0.95 0.4 #:position 0.5 0.25 fbox label)
                    (create-dia-node #:id id (geo-rotate fbox (- direction (* pi 0.5))) label)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! hh-sp1.dia [#:start-name "Read Location IDs\n(未知数版)" #:background 'White] #:-
  (move-down 1 'initialization!)
  (move-down 1 '>>|read IDs|)
  (move-down 1 '#:predicate?)

  (move-left 1 #false "Yes")
  (move-down 1 '|cons x|)
  (move-down 1 '|cons y|)
  (move-down 1)
  (move-left 1 #false "Loop")
  (L-step '>>|read IDs|)

  (jump-back)
  (move-right 1 #false "No")
  (move-down 2 '<<result)
  (move-down 1 'Done$))

(default-diaflow-storage-block-width  64.0)
(default-diaflow-storage-block-height 50.0)

(define-flowchart! hh-mutate.dia
  [#:start-name "赋值\n(x := x + 1)" #:background 'White #:λnode make-sandglass #:ignore '(home)] #:-
  (jump-down 1 '/proc/x)
  (move-right 0.4 #false "读取")
  (move-right 0.5 '|+1| "x")
  (move-right 0.5 #false "x+1")
  (move-down 0.9)
  (T-step '/proc/x #false "写入"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  hh-sp1.dia
  hh-mutate.dia)
