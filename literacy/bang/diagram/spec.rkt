#lang typed/racket/base

(provide (all-defined-out))

(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! spec.dia
  #:parameterize ([default-diaflow-arrow-target-shape #false]
                  [default-diaflow-free-track-dash 'solid])
  #:start 'specmon [#:background 'White] #:-
  (move-down 0.75 '.spec)
  (move-left 1.50)
  (move-down 0.75 'DSL)
  
  (jump-back '.spec)
  (move-right 1.5)
  (move-down 0.75 'Prover)
  
  (jump-back '.spec)
  (move-down 0.75 'Behavior)
  (move-down 0.75 '.bdd)
  (move-right 0.75)
  (move-down 0.75 'Issue)
  (move-down 1.00 'Formatter)
  
  (jump-back '.bdd)
  (move-left 0.75)
  (move-down 0.75 'Expectation))

(define-flowchart! bdd.dia
  #:parameterize ([default-diaflow-arrow-label-rotate? #true]
                  [default-diaflow-arrow-label-inline? #false]
                  [default-diaflow-free-track-dash 'long-dash]
                  [default-diaflow-free-track-target-shape default-arrow-tip]
                  [default-diaflow-node-label-string #hasheq((λtestable-unit . "编写最小可测单元")
                                                             (test-driver . "编写\n测试驱动函数或包装器")
                                                             (ffi . "编写\n驱动函数的 FFI 绑定")
                                                             (λspec . "编写 Behaviors")
                                                             (prove . "运行 Prover"))])
  #:start '.home [#:background 'White] #:-
  (jump-down 1 'λtestable-unit)
  (move-down 1 'test-driver)
  (move-down 1 'ffi)
  (move-down 1 'λspec)
  (move-down 1 'prove)
  (move-down 0.75)
  (move-left 1)
  (L-step 'λtestable-unit "迭代")

  (move-right 1)
  (L-step 'λspec "回归测试")

  (jump-to 1+2i)
  (move-left 0.38))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  spec.dia
  bdd.dia)
