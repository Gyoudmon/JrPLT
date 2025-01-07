#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! hh-sp1.dia [#:start-name "读取地址编号列表(未知数版)" #:background 'White] #:-
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  hh-sp1.dia)
