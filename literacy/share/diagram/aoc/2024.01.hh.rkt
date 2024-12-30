#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! hh-sp1.dia [#:start-name "求解总差距(代数版)" #:background 'White] #:-
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
  (move-down 1 '|sort x|)
  (move-down 1 '|sort y|)
  (move-down 1 'λsum)
  (move-down 1 '<<print)
  (move-down 1 'Done$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  hh-sp1.dia)
