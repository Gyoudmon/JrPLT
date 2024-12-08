#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! swap/nl.dia [#:start-name "Swap" #:background 'White] #:-
  (move-down 1 '|a瓶倒入c瓶|)
  (move-down 1 '|b瓶倒入a瓶|)
  (move-down 1 '|c瓶倒入b瓶|)
  (move-down 1 'Done$))

(define-flowchart! swap/arrow.dia [#:start-name "Swap" #:background 'White] #:-
  (move-down 1 '|a → c|)
  (move-down 1 '|b → a|)
  (move-down 1 '|c → b|)
  (move-down 1 'Done$))

(define-flowchart! swap/code.dia [#:start-name "Swap" #:background 'White] #:-
  (move-down 1 '|c = a|)
  (move-down 1 '|a = b|)
  (move-down 1 '|b = c|)
  (move-down 1 'Done$))

(define-flowchart! swap.dia [#:start-name "Swap" #:background 'White] #:-
  (move-right 1.5 '|define a, b, c|!)
  (move-right 1.5 '>>|read a, b|)
  (move-right 1.5 '|c = a|)
  (move-right 1.5 '|a = b|)
  (move-right 1.5 '|b = c|)
  (move-right 1.5 'Done$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  swap.dia
  swap/nl.dia
  swap/arrow.dia
  swap/code.dia)
