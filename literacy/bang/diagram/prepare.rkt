#lang typed/racket/base

(provide (all-defined-out))

(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! prepare.dia [#:start-name "第一阶段主题课" #:background 'White] #:-
  (move-down 1 '|自备个人电脑!|)
  (move-down 1 'λ|认识 Shell|)
  (move-down 1 '#:-=)
  (move-right 1 #false "写作视角")
  (move-down 0.75 'λ|踢猫游戏|)
  (move-down 1 '--|邹忌讽齐王纳谏|--)
  (move-down 0.75)
  (move-left 1 '=-)
  (move-down 1 (string->symbol "&宇宙\n大爆炸."))

  (jump-back)
  (move-left 1 #false "数学视角")
  (move-down 0.75 (string->symbol "λ数列\n（递推公式/通项公式）"))
  (move-down 1 'λ|胡克定律探究实验|)
  (L-step '=-))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  prepare.dia)
