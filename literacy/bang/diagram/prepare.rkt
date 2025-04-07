#lang typed/racket/base

(provide (all-defined-out))

(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! prepare.dia
  #:parameterize ([default-diaflow-edge-label-rotate? #true]
                  [default-diaflow-edge-label-inline? #false])
  [#:start-name "第一阶段主题课" #:background 'White] #:-
  (move-down 1 '|自备个人电脑!|)
  (move-down 1 'λ|认识 Shell|)
  (move-down 1 '#:-=)
  
  (move-left 0.75 #false "数学视角")
  (move-down 0.75 (string->keyword "λ数列\n（递推公式/通项公式）"))
  (move-left 1.0)
  (move-down 1 '--|同音替换密码|--)
  (move-down 1)
  (move-right 1 'λ|胡克定律探究实验|)
  (move-down 0.75)
  (move-right '#:-= '=-)
  (move-down 1 (string->symbol "&PBL."))

  (jump-back)
  (move-down 'λ|胡克定律探究实验|)

  (jump-back)
  (move-right 0.75 #false "写作视角")
  (move-down 0.75 '#:λ|踢猫游戏|)
  (L-step '=-)

  (jump-back)
  (move-right 1.0)
  (move-down 'λ|胡克定律探究实验| '--|邹忌讽齐王纳谏|-- "初识类与对象")
  (L-step '=-))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  prepare.dia)
