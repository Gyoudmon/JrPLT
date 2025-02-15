#lang racket

; 场景：胡克定律

; 实验得出关系表达式：Δl = 2m

; 定义函数
(define (Δl m)
  (2 . * . m))

(Δl 1)

; 调用函数
(Δl 1)
(Δl 2)

; 古灵精怪：
; (Δl "猫")

; 发现问题，引出“类型”概念，
(module safe typed/racket
  (define (Δl [m : Number])
    (2 . * . m))

  #;(displayln (Δl "猫")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 扩展，要在弹性限度之内，并且直接给出弹簧最终长度
; 定义函数：
(define (l m 原始长度 限度)
  (if (<= m 限度)
      ((2 . * . m) . + . 原始长度)
      (printf "啊啊啊啊啊啊，弹簧坏掉啦！")))

; 调用函数：
(l 1 0.5 5)
(l 6 0.5 5)
