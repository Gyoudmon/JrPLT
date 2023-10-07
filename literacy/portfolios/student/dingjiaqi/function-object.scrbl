#lang scribble/report

@require{../../literacy.rkt}

@(require "../../discipline/big-bang/lessons.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{函数与对象}

@lesson-desc[
 #:topic ['big-bang 'shell]
 #:class 'lambda-girl
]

@handbook-lesson{初识类与对象}

@period-desc[
 #:goals (vector-ref function-object-goals.λ 0) 
 #:date "2023-10-06 08:45-10:15"
]

@tech{变量}的@tech{类型}和@tech{值}是难点知识。
本班学生会通过认识 Racket 语言来理解这个难点。
因此，本节课的前半段部分是先更新课程源码，
然后熟悉 DrRacket 软件来运行 racket 程序。

本节课的后半段部分是重新思考“踢猫效应”。
这次学生是导演，要把这个现象编制成话剧表演给大家看，
这个过程涉及设计角色、安排演员、表演等环节。
这些环节恰好对应着“基于对象”的软件设计思路，
因此，我们就自然而然地引入了@tech{类}与@tech{对象}的概念。

@handbook-reference[]
