#lang scribble/report

@require{../../literacy.rkt}

@(require "../../discipline/big-bang/lessons.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{函数与对象}

@lesson-desc[
 #:topic ['big-bang 'function-object]
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

下节课开始敲代码。

@handbook-lesson{定义角色类}

@period-desc[
 #:goals (vector-ref function-object-goals.λ 0)
 #:date "2023-10-15 08:45-10:15"
]

本节课主要是以比较专业的方式复习和总结了上节课的内容，
再把纸笔设计的角色翻译成 C++ 代码。

丁嘉琪的问题主要有两个：
1. 这周电脑是真的没有硬盘空间可用了，上课嘉琪用的是我的电脑，
因此本周也没法在课后复习和练习了。
2. 敲代码的速度有待提升，不过这是所有初学者都有的问题，
我对所有学生也都会统一提要求课后练习。
因为上课时间有限，不应该浪费在打字上。

以上两个问题，需要你们家庭内部沟通好。

@handbook-reference[]
