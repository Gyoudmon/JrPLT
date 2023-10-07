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

更新源码，吴玚有经验，
我安排她去教会同学使用 git 命令来做这件事。

本节课的后半段部分是重新思考“踢猫效应”。
这次学生是导演，要把这个现象编制成话剧表演给大家看，
这个过程涉及设计角色、安排演员、表演等环节。
这些环节恰好对应着“基于对象”的软件设计思路，
因此，我们就自然而然地引入了@tech{类}与@tech{对象}的概念。

@handbook-action{课后作业}

吴玚在使用命令行操作文件夹时一直有个坎迈不过来：
项目目录@tech{路径}(@litchar{D:\wuchang\basis})不能一次输入正确了。
因此，

@itemlist[
 #:style 'order
 
 @item{请回顾@tech{盘符}和@tech{绝对路径}在 Windows 下的关系(@secref{$shell:path})，
  并上机操作克服此问题。}

 @item{【选做】请用@exec{mkdir}命令确认是否可以创建一个长得像@tech{盘符}的文件夹。
  即，冒号(@litchar{:})是否可以出现在文件名里。
  并解释为什么 @litchar{\D:\wuchange} 不是一个靠谱的@tech{路径}。}
]

@handbook-reference[]
