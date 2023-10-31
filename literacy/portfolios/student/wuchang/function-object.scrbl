#lang scribble/report

@require{../../literacy.rkt}

@(require "../../discipline/big-bang/lesson/function-object.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{函数与对象}

@lesson-desc[
 #:topic ['big-bang 'function-object]
 #:class 'lambda-girl
]

@handbook-lesson{初识类与对象}

@period-desc[
 #:goals function-object-goals 
 #:datetime ["2023-10-06" "08:45" "10:15"]]

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

@handbook-lesson{定义角色类}

@period-desc[
 #:goals function-object-goals
 #:datetime ["2023-10-15" "08:45" "10:15"]]

本节课主要是以比较专业的方式复习和总结了上节课的内容，
再把纸笔设计的角色翻译成 C++ 代码。

吴玚没有大问题，就是敲代码的速度需要加快，
不过这是所有初学者都有的问题，
我对所有学生也都会统一提要求课后练习。
因为上课时间有限，不应该浪费在打字上。

因此，建议吴玚每隔2-3天就要把最近上课时输入过的代码再输入一遍。
我觉得吴玚本周的状态不如她刚来的时候。

@handbook-lesson{计算思维}

@period-desc[
 #:goals function-object-goals
 #:datetime ["2023-10-30" "09:00" "10:15"]]

鉴于“踢猫效应”这个主题从开始到现在已经经历了个把月，
学生普遍都觉得厌倦了。
因此，本节课是该主题的最后一节课，
下节课开始下一个主题。
在本班，这个问题不大。

今天前半段时间帮学生理清了“内存”和“外存”的概念，
这是为了让学生能更好的理解@tech{变量}。
后半段时间带着学生重新思考“学编程到底是在学什么”？
吴玚此前似乎从来没有接触过与数学关系比较紧密的思维，
不过，她对这个的好奇心还挺旺的，挺好。

最后，本班的上课时间是周日上午 08:45 - 10:15，
请尽量不要迟到。

@handbook-reference[]
