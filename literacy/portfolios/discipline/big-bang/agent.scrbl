#lang scribble/base

@require{../../literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@handbook-portfolio-story{智能体与环境}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered

 @item{了解智能体与环境}

 @item{了解游戏主循环和事件系统}
 
 @item{认识@tech{输入-处理-输出}模型}
 
 @item{理解布尔类型和分支结构}]

@handbook-scenario{知识点}

@handbook-action{重点知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Boolean Type" #:abbr "bool"]{布尔类型}是一种
  只有@litchar{true}和@litchar{false}两种值的数据@tech{类型}。}

]
 
@handbook-action{难点知识}

@itemlist[
 #:style 'ordered

 @item{一切能够感知环境并做出一定反应从而改变环境的东西统称为@handbook-defterm[#:origin "Agent"]{智能体}。
  其典型代表是生物和人工智能。}

 @item{@tech{智能体}的@handbook-defterm[#:origin "Behavior"]{行为}包括感知到的环境变化和响应动作。
  在程序中，@tech{行为}可以实现为@tech{类}的@tech{方法}。}

 @item{@tech{智能体}@tech{行为}的好坏可以用一系列指标来评估，
  称为@handbook-defterm[#:origin "Performance Measurement"]{性能度量}。}

 ]

@handbook-action{扩展知识}

@itemlist[
 #:style 'ordered

 @item{计算机做事遵循@handbook-defterm[#:origin "Input-Process-Output" #:abbr "IPO"]{输入-处理-输出}模式。即，
  接受一定的@emph{输入}数据、
  基于此数据执行一系列@emph{操作}、
  很可能还会@emph{返回}一些数据作为@emph{输出结果}。

  这是一种广泛使用的描述结构化信息和过程的模型。}

 ]

@handbook-reference[]
