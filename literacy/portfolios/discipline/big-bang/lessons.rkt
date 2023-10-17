#lang racket

(provide (all-defined-out))

(require scribble/manual)

#|
In order to reduce the amount of fragmented files,
all lessons are collected in this file.

In the source of student's portfolio,
this file should be referenced with the form of `@(require ...)`
so that the building system won't depends on this file
to rebuild certain portfolios.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-goals
  (vector
   ; lesson 1
   (list "安装开发软件"
         "初次了解盘符和文件系统"
         "练习使用 cd、ls、mkdir 命令")

   ; lesson 2
   (list "理解局域网"
         "使用 git 命令从教师机下载课程源码")

   ; lesson 3
   (list "理解当前路径、绝对路径、相对路径"
         (list "了解文件路径中的特殊路径" (litchar ".") "和" (litchar ".."))
         "复习“踢猫效应”，理清楚函数的定义、实现和调用")))

(define function-object-goals.online
  (vector
   ; lesson 1
   (list "了解计算机语言、计算机程序等基本概念"
         "了解 Shell 是一种为专业人士准备的操作计算机的方式"
         "实际使用 cd, ls, mkdir 等命令建立个人工作目录"
         "借用“踢猫效应”现象理解 Python 函数的定义和调用")))

(define function-object-goals.py
  (vector
   ; lesson 1
   (list "安装、配置 Python 开发环境"
         "复习常用 PowerShell 命令，新学 where.exe")

   ; lesson 2
   (list "认识 Python 函数的定义、实现和调用"
         "利用内存教具模拟函数的调用过程")

   ; lesson 3
   (list "导演舞台剧“踢猫效应”，设计角色的属性和动作"
         "抽象设计的舞台剧角色"
         "将抽象角色翻译成 Python 类代码")))

(define function-object-goals.λ
  (vector
   ; lesson 1
   (list "初次了解 Racket 语言，借此体验“类型系统”如何确保代码正确"
         "再谈“踢猫效应”，设计舞台剧角色，引入“类”和“对象”")
   
   ; lesson 2
   (list "设计“踢猫效应”中角色的属性和动作"
         "抽象设计的舞台剧角色"
         "将抽象角色翻译成 C++ 类代码")))

(define function-object-goals.λj
  (vector
   ; lesson 1
   (list "安装 C++ 开发工具"
         "用中文描述“踢猫效应”现象并提取关键字"
         "将“踢猫效应”翻译成 Python 代码，理解函数的定义、实现和调用"
         "导演“踢猫效应”舞台剧，设计角色的属性和动作，引入“类”和“对象”"
         "将角色翻译成 Python 类")))
