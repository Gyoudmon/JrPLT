#lang racket

(provide (all-defined-out))

(require scribble/manual)

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

(define function-object-goals.py
  (vector
   ; lesson 1
   (list "安装、配置 Python 开发环境"
         "复习常用 PowerShell 命令，新学 where.exe")

   ; lesson 2
   (list "认识 Python 函数的定义、实现和调用"
         "利用内存教具模拟函数的调用过程")))
