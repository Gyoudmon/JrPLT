#lang racket

(provide (all-defined-out))

(require scribble/manual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-goals
  (hasheq 20230917
          (hasheq 0845
                  (list "安装开发软件"
                        "初次了解盘符和文件系统"
                        "练习使用 cd、ls、mkdir 命令"))

          20230924
          (hasheq 0845
                  (list "理解局域网"
                        "使用 git 命令从教师机下载课程源码"))

          20230929
          (hasheq 0845
                  (list "理解当前路径、绝对路径、相对路径"
                        (list "了解文件路径中的特殊路径" (litchar ".") "和" (litchar ".."))
                        "复习“踢猫效应”，理清楚函数的定义、实现和调用"))))
