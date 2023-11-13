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
                        "复习“踢猫效应”，理清楚函数的定义、实现和调用"))

          20231022
          (hasheq 0845
                  (list "理解 mac 系统下的当前路径和绝对路径"
                        "理清楚 mac 的主机名、登陆用户名和超级用户 root"
                        "复习常用 shell 命令(cd、ls)"
                        "练习使用 mkdir、rmdir，并思考与“函数”的共通之处")

                  1330
                  (list "初次了解盘符和文件系统"
                        "了解 Shell 是一种为专业人士准备的操作计算机的方式"
                        "练习使用 pwd、cd、ls、mkdir、rmdir、type 等常用 shell 命令"
                        (list "配置环境变量 " (litchar "Path"))
                        "使用 git 命令从教师机下载课程源码"))

          20231112
          (hasheq 1015
                  (list "安装 Python 开发软件"
                        "初次了解盘符和文件系统"
                        "练习使用 cd、ls、mkdir、rmdir 命令"))))
