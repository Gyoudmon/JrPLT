#lang scribble/report

@require{../../literacy.rkt}

@(require "../../discipline/big-bang/lesson/big-bang.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{宇宙大爆炸}

@lesson-desc[
 #:topic ['big-bang 'big-bang]
 #:class 'observer
]

@handbook-lesson{模块化}

@period-desc[
 #:goals big-bang-goals 
 #:datetime ["2023-09-28" "14:45" "16:15"]]

今天是新主题第一节课，开始熟悉游戏程序的基本结构。
游戏程序是比较复杂的一类程序，
因此也是从本节课开始，
学生会逐渐熟悉@tech{模块}化思维。

@tech{模块}化的过程离不开树形图这样的思维工具，
学生自己选择一个以前做过的项目分享给同学，
并把该项目的模块图绘制在学生手册上。
孙功铖的模块图没有大问题，
而且，字写得很好看。

游戏程序会把屏幕当作舞台，
这个舞台可以直接套用真实世界的舞台来设计。
这是本课后半段时间的主要任务。

@handbook-action{课后作业}

将课堂上定义的 Python 类誊写在学生手册第2题右侧的空白处，
并向父母解释清楚。

@handbook-lesson{平面直角坐标系}

@period-desc[
 #:goals big-bang-goals 
 #:datetime ["2023-11-04" "14:45" "16:15"]]

今天的上课氛围很好，如果现状能保持，他们以后可以是个很不错的团队。

本主题开始绘制基本图形，就要涉及图形库的安装问题，
四位同学有三位的电脑出了不同程度的问题，
导致没有学到太多东西。

@handbook-action{课后作业}
可以试试能不能看懂 C++ 代码,
在学生手册的附录部分，
找到 C++ 代码中的 load 和 reflow 两个函数。

孙功铖可以准备换 C++ 了。

@handbook-lesson{锚点和几何变换}

@period-desc[
 #:goals big-bang-goals 
 #:datetime ["2023-11-11" "14:45" "16:15"]]

今天把这个主题的理论部分都讲完了，下节课开始写代码。

今天确认了，孙功铖的电脑用 C++ 做游戏没问题，
也就不用额外解决 Pygame 的安装问题了。

今天的课跟几何关系密切，
班里两个六年级学生都很兴奋。

@handbook-lesson{空白作品}

@period-desc[
 #:goals big-bang-goals 
 #:datetime ["2023-11-25" "14:45" "16:15"]]

这节课开始阅读课程代码，
练习将课程软件中的教师作品替换成自己版本的操作步骤。

这也是孙功铖第一次看 C++ 版的课程软件源码，
因此对进度的要求会比同班其他同学慢一些，
只需能显示一个空作品即可。
实际效果，其他同学的进度也并没有快多少。


@handbook-lesson{头文件和模块文件}

@period-desc[
 #:goals big-bang-goals 
 #:datetime ["2023-12-02" "14:45" "16:15"]]

这节课做的事不多，就是在输入代码。
C++ 的代码量比 Python 多了不少，
孙功铖一节课主要都花在输入上了，
到下课时能看到基本图形都加载到了屏幕上。

其他同学完成输入后还有时间探索参数对图形的影响，
这个可操作且可视化的点让大家都很兴奋。

@handbook-action{课后作业}
请继续输入完成 shape.cpp 的 reflow 函数。
之后也可以花少量时间修改代码中的参数看看运行效果。

@handbook-reference[]
