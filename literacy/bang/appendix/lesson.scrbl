#lang scribble/manual

@require{../literacy.rkt}
@require{../diagram/feasibility.rkt}
@require{../diagram/hsdecrypt.rkt}

@(require geofun/vector)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix-story[#:tag "stage0-lessons"]{准备阶段教学实施举例}
@handbook-word-count[]

@handbook-scenario{踢猫游戏}

“踢猫游戏”作为“Hello, World!”的直接而高级的替代品，
会用代码完成一项“看图写话”任务。
这可以有效帮助活跃课堂气氛，
因为学生会写上自己熟悉的人的名字踢来踢去。

无论学生是否能够直接入门 C++，
该主题课都会按照“自然语言 -> Python -> C++”的顺序慢慢引出类型概念，
指出 Python 类型特点的硬伤，
引导学生思考“为什么 C++ 代码会比 Python 代码啰嗦很多”。

@tamer-figure!['kick-cat "用代码描述“踢猫效应”"]{
 @(let ([s 0.42])
    (list @(para (stone-image "Bang/KickCat.py.png" #:scale s) @elem{Python 版})
          @(para (stone-image "Bang/KickCat.cpp.png" #:scale s) @elem{C++ 版})))
}

该题材也是唯一使用中文命名的主题课，
顺便在实践中回答“为什么不用中文编程”。

@handbook-scenario{同音替换密码}

从唤起学生兴趣的角度来说，以“替换密码”为代表的古典密码学意外地有效，
特别是可以在小班里形成学生之间的互动。
我的课程中选择“同音替换密码”作为授课内容，
有助于同时关联起数学、语言和编程三个方面。
因其加密过程涉及随机数的选取，不适合用来写 C++ 入门程序，
但很方便学生在理解了数学原理之后用纸笔加密、与同学互换解密。

该题材使得学生在入门早期就能碰到“言之有物”的流程图(或伪代码)，
比之把日常生活中的简单逻辑、或现成的公式翻译为代码，
这可以很好的帮助优秀学生洞悉和表达自己纸笔推演的过程。

@tamer-figure!['hscipher "同音替换密码"]{
 @(let ([s 0.25])
    (list @(para (stone-image "Bang/hsdecrypt.png" #:scale 0.72) @elem{课堂现场照片})
          @(para (geo-scale hsdec.dia s) @elem{解密流程图})
          @(para (stone-image "Bang/hsdecrypt.cpp.png" #:scale 0.32) @elem{解密程序(C++)})))
}

该主题课还可以继续扩展，
初步带学生理解字符和文本概念；
正确区分密码、编码和圈子流行语；等等。

@handbook-scenario{胡克定律探究实验}

“胡克定律探究实验”的定位是师生协作项目，
学生会边实验边记录数据，然后绘制成折线图。
实验过程不可避免会产生误差乃至错误，
导致学生收集到的数据不符合直觉。
教师有机会给学生介绍统计学和人工智能领域中最基本的拟合方法，
把“脏”的实验数据变得仍具实用价值。
此过程可概括呈现为@fig-ref{hookeslaw}。

@tamer-figure!['hookeslaw "胡克定律探究实验"]{
 @(list @(para (stone-image "Bang/HookesLaw.png") @elem{实验中的误差和错误})
        @(para (hookes-law-plot data 0.70) @elem{教师辅助拟合实验数据(最小二乘法)}))
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
