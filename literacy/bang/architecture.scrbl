#lang scribble/manual

@require{literacy.rkt}
@require{diagram/architecture.rkt}

@(require geofun/vector)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{系统概要设计}
@handbook-word-count[]

JrPLT 即是本文所提技术方案的核心系统。
采用 Standard C++ 2017 编写，
兼容 Windows、macOS、Linux 三大操作系统。
除标准模版库(STL)、底层图形库和系统接口层外，
所有代码自含。

@handbook-scenario{引擎内核总体设计}
@handbook-word-count[]

@tamer-figure![
 'arch.dia
 @list{引擎内核结构模块图}]{
 @(geo-scale architecture.dia 0.32)
}

@fig-ref{arch.dia}展示的是本引擎核心部分的结构模块图。
下面简要介绍一下各模块的职能，
重要、有趣的部分留到后文再详细说明。

@handbook-itemlist[
 #:style 'compact

 @item{Datum: 这是所有其他模块的基础，提供引擎所需的基础数据类型及其操作函数。
  包括字符串、定点数、浮点数、高精度自然数、数组、枚举，等等。
  因为引擎内核的实现语言是 Standard C++ 2017，
  其基本数据类型仍缺少常用、易用的操作函数，
  或者函数命名过于古怪，不方便学生记忆。}

 @item{Graphics: 基本绘图模块，提供对 SDL2 C 接口的简单 C++ 封装。}
 @item{Physics: 物理对象模块，提供对基本的数学和物理概念的封装，
  包括矢量、矩阵、色彩空间、随机数，等等。}

 @item{User Interface: 用户接口模块，也即用户能直接感受到的部分。}
 
 @item{Virtualization: 虚拟化模块，也即用户不能直接感受到、但悄悄支撑引擎运行的部分。}
 ]

@handbook-scenario{项目制课程总体设计}
@handbook-word-count[]

@handbook-scenario{网络通信协议}
@handbook-word-count[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
