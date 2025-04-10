#lang scribble/manual

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{系统详细设计和实现}
@handbook-word-count[]

JrPLT 即是本文所提技术方案的核心系统。
采用 Standard C++ 2017 编写，
兼容 Windows、macOS、Linux 三大操作系统。
除标准模版库(STL)、底层图形库和系统接口层外，
所有代码自含。

因引擎内核的实现语言是 Standard C++ 2017，
其基本数据类型仍缺少常用、易用的操作函数，
或者函数命名过于古怪，不方便学生“阅读”。

@handbook-scenario{数学对象}
@handbook-word-count[]

@handbook-action[#:tag "matrix"]{Matrix}
@handbook-word-count[]

@handbook-scenario{通信系统}
@handbook-word-count[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
