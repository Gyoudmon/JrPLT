#lang scribble/manual

@require{literacy.rkt}
@require{diagram/architecture.rkt}

@(require geofun/vector)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{系统设计}
@handbook-word-count[]

由需求分析可知，
本文提供的技术解决方案是一个类似 Scratch 的 2D 游戏运行时系统，
以及建立在该引擎之上的一些列项目制课程。

@handbook-scenario{引擎内核总体设计}
@handbook-word-count[]

@fig-ref{arch.dia}展示了本引擎的核心架构。
跟绝大多数软件系统一样，
本教学引擎的核心系统也具备明显的层次结构。

@tamer-figure![
 'arch.dia
 @list{引擎内核架构图}]{
 @(geo-scale architecture.dia 0.32)
}

@handbook-scenario{项目制课程总体设计}
@handbook-word-count[]

@handbook-scenario{网络通信协议}
@handbook-word-count[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
