#lang scribble/manual

@require{literacy.rkt}
@require{diagram/architecture.rkt}

@(require geofun/vector)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{系统设计}
@handbook-word-count[]

由需求分析可知，
本文提供的技术解决方案是一个类似 Scratch 的 2D 游戏运行时系统，
以及与该引擎配套的适用于青少年教学实践的外围工具集。

@handbook-scenario{引擎运行时系统总体设计}
@handbook-word-count[]

@fig-ref{arch.dia}图示了本文技术解决方案的核心架构。
跟绝大多数软件系统一样，
本教学引擎的核心系统也具备明显的层次结构。

@tamer-figure![
 'arch.dia
 @list{游戏运行时系统架构图}]{
 @architecture.dia
}

@handbook-scenario{构建系统总体设计}
@handbook-word-count[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
