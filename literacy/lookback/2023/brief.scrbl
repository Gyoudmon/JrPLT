#lang scribble/base

@require{../../disciplines/literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{个人工作概况}

@handbook-scenario{教育研究}

本年度我的阅读资料主要集中在编程教育和数学教育方面，科学教育次之；
经常围观线上教育类讲座，以及与我自己圈子里的教育界朋友讨论交流。

@itemlist[
 #:style 'compact

 @item{理论方面，对以上几个教育领域的研究现状大致刷到最新了}
 @item{实践方面，还需要更多的学生样本来提高教学能力}
 @item{理念方面，我有明确想解决的教育痛点，但是困难重重}
 ]

@handbook-scenario{编程教育}

@handbook-action{C++/Python}

我的编程教育的研课思路不与市场上同类课程雷同，
但符合现代教育理论和我个人的专业积累。
经过半年教学实施，确实靠谱，且仍有优化提升的空间。

@itemlist[
 #:style 'compact
 
 @item{制定了编程课教学大纲、学生手册、教学实施的研课模版。
  为培养学生的自学能力，与科学课文档不同的是，
  编程课文档的以上几个部分都整合到了学生手册里。}
 
 @item{@tamer-figure-ref{b:bigbang}即为本年度重点研发和持续改进的
  编程课程“语言基础课”的主界面，兼顾训练基本概念和计算思维，
  强调学科交叉。
  
  @tamer-figure["b:bigbang" "语言基础课 主界面"]{
   @stone-image["2023/big-bang.png" #:scale 0.32]}
  
  图中的每一个金币都代表一个主题，按照训练的侧重点依次划分为三个阶段。
  结合实际上课情况预计，每个主题都能至少上4-6次课，
  因此这套课程对每一个初学者都至少可以学习一年半。}
 
 @item{@tamer-figure-ref{b:history}是与本课程相关的源码的历史曲线，
  截止到2023年7月底，累计代码3万行左右。
  
  @tamer-figure*["b:history" "语言基础课 历史曲线"]{
   @stone-image["2023/history.png"]}
  
  @itemlist[
 #:style 'compact

 @item{课程作品源码是学生需要自己完成的代码，约占全部代码的25%。
    就本课程而言到6月底就不再有大的变动了，因此本曲线图也仅截至到6月底。}
  
 @item{游戏引擎源码还在持续更新，代码量相较6月底有可观的增量。
    它是我所有课程作品源码和其他辅助系统的基础。
    简单来说，这个引擎可以看作是 C++ 版的 Scratch，
    但是只能纯写代码控制游戏角色、没有可拖动的积木。}]}
 ]

@handbook-action{Scratch}

Scratch 大部分课程的主题和作品源自同事，
少部分作品是我为吸引孩子注意力而定制。

我在教授 Scratch 的过程中也会有意识的培养学生一些对专业编程更有帮助的能力。
经过半年的实践，这些思路确实靠谱，除个别外学生也都很配合。

@handbook-scenario{科学教育}

科学教育不是我今年的工作重点，参与不多。

@itemlist[
 #:style 'compact
 
 @item{参与制定了科学大纲和学生手册的研课模版。}
 
 @item{负责培文2023-2024年度三年级和五年级共三个班的义务教育授课、大作品。}
 
 @item{研发探究性科学课两个大主题（黏菌 和 洋流）。}]

@handbook-scenario{其他}

教学之外，还顺便基于我的专业特长制定了一些适合我的工作流。
       
@itemlist[
 #:style 'compact
 
 @item{编写学生档案管理系统。
  按最初设想，这是个我与家长和学生沟通的核心界面，
  实际工作过程中碰到诸多其他问题，
  目前仅用于跟我理念相似的家长和学生。}
 
 @item{编写课程归档系统，
  方便给课程打上专业标签，剔除重复课件，提取关键词等基本功能。}
 
 @item{编写义务教育成绩管理系统。
  方便我记录学生成绩、分析学情，以及记忆学生姓名。}
 
 @item{编写抽奖摇奖机模拟程序。
  此程序可同时作为优秀学生的 C++ 课程项目。}
                          
 ]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
