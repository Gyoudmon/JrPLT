#lang scribble/report

@require{advent/literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[#:hide-version? #true #:subtitle "魔法能量远征"]{函数降临节}

@handbook-preface-section{代码降临节}

代码降临节（Advent of Code）是始于2015年的年度编程挑战活动，
也是圣诞日历文化在科技圈的延伸。活动每年12月1日开始，由一个或一系列虚构的故事引出，
参与者每天用代码解两个谜题，一直到圣诞节当天结束，共计25天50个谜题。

此挑战活动不要求参与者有计算机科班背景，对语言、软件和硬件配置也没有要求。
懂点编程基础知识、有一定的问题求解技巧即可开始。总之，青少练手、大学课程项目、
面试热身、企业培训，等等。各种级别的参与者都能从中找到价值。

在本文中，解谜的范式和实现语言主要是@tech[#:key "函数式编程"]{函数式思维}和
@hyperlink["https://racket-lang.org/"]{Racket}。
这里的函数更接近数学函数，而非一般程序语言里的函数；
解谜思路也更像代数中的变量替换和函数复合，而非指令的下达和执行。
如果你没有学过编程，那再好不过，直接从贴近人类思维的数学视角进入；
如果你只上过常规编程课，请暂时忘记机器视角、做好思维提升的准备。
挑战过程可能会出现函数式不擅长的问题，
届时我们也不会犹豫使用其他思路来求解。

@handbook-preface-section{魔法能量远征}

圣诞驯鹿的主食与普通驯鹿无异，但它们需要特殊能量来快递圣诞礼物。
因此，驯鹿最爱的零食是一种只会生长在丛林深处的特殊的星形水果，
精灵们每年都会带你远征去寻找这种水果。

为了供应足够的礼物快递能量，远征队在圣诞节之前要采摘到至少50颗星形水果。
尽管精灵会保证把你带到一个长满水果的树丛附近，但以防万一，你仍然可以攫取
任何沿途看到的水果。采集水果的方式是解谜，一天两个谜题，解完第一个谜才会
解锁第二个，每个谜题价值一颗水果。

祝你好运！

@handbook-smart-table[]

@include-section[(submod "advent/mee/01.calorie.counting.scrbl" doc)]
@include-section[(submod "advent/mee/02.rock.paper.scissors.scrbl" doc)]
@;include-section[(submod "advent/mee/03.rucksack.reorganization.scrbl" doc)]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:numbered? #false #:index-section? #true #:prefab-bibentries? #false]
