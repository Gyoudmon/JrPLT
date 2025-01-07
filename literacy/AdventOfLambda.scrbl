#lang scribble/book

@require{advent/literacy.rkt}
@require{bibentry.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:document-options '(openany oneside)
 #:hide-version? #true
 #:subtitle "首席历史学家的行踪"]{函数降临节}

@$tex:newcounter:algorithm[]

@texbook-frontmatter[]

@handbook-preface-section{代码降临节}

代码降临节（Advent of Code）是始于2015年的年度编程挑战活动，
也是圣诞日历文化在科技圈的延伸。活动每年12月1日开始，由一个或一系列虚构的故事引出，
参与者每天用代码解两个谜题，一直到圣诞节当天结束，共计25天50个谜题。

此挑战活动不要求参与者有计算机科班背景，对语言、软件和硬件配置也没有要求。
懂点编程基础知识、有一定的问题求解技巧即可开始。总之，青少练手、大学课程项目、
面试热身、企业培训，等等。各种级别的参与者都能从中找到价值。

在本作中，解谜的范式和实现语言主要是@tech[#:key "函数式编程"]{函数式思维}和
@:type{Typed} @hyperlink["https://racket-lang.org/"]{Racket}。
这里的函数更接近数学函数，而非一般程序语言里的函数；
解谜思路也更像代数中的变量替换和函数复合，而非指令的下达和执行。
如果你没有学过编程，那再好不过，直接从贴近人类思维的数学视角进入；
如果你只上过常规编程课，请暂时忘记机器视角、做好思维提升的准备。
挑战过程可能会出现函数式不擅长的问题，
届时我们也不会犹豫使用其他思路来求解。

@handbook-preface-section{首席历史学家的行踪}

@aoc-story[@list{缘起}]{
 往年，@aoc-emph{首席历史学家}总会出席大型圣诞雪橇发射活动，
 但今年已有好几个月没见到他的身影了。
 最近的传闻说他在北极访问较为重要的历史地点，
 一小队资深精灵历史学家邀请你陪同他们共同确认他们认为他最可能去的地方。
 
 每确认一个地点，精灵们就会标记一颗星。
 他们推算出了50个地址，认为首席历史学家@aoc-emph{必定}会出现在其中的一处。
 因此，为拯救今年的圣诞节，
 请你在圣诞老人出发之前帮助他们收集到50颗星。
 收集星星的方式是解谜，
 圣诞日历每天都会解锁两个谜题，
 @aoc-question{一个谜题价值一颗星}。
 
 祝你好运！}

@handbook-smart-table[]

@texbook-mainmatter[]

@include-section[(submod "advent/iSoH/01.historian.hysteria.scrbl" doc)]

@texbook-appendix{附录}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:numbered? #false #:index-section? #true bibentries]
