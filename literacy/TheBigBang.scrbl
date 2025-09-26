#lang scribble/acmart @manuscript @natbib @nonacm @screen @timestamp @acmthm

@require{bang/literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define about-me @affiliation[
 #:country "Earth" #| sigplan requires `country` |#
 #:institution "PLT & STEM Education"])

@(define mk-author
   (lambda names
     (apply author
            #:affiliation about-me
            #:email (email "juzhenliang@gmail.com")
            #:orcid "https://orcid.org/0009-0009-0375-2359"
            names)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:λtitle title #:author mk-author
 #:tex-package "acmhack.tex"
 ]{面向青少年程序设计教学引擎的设计与实现}
@authorsaddresses[]

@include-abstract{bang/abstract_zh.scrbl}

@; https://dl.acm.org/ccs
@ccsdesc[#:number 500]{Social and professional topics~Computational thinking}
@ccsdesc[#:number 500]{Social and professional topics~K-12 education}
@ccsdesc[#:number 300]{Software and its engineering~Interactive games}
@ccsdesc[#:number 100]{Human-centered computing~Graphical user interfaces}

@keywords{编程教育, 跨学科学习, 项目式学习, 游戏引擎}

@handbook-smart-table[]

@include-section{bang/introduction.scrbl}
@include-section{bang/techstack.scrbl}
@include-section{bang/feasibility.scrbl}
@include-section{bang/requirement.scrbl}
@include-section{bang/architecture.scrbl}
@include-section{bang/implementation.scrbl}
@include-section{bang/test.scrbl}
@include-section{bang/future.scrbl}

@texbook-appendix{附录}

@include-section{bang/appendix/lesson.scrbl}
@include-section{bang/appendix/bdd.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@acks{
 我从一名软件工程师转变成一名科学老师的契机很偶然。
 那天在路口撞到了一条横穿马路的狗，
 为开事故证明去了一条很少走的路，
 等红灯时发现了本市唯一的一家STEM教育公司。
 于是，我内心深处的某种东西被激活了。
 
 少有人走的路必定孤独。
 在信念方面，最重要的肯定是我自己，
 即便与这份事业直接相关的其他人都不真正理解我在干嘛，
 我仍然有一颗能沉下来把这件事做好的心。
 因为，在初中阶段我就发现自己是那个只能淋雨的人。
 其次，是我活跃着的开源圈子： Racket 语言社区。
 在那里我接触到了很多关于程序语言理论、
 编程教育方面专业又前卫，
 然而依然谦逊的资料和观点。
 此外，
 教育圈同样在践行着理想但靠谱的教育理念的朋友和陌生人也时常会在闲聊时让我感觉不再孤单，
 这是很珍贵的激励因素。
 
 心不变，但是需要一些技巧。
 在沟通方面，我与客户家长之间似乎隔着一道天堑。
 而打破这道壁垒的，
 是合伙人的耐心和支持。
 
 初学者的入门困境大同小异，
 但是青少年往往需要不同的解决方案。
 在教学实践方面，学生显然是最重要的人。
 他们让我学会了从孩子的角度思考问题、
 理论和技术应该朝哪个方向发力；
 他们的收获、困惑，
 以及其他情绪亦会以最直白的方式呈现。
 
 最后，特别提一下我的义务教育科学课代表，
 她是最早的一批学生里最跟我合拍的一位。
 聪明、自律，帮我解决了不少课堂问题，
 且颜值超高，就是没有时间跟我学我为她定制的课程。
 非主流的教育实践，家长和学生因看不透而退课并不稀奇，
 自她之后，再失去谁我的内心都毫无波澜。
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:numbered? #false #:index-section? #false #:prefab-bibentries? #false]
