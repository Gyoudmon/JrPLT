#lang scribble/acmart @natbib @nonacm @screen @timestamp @acmthm

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[#:author @author{WarGrey Gyoudmon Ju}]{面向青少年程序设计的STEM游戏引擎的设计与开发}
@authorsaddresses{}

@abstract{There should be the abstract of this paper.}

@; https://dl.acm.org/ccs
@; xelatex doesn't like tfm fonts which are used by `ccsdesc`, so ...
@;ccsdesc[#:number 500]{Social and professional topics~K-12 education}
@;ccsdesc[#:number 100]{Software and its engineering~Interactive games}
@;ccsdesc[#:number 100]{Human-centered computing~Graphical user interfaces}

@keywords{编程教育, 计算思维, 跨学科学习, 游戏引擎}

@handbook-smart-table[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@acks{
我从一名软件工程师转变成一名科学老师的契机很偶然。
那天在路口撞到了一条横穿马路的狗，
为开事故证明去了一条很少走的路，
等红灯时发现了本市唯一的一家STEM教育公司。
于是，我内心深处的某种东西被激活了。

少有人走的路必定孤独。
在信念方面，我首先要感谢的人是我自己，
即便与这份事业直接相关的其他人都不真正理解我在干嘛，
我仍然有一颗能沉下来把这件事做好的心。
因为，在中学阶段我就发现自己是那个只能淋雨的人。
其次，是感谢我活跃着的开源圈子： Racket 语言社区。
在那里我接触到了很多关于程序语言理论、编程教育方面专业又前卫，然而依然谦逊的资料和观点。
此外，教育圈同样在践行着理想但靠谱的教育理念的朋友和陌生人也时常会在闲聊时让我感觉不再孤单，
这是很珍贵的激励因素。

心不变，但是需要一些技巧。
在沟通方面，我与客户家长之间似乎隔着一道天堑。
而打破这道壁垒的，是同事的耐心和支持。

初学者的入门困境大同小异，
但是青少年往往需要不同的解决方案。
在教学实施方面，学生显然是最重要的人。
他们让我学会了从孩子的角度思考问题、
理论和技术应该朝哪个方向发力；
他们的收获、困惑，
以及其他情绪亦会以最直白的方式呈现。

最后，特别感谢一个特殊的学生。
她是我的科学课代表，也是最早的一批学生里最跟我合拍的一位。
聪明、懂事，帮我解决了不少课堂问题，
且颜值超高，就是没有时间跟我学真正适应这个时代的东西。
非主流的教育实践，家长和学生因不理解而退课并不稀奇，
自她之后，再失去谁都不会让我失落了。}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:index-section? #false #:numbered? #false
 (book-bib-entry #:date "2020"
                 "DTC" "Don't teach coding: until you read this book"
                 (list "Lindsey D. Handley" "Stephen R. Foster")
                 "Jossey Bass")
 (book-bib-entry #:date "2017" #:url "https://doi.org/10.7551/mitpress/10655.001.0001"
                 "CL" "Coding Literacy: How Computer Programming Is Changing Writing"
                 "Annette Vee" "The MIT Press")
 (book-bib-entry #:date "1997"
                 "CnC" "Computability and Complexity: From a Programming Perspective"
                 "Neil D. Jones" "The MIT Press")
 (book-bib-entry #:date "2013"
                 "RoR" "Realm of Racket: Learn to Program, One Game at a Time!"
                 (list "Forrest Bice" "Rose DeMaio" "Spencer Florence" "Feng-Yun Mimi Lin" "Scott Lindeman"
                       "Nicole Nussbaum" "Eric Peterson" "Ryan Plessner" "David Van Horn" "Matthias Felleisen"
                       "Conrad Barski, MD")
                 "No Starch Press")
 (book-bib-entry #:date "2011"
                 "LoL" "Land of Lisp: Learn to Program in Lisp, One Game at a Time!"
                 "Conrad Barski, MD" "No Starch Press")
 (book-bib-entry #:date "2012" #:edition "2nd"
                 "PL" "Programming Language: Application and Interpretation"
                 "Shriram Krishnamurthi" #false)
 (book-bib-entry #:date "2012" #:url "https://natureofcode.com/book/"
                 "NoC" "The Nature of Code"
                 "Daniel Shiffman" #false)
 (book-bib-entry #:date "2017" #:edition "3rd" #:url "https://doi.org/10.1016/B978-0-12-800645-0.50026-9"
                 "PBR" "Physically Based Rendering: From Theory to Impplementation"
                 (list "Matt Pharr" "Wenzel Jakob" "Greg Humphreys")
                 "The MIT Press")
 (book-bib-entry #:date "2019" #:edition "3rd"
                 "GEA" "Game Engine Architecture"
                 "Jason Gregory" "CRC Press")
 (book-bib-entry #:date "2009"
                 "TCC" "冒号课堂：编程范式与OOP思想"
                 "郑辉" "电子工业出版社")
 (book-bib-entry #:date "2022"
                 "6kids" "少儿计算思维养成记：六个孩子的编程学习笔记"
                 (list "包若宁" "卜文远" "傅鼎荃" "魏文珊" "张秦汉" "卜东波") "机械工业出版社")
 (book-bib-entry #:date "2015" #:edition "8th"
                 "SE:APA" "Software Engineering: A Practitioner's Approach"
                 (list "Roger S. Pressman" "Bruce R. Maxim")
                 "机械工业出版社")
 (book-bib-entry #:date "2018"
                 "CJLH" "The Combridge Handbook of Japanese Linguistics"
                 (editor "Yoko Hasegawa")
                 "Cambridge University Press")]
