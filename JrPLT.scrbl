#lang scribble/sigplan @nocopyright @onecolumn

@(require "literacy/literacy.rkt")
@(require "literacy/graphviz.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title{初级程序语言理论：对比学习方法}

@abstract{程序设计对于青少年和成年人来说，都是足够新颖的技能，
零基础的这两个群体在初学阶段没有太大的认知差别。
核心问题在于，如何帮助学生(包括成年人和青少年)建立新的思维模式。
这个过程的长短显然因人而异，因此，我的计算机科学课会尝试解决这个问题，
并找到一个平衡点，既要有铺垫时间来给初学者训练基础技能(如英文键盘打字)、
认识基本概念(如变量、语法和语义)、感受编程的乐趣；
又不能铺垫太长导致初学者耐心耗尽被动劝退。
而且，这个问题的解决只能从更高维度入手(比如课程的整体设计和教学过程的实施)，
而不是简单地带学生读懂每一行代码的语法细节。}

@handbook-statistics[#:gitstat-width 450 #:gitstat-radius 80
 #:altcolors '(["Racket" . Green] ["Python" . Khaki])
 git-size-ring-chart git-loc-time-series]

@handbook-smart-table[]

@include-section{literacy/preface.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:index-section? #true #:numbered? #false
 (book-bib-entry #:date "2019" #:edition "3rd"
                 "GEA" "Game Engine Architecture"
                 "Jason Gregory" "CRC Press")
 (book-bib-entry #:date "2013" #:edition "1st"
                 "RoR" "Realm of Racket: Learn to Program, One Game at a Time!"
                 (list "Forrest Bice" "Rose DeMaio" "Spencer Florence" "Feng-Yun Mimi Lin" "Scott Lindeman"
                       "Nicole Nussbaum" "Eric Peterson" "Ryan Plessner" "David Van Horn" "Matthias Felleisen"
                       "Conrad Barski, MD")
                 "No Starch Press")
 (book-bib-entry #:date "2011" #:edition "1st"
                 "LoL" "Land of Lisp: Learn to Program in Lisp, One Game at a Time!"
                 "Conrad Barski, MD" "No Starch Press")
 (book-bib-entry #:date "2012" #:edition "2nd"
                 "PL" "Programming Language: Application and Interpretation"
                 "Shriram Krishnamurthi" #false)
 (book-bib-entry #:date "2012" #:url "https://natureofcode.com/book/"
                 "NoC" "The Nature of Code"
                 "Daniel Shiffman" #false)
 (book-bib-entry #:date "2009"
                 "TCC" "冒号课堂：编程范式与OOP思想"
                 "郑辉" "电子工业出版社")
 (book-bib-entry #:date "2022"
                 "6kids" "少儿计算思维养成记：六个孩子的编程学习笔记"
                 (list "包若宁" "卜文远" "傅鼎荃" "魏文珊" "张秦汉" "卜东波") "机械工业出版社")
 (book-bib-entry #:date "2015" #:edition "8th"
                 "SE:APA" "Software Engineering: A Practitioner's Approach"
                 (list "Roger S. Pressman" "Bruce R. Maxim")
                 "机械工业出版社")]
