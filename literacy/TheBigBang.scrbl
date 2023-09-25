#lang scribble/book

@require{literacy.rkt}
@require{graphviz.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[#:subtitle "跨学科 ior 对比语言方法" #:hide-version? #true]{不只是“C++ 异或 Python”}

@texbook-frontmatter[]

@handbook-preface-section{序}

假设存在一个包罗万象的程序语言，
那我们学习任何一门具体的程序语言时会碰到的东西
也一定能在学习这个假想的语言时碰到。
如果一定要从语言角度来回答“如何学习编程，并能适应智能时代”，
那就是学习这个包罗万象的假想语言，
并知道如何将它翻译成稳定的具体的可(编译)执行的代码。

本书以 C++ 和 Python 为主线教授程序设计和程序语言，
也会援引 Racket 和其他语言，
或作为历史见证，
或作为辅助工具，
或只是出于写作本书的便利。
特别地，
本书第四部分《函数式编程》为对数学、
计算机科学、程序语言理论感兴趣的读者准备，
该部分章节以 Racket 为主线，
也会援引其他学术型语言。

知识是网状结构，
但书只能以树形结构组织。因此，
对本书章节标题的理解应该是“本节以此话题为主线”，
而不应该理解为“本节只讲这个主题”,
也不应该理解为“只有本节才讲这个主题”。
读者应铭记于心：
跨学科和对比学习是贯穿全书的暗线。

@handbook-smart-table[]

@texbook-mainmatter[]
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{big-bang/greetings.scrbl}
@include-section{big-bang/typesystem.scrbl}
@include-section{big-bang/nature.scrbl}
@include-section{big-bang/lambda.scrbl}
@include-section{big-bang/goodbye.scrbl}

@texbook-appendix{附录}

@include-section{big-bang/environment.scrbl}
@include-section{big-bang/answers.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:index-section? #true #:numbered? #true
 (book-bib-entry #:date "1997"
                 "CnC" "Computability and Complexity: From a Programming Perspective"
                 "Neil D. Jones" "The MIT Press")
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
 (book-bib-entry #:date "2019" #:edition "3rd"
                 "GEA" "Game Engine Architecture"
                 "Jason Gregory" "CRC Press")
 (book-bib-entry #:date "2020"  #:edition "1st"
                 "DTC" "Don't teach coding: until you read this book"
                 (list "Lindsey D. Handley" "Stephen R. Foster")
                 "Jossey Bass")
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
                 "Cambridge University Press")
 (url-bib-entry #"ST" "什么是科学思维？如何建立科学思维？"
                "https://b23.tv/WvDw04W"
                #:author (list "芳斯塔芙" "鬼谷藏龙")
                #:date "2023")]
