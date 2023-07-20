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
并知道如何将它翻译成稳定的可执行的具体的程序语言。

@handbook-smart-table[]

@texbook-mainmatter[]
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{greetings.scrbl}
@include-section{typesystem.scrbl}
@include-section{nature.scrbl}
@include-section{ecology.scrbl}
@include-section{goodbye.scrbl}

@texbook-appendix{附录}

@include-section{environment.scrbl}
@include-section{answers.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@texbook-backmatter[]

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
                 "机械工业出版社")]
