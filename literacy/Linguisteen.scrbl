#lang scribble/book

@(require geofun/resize) 

@require{literacy.rkt}
@require{bibentry.rkt}

@require{../stone/self/logo.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:documentclass 'book
 #:document-options '(openany oneside)
 #:subtitle (list "跨学科方法" @smaller{Racket、C++的语言朋友们})
 #:figure @geo-scale[@geo-interdisciplinary-screen[] 0.42]
 #:hide-version? #true
 ]{代码源记}

@texbook-frontmatter[]

@handbook-preface-section{序}

假设存在一个包罗万象的程序语言，
那我们学习任何一门具体的程序语言时会碰到的东西
也一定能在学习这个假想的语言时碰到。
如果一定要从语言角度来回答“如何学习编程，并能适应智能时代”，
那就是学习这个包罗万象的假想语言，
并知道如何将它翻译成稳定的具体的可(编译)执行的代码。

已知的语言光谱至少包含数以百计的常用语言。
选择何种语言入门编程在实践上没有技巧，
全靠机缘巧合(比如你碰巧遇到了擅长某某语言的启蒙老师)；
但理论上说，某些语言确实比另一些语言更适合用来入门。
综合多方因素考虑，
本书重点着墨的语言分别趋近语言光谱的两个端点：
机器端和数学端。

具体来说，
本书以 C++ 为主线教授程序语言和程序设计，
会援引 Racket 和其他语言，
或作为历史见证，
或作为辅助工具，
或只是出于写作本书的便利。
特别地，
本书第四部分《函数式编程》专为对@emph{数学}、
@emph{计算机科学}和@emph{程序语言理论}感兴趣的读者准备，
该部分章节以 Racket 为主线，
也会援引其他学术型语言。
想知道 Python 的位置吗？
它会以出其不意的形式登场，
在那之前请先好好打基础(嘘)。

知识是网状结构，
但书只能以树形结构组织。因此，
对本书章节标题的理解应该是“本章节以此话题为主线”，
而不应该理解为“本章节只讲这个主题”,
也不应该理解为“只有本章节才讲这个主题”。
读者应铭记于心：
@focus{跨学科和对比学习是贯穿全书的暗线}。

@handbook-smart-table[]

@texbook-mainmatter[]
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{linguisteen/greetings.scrbl}
@include-section{linguisteen/typesystem.scrbl}
@include-section{linguisteen/nature.scrbl}
@include-section{linguisteen/lambda.scrbl}
@include-section{linguisteen/goodbye.scrbl}

@texbook-appendix{附录}

@include-section{linguisteen/environment.scrbl}
@include-section{linguisteen/answers.scrbl}

@; force hiding numbers of following section, same as setting #:numbered? to `#true`
@;texbook-backmatter[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:numbered? #true bibentries
 (book-bib-entry #:date "1997"
                 "CnC" "Computability and Complexity: From a Programming Perspective"
                 "Neil D. Jones" "The MIT Press")
 (book-bib-entry #:date "2017" #:edition "3rd" #:url "https://doi.org/10.1016/B978-0-12-800645-0.50026-9"
                 "PBR" "Physically Based Rendering: From Theory to Impplementation"
                 (list "Matt Pharr" "Wenzel Jakob" "Greg Humphreys")
                 "The MIT Press")
 (book-bib-entry #:date "2019" #:edition "3rd"
                 "GEA" "Game Engine Architecture"
                 "Jason Gregory" "CRC Press")
 (book-bib-entry #:date "2015" #:edition "8th"
                 "SE:APA" "Software Engineering: A Practitioner's Approach"
                 (list "Roger S. Pressman" "Bruce R. Maxim")
                 "机械工业出版社")
 (book-bib-entry #:date "2018"
                 "CJLH" "The Combridge Handbook of Japanese Linguistics"
                 (editor "Yoko Hasegawa")
                 "Cambridge University Press")]
