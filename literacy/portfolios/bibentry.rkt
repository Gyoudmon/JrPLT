#lang racket/base

(provide (all-defined-out))

(require digimon/tamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bibentries
  (list (book-bib-entry #:date "2020"
                        "DTC" "Don't teach coding: until you read this book"
                        (list "Lindsey D. Handley" "Stephen R. Foster")
                        "Jossey Bass")
        (book-bib-entry #:date "2017" #:doi "https://doi.org/10.7551/mitpress/10655.001.0001"
                        "CL" "Coding Literacy: How Computer Programming Is Changing Writing"
                        "Annette Vee" "The MIT Press")
        (book-bib-entry #:date "2009" #:edition "3rd"
                        "ItA" "Introduction to Algorithms"
                        (list "Thomas H. Cormen" "Charles E. Leiserson"
                              "Ronald L. Rivest" "Clifford Stein")
                        "The MIT Press")
        (book-bib-entry #:date "2011"
                        "LoL" "Land of Lisp: Learn to Program in Lisp, One Game at a Time!"
                        "Conrad Barski, MD" "No Starch Press")
        (book-bib-entry #:date "2012" #:edition "2nd"
                        "PL" "Programming Language: Application and Interpretation"
                        "Shriram Krishnamurthi" #false)
        (book-bib-entry #:date "2012"
                        "NoC" "The Nature of Code"
                        "Daniel Shiffman" #false)
        (book-bib-entry #:date "2019" #:edition "3rd"
                        "GEA" "Game Engine Architecture"
                        "Jason Gregory" "CRC Press")
        (book-bib-entry #:date "2023" #:edition "10th"
                        "EnMSM:TD" "美国中小学数学教师实践手册"
                        (list "John A. Van de Walle" "Karen S. Karp" "Jennifer M. Bay-Williams")
                        "华东师范大学出版社")
        (book-bib-entry #:date "2016"
                        "UNiESM" "数学家讲解小学数学"
                        "【美】伍鸿熙" "北京大学出版社")
        (book-bib-entry #:date "2009"
                        "TCC" "冒号课堂：编程范式与OOP思想"
                        "郑辉" "电子工业出版社")
        (book-bib-entry #:date "2022"
                        "6kids" "少儿计算思维养成记：六个孩子的编程学习笔记"
                        (list "包若宁" "卜文远" "傅鼎荃" "魏文珊" "张秦汉" "卜东波") "机械工业出版社")))

