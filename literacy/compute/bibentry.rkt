#lang racket/base

(provide (all-defined-out))

(require digimon/tamer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bibentries
  (list (book-bib-entry #:date "2018" #:edition "2nd" #:url "https://htdp.org/2024-11-6/Book/index.html"
                        "2htdp" "How to Design Programs: An Introducation to Programming and Computing"
                        (list "Matthias Felleisen" "Robert Bruce Findler" "Matthew Flatt" "Shriram Krishnamurthi")
                        "The MIT Press")
        (book-bib-entry #:date "2020"
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
        (book-bib-entry #:date "2013"
                        "RoR" "Realm of Racket: Learn to Program, One Game at a Time!"
                        (list "Forrest Bice" "Rose DeMaio" "Spencer Florence" "Feng-Yun Mimi Lin" "Scott Lindeman"
                              "Nicole Nussbaum" "Eric Peterson" "Ryan Plessner" "David Van Horn" "Matthias Felleisen"
                              "Conrad Barski")
                        "No Starch Press")
        (book-bib-entry #:date "2008"
                        "RWH" "Real World Haskell: Code You Can Belive In"
                        (list "Bryan O’Sullivan" "John Goerzen" "Don Stewart")
                        "O'Reilly Media")
        (book-bib-entry #:date "2012" #:edition "2nd"
                        "PL" "Programming Language: Application and Interpretation"
                        "Shriram Krishnamurthi" #false)
        (book-bib-entry #:date "2006" #:edition "2nd"
                        "DMUaC" "Discrete Mathematics Using a Computer"
                        "John O’Donnell" "Cordelia Hall" "Rex Page"
                        "Springer")
        (book-bib-entry #:date "2012"
                        "NoC" "The Nature of Code"
                        "Daniel Shiffman" #false)
        (book-bib-entry #:date "2017" #:edition "3rd" #:url "https://doi.org/10.1016/B978-0-12-800645-0.50026-9"
                        "PBR" "Physically Based Rendering: From Theory to Impplementation"
                        (list "Matt Pharr" "Wenzel Jakob" "Greg Humphreys")
                        "The MIT Press")
        (book-bib-entry #:date "2019" #:edition "3rd"
                        "GEA" "Game Engine Architecture"
                        "Jason Gregory" "CRC Press")
        (book-bib-entry #:date "2023" #:edition "10th"
                        "EnMSM:TD" "美国中小学数学教师实践手册"
                        (list "John A. Van de Walle" "Karen S. Karp" "Jennifer M. Bay-Williams")
                        "华东师范大学出版社")
        (book-bib-entry #:date "2024" #:edition "1st"
                        "tFoM" "基础数学讲义：走向真正的数学"
                        (list "Ian Stewart" "David Tall")
                        "人民邮电出版社")
        (book-bib-entry #:date "2016"
                        "UNiESM" "数学家讲解小学数学"
                        "【美】伍鸿熙" "北京大学出版社")
        (book-bib-entry #:date "2009"
                        "TCC" "冒号课堂：编程范式与OOP思想"
                        "郑辉" "电子工业出版社")))

