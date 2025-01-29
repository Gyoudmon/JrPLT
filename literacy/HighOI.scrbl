#lang scribble/book

@require{literacy.rkt}
@require{../stone/self/logo.rkt}

@(require geofun/resize)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:documentclass 'book
 #:document-options '(openany)
 #:subtitle "程序语言理论视角"
 #:hide-version? #true
 #:figure @(geo-scale splash:OI (* 0.618 0.618))
 ]{高观点下的信息学奥赛}

@texbook-frontmatter[]

@handbook-preface-section{序}

@handbook-smart-table[]

@texbook-mainmatter[]
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@texbook-appendix{附录}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:numbered? #true
 (book-bib-entry #:date "2023" #:edition "10th"
                 "EnMSM:TD" "美国中小学数学教师实践手册"
                 (list "John A. Van de Walle" "Karen S. Karp" "Jennifer M. Bay-Williams")
                 "华东师范大学出版社")
 (book-bib-entry #:date "2023" #:edition "1st"
                 "TBoMfC" "写给孩子的数学之美"
                 (list "昍爸" "昍妈")
                 "人民邮电出版社")]
