#lang scribble/report

@require{literacy.rkt}

@handbook-portfolio-title{吴玚}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-smart-table[]
@$tex:setcounter['page 1]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-part-section{学生档案}

@include-section{student/wuchang/self.scrbl}

@handbook-part-section{课程简介}

@include-section{discipline/big-bang.scrbl}

@handbook-part-section{班级简介}

@include-section{class/lambda-girl.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@texbook-appendix{附录}

@include-section{faq.scrbl}

@handbook-appendix[#:index-section? #true #:numbered? #true]
