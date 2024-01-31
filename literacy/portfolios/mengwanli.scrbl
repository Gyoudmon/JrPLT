#lang scribble/report

@require{literacy.rkt}
@require{bibentry.rkt}

@handbook-portfolio-title{孟万里}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-smart-table[]
@$tex:setcounter['page 1]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-part-section{学生点评}

@include-section{student/mengwanli/self.scrbl}

@handbook-part-section{课程简介}

@include-section{discipline/big-bang.scrbl}

@handbook-part-section{班级简介}

@include-section{class/observer.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@texbook-appendix{附录}

@include-section{faq.scrbl}
@include-section{../big-bang/environment.scrbl}

@handbook-appendix[#:index-section? #true #:numbered? #true bibentries]
