#lang scribble/report

@require{literacy.rkt}
@require{bibentry.rkt}

@handbook-portfolio-title{丁嘉琪}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-smart-table[]
@$tex:setcounter['page 1]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-part-section{学生点评}

@include-section{student/dingjiaqi/self.scrbl}

@handbook-part-section{课程简介}

@include-section{discipline/big-bang.scrbl}

@handbook-part-section{班级简介}

@include-section{class/lambda-girl.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@texbook-appendix{附录}

@include-section{faq.scrbl}
@include-section{../big-bang/environment.scrbl}

@handbook-appendix[#:index-section? #true #:numbered? #true bibentries]
