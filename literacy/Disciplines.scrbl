#lang scribble/report

@require{disciplines/literacy.rkt}
@require{disciplines/bibentry.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:documentclass 'ctexrep
 #:document-options '([heading . #true] [fontset . macnew])
 #:subtitle "课程重难点"
 #:hide-version? #true
 ]{青少计算机科学}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-smart-table[]

@$tex:setcounter['page 1]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{disciplines/big-bang.scrbl}
@include-section{disciplines/contest.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@texbook-appendix{附录}

@include-section{linguisteen/environment.scrbl}

@handbook-appendix[#:numbered? #true bibentries]
