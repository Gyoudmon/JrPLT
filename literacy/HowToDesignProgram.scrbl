#lang scribble/report

@require{htdp/literacy.rkt}

@(require geofun/vector)

@(define font (desc-font #:family 'math #:size 16.0))
@(define arrow (geo-arrow 3.0 32.0 #:stroke #false #:fill 'DodgerBlue))

@(define splash
   (geo-hc-append #:gapsize 4.0
                  (geo-text " x " font #:color 'DeepSkyBlue)
                  arrow
                  (geo-cc-superimpose (geo-rectangle 48 #:stroke 'RoyalBlue)
                                      (geo-text " y = f(x) " (desc-font font #:size 12.0) #:color 'SteelBlue))
                  arrow
                  (geo-text " y " font #:color 'DeepSkyBlue)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:documentclass 'ctexrep
 #:document-options '([heading . #true] [fontset . macnew])
 #:subtitle "小小语言设计师启蒙教科书"
 #:figure splash
 #:hide-version? #true
 ]{如何设计程序}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-smart-table[]

@$tex:setcounter['page 1]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{htdp/big-bang.scrbl}
@include-section{htdp/stema.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@texbook-appendix{附录}

@include-section{linguisteen/environment.scrbl}

@handbook-appendix[
 #:reference-section? #false
 #:bibliography-section? #false
 #:index-section? #false]
