#lang scribble/report

@require{compute/literacy.rkt}
@require{bibentry.rkt}

@(require geofun/vector)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define font (desc-font #:family 'math #:size 16.0))
@(define arrow (geo-arrow 3.0 32.0 #:stroke #false #:fill 'DodgerBlue))
@(define gapsize 4.0)

@(define geo-function
   (lambda [x y z f]
     (geo-hc-append #:gapsize gapsize
                    (geo-vr-append (geo-hc-append #:gapsize gapsize (geo-text (format " ~a " x) font #:color 'DeepSkyBlue) arrow)
                                   (geo-hc-append #:gapsize gapsize (geo-text (format " ~a " y) font #:color 'DeepSkyBlue) arrow))
                    (geo-cc-superimpose (geo-rectangle 64 #:stroke 'RoyalBlue)
                                        (geo-text (format " z = ~a (~a, ~a) " f x y)
                                                  (desc-font font #:size 12.0) #:color 'SteelBlue))
                    arrow
                    (geo-text (format " ~a " z) font #:color 'DeepSkyBlue))))

@(define splash
   (geo-vc-append #:gapsize (font-size font)
                  ;(geo-function '1 '1 '2 '+)
                  (geo-function 'x 'y 'z 'f)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:documentclass 'ctexrep
 #:document-options '([heading . #true] [fontset . macnew])
 #:subtitle "小小语言设计师的指令式编程启蒙"
 #:figure splash
 #:hide-version? #true
 ]{教会计算机干活吧}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-smart-table[]

@$tex:setcounter['page 1]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{compute/greetings.scrbl}
@include-section{compute/big-bang.scrbl}
@include-section{compute/fxdata.scrbl}
@include-section{compute/vardata.scrbl}
@include-section{compute/abstract.scrbl}
@include-section{compute/memory.scrbl}
@include-section{compute/power.scrbl}
@include-section{compute/goodbye.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@texbook-appendix{附录}

@include-section{linguisteen/environment.scrbl}
@include-section{compute/for-parent.scrbl}

@handbook-appendix[#:numbered? #true
 bibentries]

