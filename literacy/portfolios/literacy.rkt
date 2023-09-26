#lang racket/base

(provide (all-defined-out))
(provide (all-from-out "../literacy.rkt"))

(require "../literacy.rkt")

(require scribble/core)
(require scribble/latex-properties)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tamer-indexed-block-hide-chapter-index #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (handbook-portfolio-title stx)
  (syntax-parse stx #:datum-literals []
    [(_ name ...)
     (syntax/loc stx
       (handbook-title #:hide-version? #true
                       #:subtitle "青少计算机科学"
                       #:author "居老师"
                       #:properties tamer:tex:prop
                       name ... (hspace 1) "成长档案"))]))

(define-syntax (handbook-portfolio-story stx)
  (syntax-parse stx #:datum-literals []
    [(_ argl ...)
     (syntax/loc stx
       (handbook-root-story argl ...))]))

(define-syntax (class-desc stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~once (~seq #:langs [langs ...]))
              (~once (~seq #:properties [properties ...]))
              (~once (~seq #:capacity capacity))
              (~once (~seq #:next [next ...])))
        ...)
     (syntax/loc stx
       (nested #:style 'vertical-inset
               (para (emph "授课语言：")
                     (add-between (list langs ...) " "))
               
               (para (emph "培养线索：")
                     (add-between (list properties ...) " "))
               
               (para (emph "班级容量：")
                     capacity)

               (para (emph "进阶方向：")
                     (add-between (list next ...) " "))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stu-name
  (lambda name
    (make-element #false name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer:tex:prop
  (make-tex-addition
   (digimon-path 'stone "Portfolios/tamer.tex")))

(define tag:Racket (elem #:style "langTag" "Racket"))
(define tag:C++ (elem #:style "langTag" "C++"))
(define tag:Python (elem #:style "langTag" "Python"))
(define tag:Scratch (elem #:style "langTag" "Scratch"))

(define tag:CT (elem #:style "disTag" "计算思维"))
(define tag:BP (elem #:style "disTag" "基础训练"))
(define tag:EE (elem #:style "disTag" "寓教于乐"))

(define tag:PL (elem #:style "disTag" "计算机语言"))
(define tag:CS (elem #:style "disTag" "计算机科学"))
(define tag:CE (elem #:style "disTag" "计算机工程"))

(define tag:MM (elem #:style "disTag" "数学建模"))
(define tag:SC (elem #:style "disTag" "科学计算"))
(define tag:OI (elem #:style "disTag" "信息学奥赛"))
(define tag:ET (elem #:style "disTag" "电子技术"))
(define tag:OW (elem #:style "disTag" "其他"))

(define tag:TLDR (elem #:style "tldrTag" "太长;不看"))
(define tag:deep (elem #:style "deepTag" "少侠;留步"))
