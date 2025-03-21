#lang racket/base

(provide (all-defined-out))
(provide (all-from-out "../literacy.rkt"))

(require "../literacy.rkt")

(require digimon/syntax)

(require (for-syntax racket/list))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tamer-indexed-block-hide-chapter-index #false)

(tamer-default-figure-label "图")
(tamer-default-figure-label-separator #false)
(tamer-default-figure-label-tail " ")
(tamer-default-figure-label-style 'bold)

(tamer-story-submodule-name 'advent)
(tamer-story-propagate-exceptions #true)

(tamer-filebox-line-number-space 2)
(tamer-marginnote-left? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-mparwidth (* (/ 72.0 25.4) 36 #;'mm))
(define aoc-linewidth 400.0)
(define aoc-flowchart-ratio 0.85)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (aoc-task stx)
  (syntax-parse stx #:datum-literals []
    [(_ year day title ...)
     (syntax/loc stx
       (handbook-root-story #:tag (format "aoc:~a:~a" year day)
                            (hyperlink (format "https://adventofcode.com/~a/day/~a" year day)
                                       title ...)))]))

(define-syntax (aoc-desc stx)
  (syntax-parse stx #:datum-literals []
    [(_ (~alt (~once (~seq #:keywords [kw ...]))
              (~once (~seq #:edition [edition date])))
        ...)
     (syntax/loc stx
       (handbook-sidenote*
        (list (emph "时间戳") ": " (tt date) ~ (format "第~a版" 'edition) (linebreak))
        (list (emph "文字量") ": " (handbook-word-count #:make-element elem) (linebreak))

        (let ([keywords (list kw ...)])
          (when (pair? keywords)
            (add-between #:splice? #true
                         #:before-first (list (emph "关键词") ":" ~)
                         (for/list ([keyword (in-list keywords)])
                           (racketkeywordfont (tech keyword)))
                         (list "," ~))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-tamer-path
  (lambda [path]
    (digimon-path 'tamer path)))

(define aoc-image
  (lambda [path #:scale [scale 1.0]]
    (image (digimon-path 'stone "AOC" path) #:scale scale)))

(define aoc-example
  (let ([csep (hspace 2)])
  (lambda [data desc]
    (list (linebreak)
          (tabular #:sep csep
                   #:style 'boxed
                   #:row-properties '((top top))
                   
                   (list (list data desc)))
          (linebreak)))))

(define aoc-emph
  (lambda argv
    (apply elem #:style story-emph-style argv)))

(define aoc-question
  (lambda argv
    (apply elem #:style story-question-style argv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define story-style (make-style "aocStoryTearedPaper" '(multicommand)))
(define story-complain-style (make-style "aocComplain" null))
(define story-bonus-style (make-style "aocBonus" null))
(define story-emph-style (make-style "aocEmph" null))
(define story-question-style (make-style "aocQuestion" null))

(define aoc-story
  (lambda [title . paras]
    (make-nested-flow story-style
                      (list (para title)
                            (apply tamer-indent-paragraphs paras)))))

(define aoc-complain
  (lambda paras
    (make-nested-flow story-complain-style 
                      (list (apply tamer-indent-paragraphs paras)))))

(define aoc-bonus
  (lambda paras
    (make-nested-flow story-bonus-style 
                      (list (apply tamer-indent-paragraphs paras)))))

(define aoc-tag
  (lambda [bcolor fgcolor content]
    (texbook-command "tagBox" #:opt-args (list bcolor) #:args (list fgcolor) content)))

(define aoc-detailed-tag
  (lambda [bcolor fgcolor title body]
    (texbook-command "tagDetailedBox" #:opt-args (list bcolor) #:args (list fgcolor title) body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define natural-language-isnt-natural (aoc-tag 'Chocolate 'Black (emph "自然语言不自然")))
(define natural-language-isnt-natural* (handbook-sidenote* natural-language-isnt-natural))
