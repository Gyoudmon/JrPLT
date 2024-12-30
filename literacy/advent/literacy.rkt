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
       (itemlist #:style 'compact
                 (item (add-between #:splice? #true
                                    #:before-first (list (emph "关键词") ":" ~)
                                    (for/list ([key (in-list (list kw ...))])
                                      (racketkeywordfont (tech key)))
                                    (list "," ~)))
                 (item (list (emph "时间戳") ": " (tt date) ~ (format "第~a版" 'edition)))
                 (item (list (emph "文字量") ": " (handbook-word-count #:make-element elem)))))]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define story*
  (lambda argv
    (nested #:style 'inset
            (for/list ([content (in-list argv)]
                       #:when (element? content))
              (para content)))))

(define story
  (lambda argv
    (apply racketplainfont
           (list* (hspace 4) argv))))

