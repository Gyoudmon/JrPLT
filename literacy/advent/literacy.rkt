#lang racket/base

(provide (all-defined-out))
(provide (all-from-out "../literacy.rkt"))

(require "../literacy.rkt")

(require digimon/syntax)

(require (for-syntax racket/list))
(require (for-syntax racket/symbol))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(tamer-indexed-block-hide-chapter-index #true)

(tamer-default-figure-label "图")
(tamer-default-figure-label-separator #false)
(tamer-default-figure-label-tail ". ")
(tamer-default-figure-label-style 'bold)

(tamer-story-submodule-name 'advent)
(tamer-story-propagate-exceptions #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (aoc-task stx)
  (syntax-parse stx #:datum-literals []
    [(_ year day title ...)
     (syntax/loc stx
       (handbook-story (hyperlink (format "https://adventofcode.com/~a/day/~a" year day)
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
                 (item (list (emph "时间戳") ": " (tt date) ~ (format "第~a版" 'edition)))))]))

(define-syntax ($argv stx)
  (syntax-case stx []
    [(_ [aname desc ...] [aname:rest desc:rest ...] ...)
     (with-syntax* ([name (datum->syntax #'aname (symbol->immutable-string (syntax-e #'aname)))]
                    [(name:rest ...) (for/list ([<name> (in-list (syntax->list #'(aname:rest ...)))])
                                       (datum->syntax <name> (symbol->immutable-string (syntax-e <name>))))]
                    [maxlength (apply max (map string-length (map syntax-e (syntax->list #'(name name:rest ...)))))])
       (syntax/loc stx
         (list (elem (hspace (- maxlength (string-length name)))
                     (racketvarfont name) ~ desc ...)
               (elem (linebreak) "; " (hspace (- maxlength (string-length name:rest)))
                     (racketvarfont name:rest) ~ desc:rest ...) ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define story*
  (lambda argv
    (nested #:style 'inset
            (decode-compound-paragraph (decode-flow argv)))))

(define story
  (lambda argv
    (apply racketplainfont
           (list* (hspace 4) argv))))

