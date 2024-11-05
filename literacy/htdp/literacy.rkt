#lang racket/base

(provide (all-defined-out))
(provide (all-from-out "../literacy.rkt"))

(require "../literacy.rkt")

(require scribble/core)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (handbook-portfolio-story stx)
  (syntax-parse stx #:datum-literals []
    [(_ argl ...)
     (syntax/loc stx
       (handbook-root-story argl ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tamer-indexed-block-hide-chapter-index #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tag:Racket (elem #:style "langTag" "Racket"))
(define tag:C++ (elem #:style "langTag" "C++"))
(define tag:Python (elem #:style "langTag" "Python"))
(define tag:Scratch (elem #:style "langTag" "Scratch"))
