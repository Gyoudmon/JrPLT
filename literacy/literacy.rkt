#lang racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/tamer))

(require digimon/tamer)
(require digimon/collection)

(require scribble/manual)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Just in case for README.md
(enter-digimon-zone!)

(tamer-indexed-block-hide-chapter-index #false)

(tamer-block-label-separator #false)
(tamer-block-label-tail "： ")
(tamer-block-label-style 'bold)

(tamer-default-figure-label "图")
(tamer-default-code-label "段")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stone-image
  (lambda [path #:scale [scale 1.0]]
    (image (digimon-path 'stone path) #:scale scale)))

(define tamer-c++
  (lambda [id caption subpath start [end #px"END"] [ocness 'open]]
    (tamer-code! #:oc-ness ocness #:rootdir (build-path ".." "cpp")
                 id caption subpath start end)))

(define tamer-c++-class
  (lambda [id caption subpath]
    (tamer-code-class #:rootdir (build-path ".." "cpp")
                      id caption subpath)))

(define tamer-c++-function
  (lambda [id caption subpath #:ns [ns 'WarGrey::IMS] #:subpattern [subpattern #false]]
    (tamer-code-function #:rootdir (build-path ".." "cpp") #:ns ns #:subpattern subpattern
                         id caption subpath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define desc
  (lambda argv
    (apply racketmetafont argv)))

(define name
  (lambda [arg]
    (tamer-deftech arg)))

(define term-name
  (lambda [arg]
    (defterm arg)))

(define variable
  (lambda argv
    (apply racketvarfont argv)))

(define sign
  (lambda argv
    (apply racketparenfont argv)))

(define id
  (lambda argv
    (apply racketidfont argv)))

(define type
  (lambda body
    (apply racketvalfont body)))

(define form
  (lambda argv
    (apply racketkeywordfont argv)))

(define idea
  (lambda argv
    (apply racketoutput argv)))

(define focus
  (lambda argv
    (apply racketvalfont argv)))

(define thus
  (lambda argv
    (apply racketresultfont argv)))

(define note
  (lambda argv
    (apply racketcommentfont argv)))

(define fallacy
  (lambda argv
    (apply racketerror argv)))

(define question
  (lambda argv
    (apply racketparenfont argv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-tabular/2
  (lambda [table-head table-rows [gap 1] [empty-cols (list "")]]
    (define col-size (length table-head))
    (define col-properties (make-list col-size 'left))
    (define cel-properties (make-list col-size '()))
     
    (tabular #:sep (hspace gap)
             #:style 'centered
             #:row-properties '(bottom-border ())
             #:column-properties (append col-properties '(center) col-properties)
             #:cell-properties (list (append cel-properties '(left-border) cel-properties))
             (cons (append table-head empty-cols table-head)
                   (let make-table ([rows table-rows]
                                    [swor null])
                     (cond [(null? rows) (reverse swor)]
                           [(null? (cdr rows)) (reverse (cons (append (car rows) empty-cols (make-list col-size 'cont)) swor))]
                           [else (make-table (cddr rows) (cons (append (car rows) empty-cols (cadr rows)) swor))]))))))
