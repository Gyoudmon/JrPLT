#lang racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/tamer))
(provide (rename-out [:cmt :desc] [:out :type]))

(require digimon/tamer)
(require digimon/collection)

(require scribble/manual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Just in case for README.md
(enter-digimon-zone!)

(tamer-block-label-separator #false)
(tamer-block-label-tail "： ")
(tamer-block-label-style 'bold)

(tamer-default-figure-label "图")
(tamer-default-code-label "段")

(current-tongue 'zh-Hans)

(handbook-bibtex-load (digimon-path 'literacy "bibliography.tex"))

(d2-default-sketch? #true)
(d2-default-theme 'Terminal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /dev/src (build-path "literacy" "share" "src"))

(define stone-image
  (lambda [path #:scale [scale 1.0]]
    (image (digimon-path 'stone path) #:scale scale)))

(define tamer-c++
  (lambda [id caption subpath start [end #px"END"] [ocness 'close-open]]
    (tamer-code! #:oc-ness ocness #:rootdir /dev/src
                 id caption subpath start end)))

(define tamer-c++-class
  (lambda [id caption subpath]
    (tamer-code-class #:rootdir /dev/src
                      id caption subpath)))

(define tamer-c++-function
  (lambda [id caption subpath #:ns [ns #false] #:subpattern [subpattern #false]]
    (tamer-code-function #:ns ns #:subpattern subpattern #:rootdir /dev/src
                         id caption subpath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define idea
  (lambda argv
    (apply racketoutput argv)))

(define focus
  (lambda argv
    (apply racketvalfont argv)))

(define question
  (lambda argv
    (apply racketparenfont argv)))

(define mlink
  (lambda argv
    (apply litchar argv)))

(define hotkeys
  (lambda keys
    (elem #:style "keys"
          (string-join (add-between (map ~a keys) "+")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define japanese
  (lambda [#:latex? [latex? 'auto] . contents]
    (cond [(symbol? latex?)
           (make-traverse-element
            (λ [get set!]
              (japanese #:latex? (handbook-latex-renderer? get)
                       contents)))]
          [(and latex?) (make-element "japanese" (list contents))]
          [else contents])))
