#lang racket/base

(require scriblib/bibtex)

(provide (all-defined-out))
(provide (all-from-out "../literacy.rkt"))

(require "../literacy.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(current-tongue 'en)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /dev/src  (build-path "digitama" "plteen"))
(define /dev/t  (build-path "tamer"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plteen-cpp
  (lambda [id caption subpath start [end #px"END"] [ocness 'open-open]]
    (tamer-code! #:oc-ness ocness #:rootdir /dev/src
                 id caption subpath start end)))

(define tamer-cpp
  (lambda [id caption subpath start [end #px"END"] [ocness 'open-open]]
    (tamer-code! #:oc-ness ocness #:rootdir /dev/t
                 id caption subpath start end)))

(define tamer-rkt
  (lambda [subpath start [end #px"END"] [ocness 'open-open]]
    (tamer-racketbox/region #:pxstart start #:pxstop end
                            (build-path (digimon-path 'zone) /dev/t subpath))))
