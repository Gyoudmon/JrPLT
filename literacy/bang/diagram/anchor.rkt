#lang racket/base

(provide (all-defined-out))

(require racket/string)

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define anchor.dia
  (let ([anchor-font (desc-font #:size 9)])
    (geo-cc-superimpose
     (geo-blank 1 128)
     (for/fold ([base (geo-square 96 #:fill 'palegreen #:stroke #false)])
               ([superimpose (in-list (list geo-lt-superimpose geo-ct-superimpose geo-rt-superimpose
                                            geo-lc-superimpose geo-cc-superimpose geo-rc-superimpose
                                            geo-lb-superimpose geo-cb-superimpose geo-rb-superimpose))])
       (let* ([anchor (string-upcase (cadr (string-split (symbol->string (object-name superimpose)) "-")))]
              [label (geo-text anchor anchor-font #:color 'crimson)])
         (superimpose base label))))))
