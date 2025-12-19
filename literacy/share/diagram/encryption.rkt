#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(default-diaflow-track-font (desc-font #:size 'x-large))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! encryption.dia #:start '/doc/plain/s
  [#:background 'White
   #:block-desc #hasheq((/doc/plain/s  . "明文\nplain")
                        (/doc/cipher/s . "密文\nqmbjo")
                        (/doc/plain/r  . "明文\nplain")
                        (/doc/cipher/r . "密文\nqmbjo"))] #:-
  (move-right 1.5 '/doc/cipher/s "加密")
  (move-right 1.5 '/doc/cipher/r "传输")
  (move-right 1.5 '/doc/plain/r "解密")

  (jump-to -0.5-0.8i)
  (move-right 2.5)
  (move-down 2)
  (move-left 2.5 #false)
  (move-up 2)

  (jump-to 2.5-0.8i)
  (move-right 2.5)
  (move-down 2)
  (move-left 2.5 #false)
  (move-up 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  encryption.dia)
