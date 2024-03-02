#lang racket/base

(provide (all-defined-out))

(require scribble/manual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stream-goals
  (hasheq 20240130
          (hasheq 1020
                  (list "理解文件基础操作"
                        "理解标准输入输出流"
                        (list "认识流对象操作符(" (racketparenfont ">>") "和" (racketparenfont "<<") ")")
                        "理解文件复制的逻辑"))))
