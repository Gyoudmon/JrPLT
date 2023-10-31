#lang at-exp racket/base

(provide (all-defined-out))

(require bitmap)

(require racket/symbol)

(require scribble/base)
(require scribble/core)
(require scribble/manual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define anchor-font (desc-font #:size 9))

(define anchor-demo
  (for/fold ([base (bitmap-solid 'palegreen 96)])
            ([anchor (in-list '(LT CT RT LC CC RC LB CB RB))])
    (let ([label (bitmap-text anchor anchor-font #:color 'crimson)])
      (case anchor
        [(CT) (bitmap-ct-superimpose base label)]
        [(RT) (bitmap-rt-superimpose base label)]
        [(LC) (bitmap-lc-superimpose base label)]
        [(CC) (bitmap-cc-superimpose base label)]
        [(RC) (bitmap-rc-superimpose base label)]
        [(LB) (bitmap-lb-superimpose base label)]
        [(CB) (bitmap-cb-superimpose base label)]
        [(RB) (bitmap-rb-superimpose base label)]
        [else (bitmap-lt-superimpose base label)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define anchors
   (hasheq 'L @tt{@litchar{L}eft}
           'C @tt{@litchar{C}enter}
           'R @tt{@litchar{R}ight}
           'T @tt{@litchar{T}op}
           'B @tt{@litchar{B}ottom}))

(define colored-percentages
  (hasheq 0.0 (elem #:style (make-style #false (list (make-color-property "orange"))) "0.0")
          0.5 (elem #:style (make-style #false (list (make-color-property "green"))) "0.5")
          1.0 (elem #:style (make-style #false (list (make-color-property "violet"))) "1.0")))

@(define width%s  (hasheq 'L 0.0 'C 0.5 'R 1.0))
@(define height%s (hasheq 'T 0.0 'C 0.5 'B 1.0))

@(define make-anchor-row
   (lambda [xalign yalign semantics]
     (list @litchar[(string-append (symbol->immutable-string xalign)(symbol->immutable-string yalign))]
           (list (hash-ref anchors xalign) "-" (hash-ref anchors yalign))
           (tt semantics)
           (hash-ref colored-percentages (hash-ref width%s xalign))
           (hash-ref colored-percentages (hash-ref height%s yalign)))))

(define anchor-table
  (tabular #:sep @hspace[4]
           #:column-properties '(vcenter)
           (list (list @tabular[(list (list @hspace[1])
                                      (list anchor-demo))]
                       @tabular[#:style 'boxed #:sep @hspace[2]
                                #:column-properties '(center)
                                #:row-properties '((top-border bottom-border))
                                
                                (list (list @emph{锚点缩写} @emph{锚点全称} @emph{对齐语义} @emph{宽度%} @emph{高度%})
                                      (make-anchor-row 'L 'T  "左上角对齐")
                                      (make-anchor-row 'C 'T  "中上对齐")
                                      (make-anchor-row 'R 'T  "右上角对齐")
                                      (make-anchor-row 'L 'C  "左中对齐")
                                      (make-anchor-row 'C 'C  "中心对齐")
                                      (make-anchor-row 'R 'C  "右中对齐")
                                      (make-anchor-row 'L 'B  "左下角对齐")
                                      (make-anchor-row 'C 'B  "中下对齐")
                                      (make-anchor-row 'R 'B  "右下角对齐"))]))))
