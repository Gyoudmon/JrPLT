#lang typed/racket/base

(provide (all-defined-out))

(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define arch-tree-hwidth 0.75)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! architecture.dia
  #:parameterize ([default-diaflow-block-width 150.0]
                  [default-diaflow-arrow-target-shape #false]
                  [default-diaflow-free-track-dash 'solid])
  #:start 'JrPLT [#:background 'White] #:-
  (move-down 0.75 '.plteen)
  (move-left 2.50)
  (move-down 0.75 (string->symbol "Graphics\n(Drawing Context)"))
  (move-down 1.00 '#:.GA)
  (move-down 1.00 '#:.GB)
  (move-down 1.00)
  (move-left arch-tree-hwidth 'Texture)
  (jump-back) (move-left arch-tree-hwidth 'Image)
  (jump-back) (move-left arch-tree-hwidth 'Font)
  
  (jump-back '.plteen)
  (move-right 2.5)
  (move-down 0.75 'Physics)
  (move-down 1.00 '#:.PA)
  (move-down 1.00 '#:.PB)
  (move-down 1.00 '#:.PC)
  (move-down 1.00)
  (move-right arch-tree-hwidth 'Motion)
  (jump-back) (move-right arch-tree-hwidth '|Color Space|)
  (jump-back) (move-right arch-tree-hwidth 'Geometry)
  (jump-back) (move-right arch-tree-hwidth 'Algebra)
  
  (jump-back '.plteen)
  (move-left 0.75)
  (move-down 0.75 'Virtualization)
  (move-down 1.00 '#:.VA)
  (move-down 1.00 '#:.VB)
  (move-down 1.00 '#:.VC)
  (move-down 1.00 '#:.VD)
  (move-down 1.00)
  (move-left arch-tree-hwidth (string->symbol "Wormhole\n(UDP)"))
  (move-left 1.25 'ASN.1)
  (jump-back) (move-left arch-tree-hwidth (string->symbol "Filesystem\nResource"))
  (jump-back) (move-left arch-tree-hwidth 'Position)
  (jump-back) (move-left arch-tree-hwidth 'Screen)
  (jump-back) (move-left arch-tree-hwidth 'Display)

  (jump-back '.plteen)
  (move-right 0.75)
  (move-down 0.75 '|User Interface|)
  (move-down 1.00 '#:.UA)
  (move-down 1.00 '#:.UB)
  (move-down 1.00 '#:.UC)
  (move-down 1.00 (string->symbol "Matter\n(Visual Object)"))
  (jump-back) (move-right arch-tree-hwidth 'Layout)
  (jump-back) (move-right arch-tree-hwidth 'Interaction)
  (jump-back) (move-right arch-tree-hwidth 'Timeline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  architecture.dia)
