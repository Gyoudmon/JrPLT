#lang scribble/sigplan

@(require "literacy.rkt")
@(require "share/graphviz.rkt")
@(require "share/git-ring.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@; c20f4a0b760302af32811f3713f3493e23fca441
@handbook-statistics[#:gitstat-width 450 #:gitstat-radius 80
 #:altcolors '(["Racket" . Green] ["Python" . Khaki])
 git-size-ring-chart git-loc-time-series]
