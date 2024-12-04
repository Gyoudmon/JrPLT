#lang scribble/sigplan

@(require "literacy.rkt")
@(require "share/graphviz.rkt")
@(require "share/git-ring.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-statistics[#:gitstat-width 460 #:gitstat-radius 75
 #:altcolors '(["Racket" . Green] ["Python" . Khaki])
 git-size-ring-chart git-loc-time-series]
