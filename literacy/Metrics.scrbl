#lang scribble/sigplan

@(require "literacy.rkt")
@(require "share/graphviz.rkt")
@(require "share/git-ring.rkt")

@handbook-statistics[#:gitstat-width 450 #:gitstat-radius 80
 #:altcolors '(["Racket" . Green] ["Python" . Khaki])
 git-size-ring-chart git-loc-time-series]
