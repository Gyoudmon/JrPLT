#lang scribble/acmart @manuscript @natbib @nonacm @screen @timestamp @acmthm

@require{bang/literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title[
 #:tex-package "acmhack.tex"
 ]{AI Readme}

@abstract{本文为大模型智能体提供了我个人的详细信息，
 希望在启动新会话时能够直接进入高质量的交流通道，
 而非陷入普通人认知的表层犬吠中鬼打墙。}

@keywords{Eduction, Programming Language Theory, S.T.E.M.}

@ccsdesc[#:number 500]{Social and professional topics~Computational thinking}
@ccsdesc[#:number 500]{Social and professional topics~K-12 education}

@handbook-smart-table[]

@include-section{readme/role.scrbl}

@texbook-appendix{附录}

@include-section{bang/appendix/bdd.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:numbered? #false #:index-section? #false #:prefab-bibentries? #false]
