#lang scribble/base

@require{../literacy.rkt}

@handbook-portfolio-story{git}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered

 @item{了解版本控制的基本概念}
 
 @item{认识远程地址格式}
 
 @item{使用常用 git 命令从教师机下载、同步源码}]

@handbook-scenario[#:tag "$git:com_cmd"]{常用 git 命令}

@(define make-shell-row
   (lambda [shell arglist semantics]
     (define cmd (string-append "git " (symbol->string shell)))
     (define args (map smaller arglist))

     (list @commandline[(add-between (cons (exec cmd) args) @hspace[1])]
           @smaller[semantics])))

@tabular[
 #:style 'boxed
 #:sep @hspace[2]
 #:column-properties '(left)
 #:row-properties '((top-border bottom-border))

 (list (list           @commandline{@emph{命令}}         @emph{语义})
       (make-shell-row 'clone     '("远程地址" "本地地址") "将“远程地址”的源码库下载到“本地地址”")
       (make-shell-row 'pull      '()                    "从“远程地址”同步源码")
       (make-shell-row 'diff      '()                    "查看源码变更的具体内容"))]

@handbook-reference[]
