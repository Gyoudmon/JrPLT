#lang scribble/base

@require{../literacy.rkt}

@handbook-portfolio-story{Shell}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered

 @item{了解操作系统、程序语言和应用程序}
 
 @item{了解文件路径和网络地址}
 
 @item{知道 Shell 既是一种操作计算机的方式，也是一种程序语言}
 
 @item{使用常用 shell 命令管理项目目录，完成后续课程必要的教学配置}]

@handbook-scenario{知识点}

@handbook-action{基础知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Programming Language"]{程序语言}是人们驱动计算机完成任务的语言。}

 @item{@handbook-defterm[#:origin "Program"]{程序}是一段用程序语言说的话。}
 
 @item{现在的家用计算机都遵循“存储程序”思想。
  程序写出来保存在外存(硬盘)里，程序运行时要加载到内存里。
  因此@handbook-defterm{位置}是个基础且重要的概念。}]

@handbook-action{重点知识}

@itemlist[
 #:style 'ordered

 @item{适合软件工程师操作计算机的方式，除了图形界面，还有命令行界面。
  提供命令行界面的程序称为@handbook-defterm[#:origin "Shell"]{壳}，
  通常它们也是一类程序语言，称为脚本语言。
  
  @handbook-defterm[#:origin "Command Line"]{命令行}界面的特点是，
  用户通过键盘操作计算机，每次输入一行，按回车键后计算机开始执行刚刚输入的行，
  并把输出结果显示在屏幕上。}

 @item{Windows 启动 PowerShell 快捷做法：@hotkeys['win #\R]，输入 @:mod{PowerShell}。}]

@handbook-action[#:tag "$shell:path"]{难点知识}

@itemlist[
 #:style 'ordered
 
 @item{@handbook-defterm[#:origin "Drive Specifier"]{盘符} 是 Windows 硬盘分区的符号，
  写作@litchar{英文字母:}。比如 @litchar{C:}、@litchar{D:}。注意那个冒号(@litchar{:})不能漏。}

 @item{@handbook-defterm[#:origin "Path"]{路径}用于唯一定位一个资源(目录或文件)。
  资源通常以@emph{树形结构}组织，不同层级之间用@litchar{/}或@litchar{\}分隔。

  @itemlist[
   @item{在 Windows 系统里至少有一个(树)根，对应着各个硬盘，
    因此 Windows 的@tech{路径}通常会跟在@tech{盘符}后面。
    比如：@litchar{D:\name\basis}、@litchar{G:Laboratory}。}
    
   @item{在 macOS、Linux 等 Unix 系统里只有一个(树)根，
    因此 Unix 的@tech{路径}之前什么都不加。
    比如：@litchar{/home/name/Laboratory}。}]}
  
 @item{@handbook-defterm[#:origin "Current Directory"]{当前目录}指的是应用程序运行时所关联的目录。
  所有的应用程序在启动时都会关联一个@tech{当前目录}。
  比如，无论是直接图形界面登陆，还是@tech{命令行}远程登陆，
  你进入系统后的@tech{当前目录}都是你自己的@handbook-defterm[#:origin "HOME"]{首目录}。
  桌面上显示的文件和文件夹都在你@tech{首目录}的@tt{Desktop}子目录里。}

 @item{@handbook-defterm[#:origin "Absolute Path"]{绝对路径}指从根开始的一条完整路径。
  @tech{绝对路径}一定会以@litchar{/}或@litchar{\}开始。}
 
 @item{@handbook-defterm[#:origin "Relative Path"]{相对路径}指的是相对@tech{当前目录}的路径。
  @tech{相对路径}不以@litchar{/}或@litchar{\}开始，
  但@tech{相对路径}被用于定位时会被转化为@tech{绝对路径}。}]

@handbook-scenario[#:tag "$shell:com_cmd"]{常用 Shell 命令}

@(define make-shell-row
   (lambda [shell arglist semantics helper]
     (define cmd (symbol->string shell))
     (define args (map smaller arglist))

     (list @commandline[(add-between (cons (exec cmd) args) @hspace[1])]
           @smaller[semantics]
           @smaller[helper])))

@tabular[
 #:style 'boxed
 #:sep @hspace[2]
 #:column-properties '(left)
 #:row-properties '((top-border bottom-border))

 (list (list           @commandline{@emph{命令}}      @emph{语义}                        @emph{助记单词})
       (make-shell-row 'cd        '("目录路径")       "切换“当前工作目录”的路径"            @tt{@litchar{c}hange @litchar{d}irectory})
       (make-shell-row 'ls        '("[目录路径]")     "列出目录的内容"                     @tt{@litchar{l}i@litchar{s}t directory contents})
       (make-shell-row 'mkdir     '("目录路径")       "在指定路径创建目录"                 @tt{@litchar{m}a@litchar{k}e @litchar{dir}ectory})
       (make-shell-row 'rmdir     '("目录路径")       "删除指定路径的目录"                 @tt{@litchar{r}e@litchar{m}ove @litchar{dir}ectory})
       (make-shell-row 'mv        '("源路径" "新路径") "移动文件或目录"                    @tt{@litchar{m}o@litchar{v}e path})
       (make-shell-row 'cp        '("源路径" "新路径") "复制文件或目录"                    @tt{@litchar{c}o@litchar{p}y path})
       (make-shell-row 'pwd       '()                "打印“当前工作目录”的路径"            @tt{@litchar{p}rint @litchar{w}orking @litchar{d}irectory})
       (make-shell-row 'cat       '("文件路径")       "按顺序将指定文件的内容打印在屏幕上"    @tt{con@litchar{cat}enate and print files})
       (make-shell-row 'which     '("命令名")         "打印“命令名”对应的实际路径(Unix)"    @tt{-})
       (make-shell-row 'where.exe '("命令名")         "打印“命令名”对应的实际路径(Windows)" @tt{-}))]

@handbook-reference[]
