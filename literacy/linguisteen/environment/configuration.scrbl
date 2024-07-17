#lang scribble/manual

@(require "../literacy.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{教学配置}

@(define figure-scale 0.20)

初学编程最麻烦的地方不来自于编程本身，
而是要配置好各种编程软件让你自己能顺利学习，
有时候这个过程真的挺让人挫败的。

@emph{本节专业性较强，
 如果家里没有专业人士协助，
 你可能要花很长时间才能自己摸索出来。}

@handbook-scenario[#:tag "env-conf"]{配置环境变量 @litchar{Path}}

@centered{@emph{环境变量对于软件工程师来说很重要，请务必熟练掌握本节内容。}}

环境变量是操作系统推送给每一个应用程序的特殊变量，
用以告诉应用程序自己正运行在什么环境里，
比如程序是运行在 Windows 里还是在 macOS 里、
启动自己的用户是谁，等等。
其中最重要的是，
程序知道要去哪里找别的程序以便在需要的时候协作完成一项任务。

命令提示符（cmd.exe）或 PowerShell 是两个最典型的程序，
用户使用它们的大部分时间都在启动、执行别的程序。
当你随便输入一个不存在的命令名字时，它们会抱怨找不到这个程序。
现在，你输入 racket 它依然还是抱怨找不到，而你明明才刚刚安装上。
这是因为还没有配置环境变量，它们不知道去哪找到 racket。

对于我们用得到的部分，cmd.exe 和 PowerShell 差不多。
但方便起见，建议大家熟练使用 PowerShell 常用命令(@secref{$shell:com_cmd})。

接下来请按如下步骤配置环境变量 @litchar{Path}：

@itemlist[#:style 'compact
          @item{在 PowerShell 里输入 @exec{sysdm.cpl} 回车打开@tamer-figure-ref{sysdm}的“系统属性”窗口。
           @tamer-figure!["sysdm" "系统属性"]{@stone-image["installation/sysdm_cpl.png" #:scale figure-scale]}}

          @item{在“高级”标签页最下方有个“环境变量”，点击打开 @tamer-figure-ref{env}。
           @tamer-figure!["env" "环境变量"]{@stone-image["installation/env.png" #:scale figure-scale]}}
          
          @item{编辑下面那个“系统变量(S)”里的 @envvar{Path}，对照@tamer-figure-ref{path}，
           把目录路径 @filepath{C:\Program Files\Racket} 加进去。
           当程序需要定位其他程序时，它会按从上到下的顺序依次检查待查找的程序是否在该目录下。
           @tamer-figure!["path" "编辑环境变量"]{@stone-image["installation/path.png" #:scale 0.36]}}]

至此，关闭 PowerShell 再重新打开就可以运行 racket 命令了。
Windows 学员可以用@exec{where.exe}（后缀@exec{.exe}不能省略）命令来确定某个程序能不能被找到，
比如：

@itemlist[#:style 'compact
          @commandline{where.exe racket}]

这个命令会告诉你 racket 存放的完整路径，
如果你安装了多个版本，它也会把其他版本的路径都列出来，默认情况运行第一个。
@focus{其他系统的学生请将 @exec{where.exe} 替换成 @exec{which}。}

@handbook-action{添加与课程相关的其他路径}

以下几个路径在安装了必要软件之后就会存在(如果找不到说明你没安装相关软件)，
因此在添加的时候可以直接点@tamer-figure-ref{path}右边
的@onscreen{Browse}(@onscreen{浏览})来定位路径。

@itemlist[
 @item{C:\Program Files\Git\mingw64\bin}
 @item{C:\Program Files\Git\usr\bin}
 @item{C:\Program Files\CodeBlocks\MinGW\bin}
 @item{C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build}
 @item{C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build}
 ]

下面这个路径只能自己输入了，
现在它还不存在，
但安装它又需要先配好上面的路径。

@itemlist[
 @item{C:\opt\GYDMstem\lib}
 ]

@handbook-scenario{安装我用 Racket 写的 C++ 软件构建工具}

用我自己写的软件来教学生，肯定有家长会质疑这个做法。
初学编程真的麻烦且枯燥，学生很可能在第一个月就被各种烦人的细节劝退。
我的工具可以以最简洁的方式带学生入门，
毕竟专业的软件都不太适合中小学生直接用。
如果学生有能力参加竞赛，我们会专门培训标准工具链以应付笔试考试。

以下命令均在 PowerShell 里运行，
并且需要@emph{以管理员方式启动 PowerShell}：

@itemlist[#:style 'compact
          @commandline{raco pkg install -i --auto digimon}]

或

@itemlist[#:style 'compact
          @commandline|{git clone stem@plteen.fun:digimon C:/opt/digimon}|
          @commandline{raco pkg install -i --auto --link C:/opt/digimon}]

以上两种方法推荐第一种。
如果因为网络问题导致安装失败才换第二种方法。

@handbook-scenario{安装课程相关软件}

这部分软件是运行课程相关程序时必须要有的，
包含图片素材、头文件、动态链接库等一系列重要文件。

@itemlist[#:style 'compact
          @commandline|{git clone stem@plteen.fun:GYDMstem C:/opt/GYDMstem}|]

@handbook-scenario{安装 Pygame}

@centered{@emph{不学 Python 的同学可以跳过。}}

Pygame 是比较流行的 Python 游戏库，其内核是 SDL。

在本系列课程中，Pygame 是课程代码库和 SDL 的中间层。
也就是说，为使Python和C++课程处于同一起跑线上，
我的代码库会尽量降低对 Pygame (在SDL之上的扩展)的依赖。
因此，对于已经熟悉 Pygame 的学生，他们过往课程的经验或许有用，
但思维方式一定会有较大改变。你可能需要让学生和家长明白，
这个改变肯定是好的。C++都能变得通俗易懂，把这份功力用在
Python 上，自然是降维打击。

在 PowerShell 中。
执行以下命令安装 Pygame：

@itemlist[#:style 'compact
          @commandline{python -m pip install --pre pygame}]

Python 版本太新会导致没有新版本的可用的 Pygame，
因此要添加@DFlag{pre}选项。

本来，PySDL2 是个更好的选项，但这个库问题太多了，它的开发者和维护者都相当业余的样子。
因此重新启用 Pygame。

@handbook-scenario{同步课程源码}

课程源码就是那个在上课时以学生名字命名的文件夹里的东西，
随便复制到哪都行，建议跟上课时所在目录一样。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
