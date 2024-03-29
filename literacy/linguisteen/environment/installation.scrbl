#lang scribble/manual

@(require "../literacy.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{安装软件}

@(define figure-scale 0.20)

@tamer-figure!["cpp-apps" "C++ 课程建议软件"]{@graphviz[(digimon-path 'stone "Disciplines/software.gv") #:size 4.2]}

本章推荐的软件均为学生学习所需。
教师可根据需要自己安装其他软件，比如“小熊猫C++”、各类在线测评(OJ)系统,
这类软件对教师最大的用处是筛题方便(它们专为比赛而生，题库丰富，但不见得对学生具有普适性)。
但是否要让学生直接使用需要考虑一个问题：
@emph{配色方案、操作习惯、鼠标提示等方面的差异都可能会导致学生困惑。
 我的课件、学生手册中的代码截图均出自我推荐的软件，
 这些截图显然没有必要为每一种软件都准备一份。}

@handbook-scenario{必备软件}

本小节列出的软件所有学生都要安装。

有了这些软件，
学生只需要花费1-2次课的学习成本就能显著减少后续学习过程中的痛苦程度。

@handbook-action{Racket}

Racket 是 Lisp 语系的方言，
而 Lisp 是最古老的两大高级程序语言之一。
现在大多数人听说过的语言(包括 Python)的很多设计思路，
基本都是 Racket 先有，然后被其他语言的作者抄到自己的语言里去。

Racket 是我最常用的语言，它是现代 Lisp 最前卫的方言之一，
也是“程序语言理论”领域最专业的语言(没有之一)，
它老本行碰巧又是编程教育。

为降低初学者的入门痛苦程度，
我自己用 Racket 写了很多趁手的工具来帮助学生学习。
Racket 在本系列课程中充当 C++ 构建、测试工具，
一行简短的命令搞定所有恼人的编译细节。

安装步骤简单，
去@hyperlink["https://racket-lang.org/download/"]{官方网站}下载跟你系统一致的版本、
双击安装即可。

@handbook-action{Visual Studio Code}

Visual Studio Code 是微软推出的“可扩展编辑器”。
其主要职能是“编辑”代码，功能比 Visual Studio 少很多。
对学生而言 Visual Studio Code 足够轻便，
但仍然需要额外安装其他编译器工具链才能正常“运行”或“调试”程序。

Visual Studio Code 是开源软件，别人可以一起贡献代码，因而安装了合适的扩展就可以编辑相应语言的代码了，
当你打开一个明显是代码的文件时，它会提醒你安装相应语言的扩展，然后就可以愉快地编码了。

这是我推荐安装 Visual Studio Code 作为代码编辑器的主要原因：

@itemlist[
 @item{最直接的原因，它可以直接配置成用我写的辅助工具来帮助学生编译、测试程序。}
  
 @item{编写 C++/Python 或其他语言时，字体、配色、操作风格一致。
  这一点对学生来说很重要，尤其是配色，在学生的认知里尚未将颜色与语法建立关联。
  在同一班级、乃至在我们所有学生里混用不同的编辑器，
  配色方案、鼠标提示、操作习惯上的差异都会导致学生困惑。
  我的课件、作业纸显然也不应该为每一种开发软件看到的效果都截个图。}
  
 @item{在帮助学生理解代码线索这个角度， Visual Studio Code 一定会与时俱进，
  专业人士都点赞的，学生也一定会受益。
  反之，专门为学生服务的开发软件就不一定了。}

 @item{界面简洁、美观。
  相比较而言，那些宣称自己为学生服务的开发软件都太老土了。
  在这一代孩子眼里，它们土得就像是游戏软件的设置界面。}
 ]

安装步骤简单，去@hyperlink["https://visualstudio.microsoft.com/downloads/"]{与Visual
 Studio相同的页面}找到那个大大的 Visual Studio Code，然后下载安装一气呵成。

@handbook-action{git}

git 是目前应用最广泛的版本管理工具。
在本系列课程中，
git 既用于管理我们自己的代码库，
也用于管理和安装依赖的第三方库。

安装过程不算复杂，
去@hyperlink["https://git-scm.com/download/win"]{官方网站}下载自己系统的版本。

@handbook-scenario{赛事班相关}

本小节列出的软件仅科技特长比赛班的学生需要安装。

@handbook-action{Code::Blocks}

@centered{@emph{不学 Python 的学生可以跳过。}}

Code::Blocks 和 Dev-C++ 都是信奥赛考纲里指明的C++开发工具。
但是 Dev-C++ 早就死了，能安装的版本对 C++ 的支持过于古旧，因此不考虑。

安装 Code::Blocks 很简单，一路“下一步”即可。
但是下载的时候一定要注意，
在@hyperlink["https://www.codeblocks.org/downloads/binaries/"]{官方下载页}，
Windows 版一定要选带@litchar{mingw-setup.exe}的文件。

论编程学习，Windows 是真不算有多好，
所以你在 Windows 里安装开发软件，它们都会自带一个刚好够用的 Linux 环境，
mingw 就是其中比较省心的一个。

不装 Code::Blocks 直接用微软的 Visual Studio 行不行？
比赛的话会有兼容性问题了，
不比赛且你的老师真的知道我在说什么，那就没问题。

@handbook-action{Python}

@centered{@emph{不学 Python 的学生也建议@focus{不要}跳过。}}

Python 是什么不需要我再强调了，地球人都知道，不过它也就“用的人多”这一个优点。

去@hyperlink["https://www.python.org/downloads/windows/"]{官方网站}下载与你的系统结构相对应的
@onscreen{installer}，现在一般都是@tt{64-bit}的。
注意，一定要是@onscreen{installer}，不能是@onscreen{embeddable package}。

@tamer-figure!["python" "安装 Python"]{@stone-image["installation/python.png" #:scale figure-scale]}

安装过程有个细节，
务必在安装界面勾选@onscreen{add python.exe to PATH}(@tamer-figure-ref{python})。
之后用 Visual Studio Code 打开任意 Python 源码文件，
按照它的提示安装相关扩展。

你也可以按自己的喜好安装其他 Python IDE，比如 PyCharm 之类的。
不在此赘述。

@handbook-scenario{计算思维课程相关}

本小节列出的软件仅计算思维班的学生需要安装，而且是必须安装，没有捷径。

@handbook-action{vs_BuildTools}

@centered{@emph{不学 C++ 的学生可以跳过。}}

@tamer-figure!["vsbt" "安装 VS Build Tools"]{@stone-image["installation/vs_buildtools.png" #:scale 0.24]}

Windows 的 Visual Studio， macOS 的 XCode 都是巨无霸软件，
因为它们包含的东西不仅仅是构建和运行程序的必要软件，
还包含一系列辅助专业软件工程师的工具软件。
对于中小学生来说，往往只能用自己家长淘汰的旧电脑，
硬盘空间是个大问题。
因此，Windows 学员可以去 @hyperlink["https://aka.ms/vs/17/release/vs_BuildTools.exe"]{隐藏连接}下载
只包含必要软件的 vs_BuildTools，可以省下不少硬盘空间。

安装时一切从简，只需勾选@onscreen{Desktop Development with C++}一项即可，
并同时去掉@tamer-figure-ref{vsbt}右侧@onscreen{optional}栏除第一项以外的所有选项。

这个软件实际上也能在@hyperlink["https://visualstudio.microsoft.com/downloads/"]{官方下载页面}找到，
只不过它默认被折叠了。
拉到最下面，点开@onscreen{Tools for Visual Studio}，
找到@onscreen{Build Tools for Visual Studio 2022}，
后面那个@onscreen{Download}按钮就是。
以后Visual Studio 主版本号升级了，
这个连接的名字也会跟着变的，
到时重新安装即可。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
