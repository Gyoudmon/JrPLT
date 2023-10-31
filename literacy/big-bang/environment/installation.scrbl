#lang scribble/manual

@(require "../literacy.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{安装必要软件}

@(define figure-scale 0.20)

@handbook-scenario{Racket}

Racket 是 Lisp 语系的方言，
而 Lisp 是最古老的两大高级程序语言之一。
现在大多数人听说过的语言(包括 Python)的很多设计思路，
基本都是 Racket 先有，然后被其他语言的作者抄到自己的语言里去。

Racket 是我最常用的语言，它是现代 Lisp 最前卫的方言之一，
也是“程序语言理论”领域最专业的语言(没有之一)，它老本行碰巧又是编程教育。
如果把所有语言按顺序排列成光谱，
那 C 和 C++ 代表的就是较底层的机器端；
Racket 则是与之相对的数学端，
且对数学理论的追求最为优雅和纯粹。

Racket 在本系列课程中充当 C++ 构建工具，
一行简短的命令搞定所有恼人的编译细节，
特别是解决了“一个项目只能有一个main”问题。

安装步骤简单，
去@hyperlink["https://racket-lang.org/download/"]{官方网站}下载跟你系统一致的版本、
双击安装即可。

@handbook-scenario{git}

git 是目前应用最广泛的版本管理工具。
在本系列课程中，
git 既用于管理我们自己的代码库，
也用于管理和安装依赖的第三方库。

安装过程不算复杂，
去@hyperlink["https://git-scm.com/download/win"]{官方网站}下载自己系统的版本。

@handbook-scenario{vs_BuildTools}

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

@handbook-scenario{Visual Studio Code}

Visual Studio Code 也是微软推出的产品，但它主要职能是“编辑”代码，功能比 Visual Studio 少很多。
对学生而言 Visual Studio Code 足够轻便，也有利于他们分清楚“编辑”和“编译”两个过程。

它的安装更简单，还是去同一个@hyperlink["https://visualstudio.microsoft.com/downloads/"]{页面}找到那个大大的
Visual Studio Code，
然后下载安装一气呵成。

Visual Studio Code 是开源软件，别人可以一起贡献代码，因而安装了合适的扩展就可以编辑所有语言的代码了，
且字体、配色、操作风格一致，这一点 Visual Studio 是做不到的。
当你打开一个明显是代码的文件时，它会提醒你安装相应语言的扩展，然后就可以愉快地编码了。

@handbook-scenario{Python}

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

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
