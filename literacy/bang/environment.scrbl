#lang scribble/manual

@require{literacy.rkt}

@require{diagram/setup.rkt}

@(require geofun/vector)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{开发工具和运行环境}
@handbook-word-count[]

本系统既是一个软件系统，也是一个教学系统，两者深度融合，不分彼此。

每一个学生在学习的准备阶段就是要在自己的计算机上正确安装开发软件、配套的课程源码并配置好，
其详细流程见@fig-ref{setup.dia}。

入学流程第一步是让学生“自备个人电脑”，
这其实隐含表达了当下少儿编程市场的一个大问题。
因为教学内容套路化——找个成熟题库按部就班刷题就是了，
线下编程机构普遍自己提供廉价计算机供学生使用，
进而导致家长也普遍觉得这个现象很合理。
这在学校尚有可行之处，
因为校队队员通常目标明确，
且天天有机会去机房练习。
其他同学，如果没有自己的电脑，
那他们没有任何途径能把所学知识带回去，
更不要说主动复习和练习了。
在正常学期中，一周一次课，
没有自备电脑、
或虽有电脑但不练习的学生普遍进度缓慢，
甚至没有进度。
因此我不建议不能自备电脑的学生系统学编程，
不如专心把时间分配给课内科目。

接下来的流程步骤均依赖终端软件，
Windows 10 及更高版本的默认安装就能胜任。 

@tamer-figure!['setup.dia "入学第一课流程图"]{@(geo-scale setup.dia 0.42)}

@handbook-scenario{安装开发工具}
@handbook-word-count[]

@handbook-action{Racket}

Racket@$cite[plt-tr1] 起源于游戏化的青少年编程教育@$cite{RoR}，
在此需求之上从 PLT Scheme 发展成为了一个独立又前卫的
Lisp 方言@handbook-footnote{Lisp 是最古老的两大高级程序语言之一,
 PLT Scheme 又是 Lisp 语系知名度最高的两大方言之一。}，
并且持续给程序语言设计领域输出新思想和新技术。

在本系统中，
我用 Racket 编写的构建工具可以帮助学生隐藏编译、测试 C++ 程序和项目时的所有恼人细节。
对于学有余力的学生，Racket也是一个帮助他们打开学科眼界的窗口。

此外，我用 Racket 编写的图形程序可以方便快捷的生成教学相关的可视化素材。

@handbook-action{C++ 工具链}

Windows、macOS、Linux 三大操作系统都有自己专属的C++工具链软件。

Linux 安装 gcc、macOS 安装 XCode 没有异议；
在 Windows 系统下，相对于安装巨无霸的 Visual Studio，只安装
vs_BuildTools@handbook-footnote{只需勾选 Desktop Developlement with C++。}可以节省大量硬盘空间，
这对于青少年学生来说极其必要，因为他们通常只能用家长淘汰下来的旧电脑。

@handbook-action{Visual Studio Code}

Visual Studio Code(以下简称为 VSCode)是微软推出的“可扩展编辑器”，其主要职能是编辑代码。
VSCode 对学生而言足够轻便，与本文推荐的其他软件配合可显著减少学生入门过程中的痛苦程度。

@handbook-action{git}

git 是目前应用最广泛的版本控制软件之一，
在本系统中，它主要用于帮助学生从教师机同步课程源码。
以及，课程后期学生之间、师生之间团队协作做项目用。

@handbook-action{其他}

以上软件运行本系统就已经够用了，但在实际的教学实施中，
合理使用如下软件可以提升教学过程的流畅度。

@handbook-itemlist[
 #:style 'compact

 @item{Python凭借其“容易上手”的特点晋升为用户数量最多的语言。
  对于青少年中的初学者，尤其是小学生群体，C++语言的文法是道砍，
  因此在有能力学编程的学生中，确实也存在少部分不能直接学 C++ 的孩子，
  他们需要 Python 来过渡。
  此外，Python 在我的课程设置中也是C++的对比学习工具。}

 @item{Code::Blocks 是信息学奥林匹克竞赛指定的 Linux 环境下的刷题软件，
  Windows 学生也需要它附带的 Mingw 环境。
  对于有志于竞赛的学生，
  安装这个可以获得一个与实际比赛一致的刷题环境。}]

@handbook-scenario{配置环境变量}
@handbook-word-count[]

如何在孩子们对编程毫无概念的情况下一把吸引住他们的注意力？
答案是从 Shell 开始入门，在等待上述软件安装的过程中，
我会带学生认识 Shell，
让他们自己通过“输入一系列奇怪的单词控制计算机完成对文件和文件夹的操作”来
体验计算机语言的魅力，
孩子们因此产生的惊讶和兴奋通常不亚于初次见到酷炫的化学反应。

这个过程可以顺便给学生普及必要的基础知识，
比如文件路径、语言文法、可执行程序、输入与输出等。
然后自然而然过渡到对“环境变量”的配置，
本系统的运行需要将以下几个路径加入到环境变量 @envvar{PATH} 中：

@handbook-itemlist[
 @item{C:\Program Files\Racket}
 @item{C:\Program Files\Git\mingw64\bin}
 @item{C:\Program Files\Git\usr\bin}
 @item{C:\Program Files\CodeBlocks\MinGW\bin}
 @item{C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build}
 @item{C:\opt\JrPLT\lib}
 ]

@handbook-scenario{部署系统}
@handbook-word-count[]

部署系统的过程可以脚本化，
但在实际教学实施中仍然是我带着学生一起做。
主要目的有以下几点：

@handbook-itemlist[
 #:style 'compact
 
 @item{给学生普及“版本控制”的基本概念，
  从一开始就给学生营造一个团队协作的氛围。
  在后续学习过程中养成“进教室”先更新的习惯。}
 
 @item{给学生普及远程登陆的基本常识，
  让学生体验一把当黑客的感觉。}

 @item{观察学生对特殊字符是否敏感，
  有没有耐心应对 C++ 这样的纯文本编程语言。}
 ]

@handbook-action{部署构建工具}

digimon 是我用 Racket 编写的工具箱软件，内含C++项目构建工具和测试系统。

在配置好Racket环境变量之后，
可将本文配套源码中的 digimon 目录复制到 @filepath{C:\opt\}，
然后执行以下命令安装：

@handbook-action{部署系统运行时库和游戏资源}

JrPLT 是本系统的运行时库和预设资源。

JrPLT 也包含在本文配套的源码中，
将同名文件夹复制到 @filepath{C:\opt\} 即可。
其 lib 子目录已在上述步骤中配置到环境变量中了(最后一条路径)。

对于 Linux 和 macOS 学生，
借助系统自带的包管理工具可以很方便的安装 SDL2，
这是本系统依赖的底层图形库。
对于 Windows 学生，
安装 SDL2 比较麻烦，
因此它和它依赖的动态链接库都已经包含在 JrPLT 中了，
无需额外安装。

@handbook-action{部署系统}

big-bang 是本游戏引擎的源码，
在实际授课中一般不提供给学生，
由教师编译好并包含在 JrPLT 中一起分发给学生。

@handbook-action{部署课程源码}

课程源码既包含教师演示程序，也包含学生的课程项目，但不包含本系统的引擎源码。


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
