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
该图以 Windows 为范本，
实际教学中使用 macOS 和 Linux 的学生极少，
碰到了可单独指导。

@tamer-figure![
 'setup.dia
 @list{入学第一课流程图}]{
 @(geo-scale setup.dia 0.42)
}

入学流程第一步是让学生“自备个人电脑”，
这其实隐含表达了当下少儿编程市场的一个大问题。
因为教学规模化、内容套路化——找个成熟题库按部就班刷题就是了，
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
这些步骤都可以脚本化，
但在实际教学实践中仍然是我带着学生一起做。

传统的编程入门课喜欢带学生写一个程序在终端输出“Hello, World!”，
但是这种做法仅仅只是一种象征性的文化惯性，
并不会提供任何有价值的教学效果。
当代孩子入门编程的年龄越来越低，
莫说对古早的计算机文化没有代入感，
甚至缺乏必要的操作计算机的信息技术常识。
因此，相对于让孩子们满脸困惑地跟着老师敲键盘，
不如从认识 Shell 开始。
通过“输入一系列奇怪的单词控制计算机完成对文件和文件夹的操作”来立竿见影地体验一把当黑客的感觉，
孩子们因此产生的惊讶和兴奋通常不亚于初次见到酷炫的化学反应。

除了准备好学习环境，这个过程还包括如下额外的教学任务：

@handbook-itemlist[
 #:style 'ordered
 
 @item{普及必要的基础知识，
  包括文件路径、语言文法、可执行程序、输入输出、远程密码登录，等等。}

 @item{普及“版本控制”的基本概念，
  从一开始就给学生营造一个团队协作的氛围，
  并在后续学习过程中养成“进教室先更新”的习惯，
  以此强化对“文件路径”概念的记忆和理解。}

 @item{观察学生对英文单词、特殊字符是否敏感，
  有没有耐心应对 C++ 这样的纯文本编程语言。}
 ]

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

此外，我用 Racket 编写的图形程序可以方便快捷的生成教学相关的可视化素材，
包括本文中使用的各类图表。

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

以上软件运行本系统就已经够用了，
但在实际的教学实践中，
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

部署过程中用到的 Shell 命令以标签形式呈现在@fig-ref{setup.dia}的流程中。
其中，“[host]”是教师机地址，位于局域网或云服务器中；
“[name]”是学生自己名字的拼音，
实际教学中就固定设置在硬盘根目录下。
因为学生和家长普遍都没有文件管理意识，
让学生家庭自己安排文件夹，这个任务就无限搁置了。

@handbook-action{部署辅助系统}

辅助系统安装在“C:\opt”中，分属两个 Racket 软件包中：

@handbook-itemlist[
 #:style 'ordered

 @item{digimon 包含本系统的构建工具和测试系统}
 @item{graphics 包含本系统相关的可视化素材生成工具}
 ]

@handbook-action{部署课程系统运行时库和资源库}

JrPLT 是本系统的运行时库和预设资源（但不包括源码），
也安装在“C:\opt”目录下。

本系统的运行还依赖 SDL2。
对于使用 Linux 和 macOS 学生，
借助系统自带的包管理工具可以很方便的安装。
对于 Windows 学生，
SDL2 及其依赖的动态链接库都已经包含在 JrPLT 中了，
无需再额外安装。

@handbook-action{部署课程源码}

课程源码克隆到学生自己的“工作目录”中，包含两个源码库：

@handbook-itemlist[
 #:style 'ordered

 @item{noi 包含传统方式学习用的源码}
 @item{pbl 包含项目制学习用的源码}
 ]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
