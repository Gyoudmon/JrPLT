#lang scribble/manual

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{技术栈}
@handbook-word-count[]

@handbook-scenario{同类技术方案}
@handbook-word-count[]

研发一个新的软件系统，
首先要面对的问题是“为什么不用现成的方案”？

在主流少儿编程市场上，
学生只有 Scratch、C++ 和 Python 三个选项。
对于本文尝试解决的问题，
不宜脱离以上现实问题完全另起炉灶。

@handbook-action{Unity}
@handbook-word-count[]

Unity 是功能强大的跨平台游戏开发环境及运行引擎，
而且其设计之初就解决了游戏开发领域的一大痛点：
其他游戏开发工具都太难用、开发过程太痛苦。
因此，Unity 在设计上的成功成了游戏开发领域的标杆，
有 Unity 经验的求职者因其职业习惯靠谱，
因而也更容易找到工作。

对于学习编程的青少年来说，
Unity 有两个大问题：一是它的主语言是 C#；
二是它是 3D 游戏引擎，入门门槛天然不低。

@handbook-action{Scratch}
@handbook-word-count[]

Scratch 是时下针对青少年设计的最热门的图形化创作工具，
也确实是青少年入门编程的主要选择。
以至于其衍生产品如 Mind+、
Web 端复制品 腾讯可视化游戏开发工具 等层出不穷。
这类解决方案的共同点是将具象的积木抽象成了虚拟的积木，
允许幼儿园的孩子就开始理解编程的基本概念。
如：条件、循环、事件驱动等。

不能说它的初衷不好，
但它对程序逻辑的表达能力很有限，
优秀的孩子很快就会碰到天花板。
不过，它的成功恰恰也说明了，
青少年需要一个像 Scratch 一样易用、
又能同时学习专业编程的选项。
因此，本文的技术解决方案由此产生，
将 Scratch 的虚拟积木抽象成函数和方法调用，
保留运行时的图形化呈现，
但使用代码语言替代积木编写程序创作作品、
探索现代 K-12 教育。

此外，Scratch 3.30 也开始收费了。

@handbook-action{Pygame}
@handbook-word-count[]

Pygame 是 Python 提供的游戏开发库，
建立在跨平台的多媒体开发框架 SDL 之上。

市面上确实有不少针对青少年的创意编程课以 Pygame 为授课工具。
其问题在于，那些课程没有仔细设计，
学生需要把太多精力耗费在 pygame 本身的细节上，
而无法专注于自己的目标。

事实上，为照顾不能直接学 C++ 的孩子，
本文的解决方案也包括一个简化的 Python 版本，
使得学生能够使用 pygame 学习与 C++ 学生同步的项目制课程。

@handbook-scenario{核心系统及第三方库}
@handbook-word-count[]

@handbook-action{JrPLT}
@handbook-word-count[]

JrPLT 即是本文所提技术方案的核心系统。
采用 C++17 编写，
兼容 Windows、macOS、Linux 三大操作系统。
除标准模版库(STL)、底层图形库和系统接口层外，
所有代码自含。

@handbook-action{TheBigBang}
@handbook-word-count[]

TheBigBang 是基于本系统二次开发的项目制课程。
包含教师演示系统、
学生个人项目和学生团队项目。 

@handbook-action{SDL2}
@handbook-word-count[]

SDL 是一个用于开发跨平台多媒体软件的底层框架。
它封装了图形库、人机输入设备事件、
定时器、网络接口等2D游戏引擎的基本部件，
使得我们的系统可以直接提供对跨平台的支持。

@handbook-scenario{学生用开发软件}
@handbook-word-count[]

@handbook-action{Racket}

Racket@$cite[plt-tr1] 起源于游戏化的青少年编程教育@$cite{RoR}，
在此需求之上从 PLT Scheme 发展成为了一个独立又前卫的
Lisp 方言@handbook-footnote{Lisp 是最古老的两大高级程序语言之一,
 PLT Scheme 又是 Lisp 语系知名度最高的两大方言之一。}，
并且持续给程序语言设计领域输出新思想和新技术。
在我的课程设置中，
Racket 是一个帮助学生打开学科眼界的窗口

在本文的技术方案中，
学生需安装两个我编写的 Racket 软件包：

@handbook-itemlist[
 #:style 'compact

 @item{digimon 包含构建工具和测试系统，
  可以帮助学生隐藏编译、
  测试 C++ 程序和项目时的所有恼人细节。}

 @item{graphics 包含函数式可视化工具@handbook-footnote{
   本文中的软件工程图也由该软件生成。
   }，可以帮助老师和学生生成工整的流程图、
  清晰易懂的内存快照图，等等。}
 ]

@handbook-action{C++ 工具链}

Windows、macOS、Linux 三大操作系统都有自己专属的C++工具链软件。

Linux 安装 gcc、macOS 安装 XCode 没有异议；
在 Windows 系统下，相对于安装巨无霸的 Visual Studio，
只安装 vs_BuildTools 可以节省大量硬盘空间，
这对于青少年学生来说极其必要，
因为他们通常只能用家长淘汰下来的旧电脑。

@handbook-action{Visual Studio Code}

Visual Studio Code(以下简称为 VSCode)是微软推出的“可扩展编辑器”，其主要职能是编辑代码。
VSCode 对学生而言足够轻便，与本文推荐的其他软件配合可显著减少学生入门过程中的痛苦程度。

@handbook-action{git}

git 是目前应用最广泛的版本控制软件之一，
在本文的教学实践中，
它主要用于帮助学生从教师机同步课程源码。
以及，课程后期学生之间、师生之间团队协作做项目用。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
