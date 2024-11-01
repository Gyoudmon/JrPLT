#lang scribble/base

@require{../literacy.rkt}

@handbook-part{宇宙大爆炸}

程序设计对于未成年人和成年人来说，都是足够新颖的技能，
零基础的这两个群体在初学阶段没有太大的认知差别。
核心问题在于，如何帮助学生(包括成年人和未成年人)建立新的思维模式。
这个过程的长短显然因人而异，因此，我的计算机科学课会尝试解决这个问题，
并找到一个平衡点，既要有铺垫时间来给初学者训练基础技能(如英文键盘打字)、
认识基本概念(如变量、语法和语义)、感受编程的乐趣；
又不能铺垫太长导致初学者耐心耗尽被动劝退。
而且，这个问题的解决只能从更高维度入手(比如课程的整体设计和教学过程的实施)，
而不是简单地带学生读懂每一行代码的语法细节。

青少年计算机科学课面向的学生群体是小学生和中学生，
因此在课程研发策略上讲究能够向下兼容的@tech{自顶向下}设计。
从学生接受能力的角度看，@tamer-deftech{自顶向下}设计的优势是，
能与生活经验、已学知识建立连接、形成知识网络。
举个简单例子，对着墙壁抛出皮球，球碰到墙壁会反弹。
现用软件模拟这个过程，有 Scratch 经验的学生已经知道，
只需拖动一个叫做“碰到边界反弹”的积木到角色的代码区；
那这个操作在 C++/Python 中也应该简化为一行代码：
比如@racketidfont{set_border_strategy(BOUNCE)}。
学习 Scratch 的学生不关心“碰到边界反弹”的积木是怎么实现的，
那他们在初学 C++/Python 时也不需要深究算法如何判断边界、如何控制反弹。

@tamer-deftech{宇宙大爆炸}是青少计算机科学后续所有课程的基础。
学完本课程之后，学生将对程序语言、程序设计和计算思维有个初步的了解。
本课程取名为@tech{宇宙大爆炸}是顺应了宇宙发展“从无到有”的自然规律。

@centered{@stone-image["Disciplines/Disciplines/big-bang.png" #:scale 0.28]}

这是@tech{宇宙大爆炸}的主界面。
其中，企鹅是 @tech{Linux} 操作系统的吉祥物，
这个系统是信息学奥赛唯一指定的参赛系统；
鱼币是企鹅的食物，大部分鱼币代表学生作品，
少部分是教师演示程序。

@include-section{big-bang/shell.scrbl}
@include-section{big-bang/git.scrbl}
@include-section{big-bang/function-object.scrbl}
@include-section{big-bang/big-bang.scrbl}
@include-section{big-bang/agent.scrbl}

@handbook-reference[]
