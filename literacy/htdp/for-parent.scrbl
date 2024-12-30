#lang scribble/base

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story[#:tag "parent-how-to"]{家长如何在课后帮助孩子学习编程？}

理想情况下，
家长最好跟孩子一起学习，
这是唯一有效帮助孩子规划课后学习时间的前提。
因为，编程这项能力足够新颖和专业，
成年人也没有办法光靠“看”网上的评论就真的做到理解编程。

非理想情况下，
家长可以从以下角度提供辅助。

@handbook-scenario{学习态度}

编程是多门综合性较强的学科，
目前仅在少部分地区才会算在中考或高考的总分里。
这说明，这门课程并非不重要，
而是实际执行中尚有很多问题难以或无法解决。
比如：大部分中小学学校里没有足够的机房；
大部分家庭也没有条件随随便便就给孩子买电脑。

更直接的原因是，它对学生的知识贮备要求很高，
不仅仅是大家听得最多的数学基础，
语文基础、英语基础、科学素养也同等重要。
因此，学编程之前，孩子应该尽早知道“偏科不可取”；
决定要学编程之后，也应该尽早认真对待，
切不可抱着“可学可不学”的心态玩闹。

@handbook-action{学科交叉}

鉴于目前应试教育的强势和小学生学习编程的客观难度，
不能解决中学生的学业困境，
那青少年计算机科学课的价值就发挥不出来。

编程是中小学阶段唯一一门正真意义上的综合课程，而且是硬核综合。
在学术圈，交叉研究做的都是涉及领域的交集。
但是对于青少年，他们各方面的知识积累和素养都不够充分，
以至于即使只涉及到交集部分，他们也还是力不从心。
偏科的学生问题更突出。

@itemlist[
 #:style 'compact
 
 @item{@emph{语文}，母语语言能力。
  编程是一种特殊的写作题材，有着独特的格式习惯和要求。
  事实上，给义务教育阶段的普通学生拿一张 A4 纸，他们都很可能不知道从哪下笔。
  因为纸上没有划线，也从未有人跟他们说过“页边留白”很重要。}
 
 @item{@emph{数学}，科学语言能力。
  这位的重要性地球人都知道，具体到青少年编程课上，与之关系最密切的是代数思维和数学建模能力。
  如果有志于比赛，那必须也要包含中学数学、大学离散数学等诸多分支。}
 
 @item{@emph{英语}，第二语言吸收能力。
  这位第一次在学校教育中告诉学生“语法”为何物，也是第一次在学生的大脑中悄无声息地建立语言的文法结构。
  @focus{语法的地位很微妙，这是青少年搞定电脑、可以开始坐下来敲代码的第一道坎。}
  课程精心设计，概念都不难理解，但是理解概念只是第一步，好多学生卡在按语法规则组织概念上。
  比如，把函数三要素写成“@:sym{返回值类型 函数名(参数列表)}”的形式;
  或者给出一个很简单的函数声明，
  就是有学生死活读不出任何信息。
  学生需要培养“@emph{分离语法和语义}”的能力。}

 @item{@emph{物理}，本地宇宙的底层逻辑。
  电子游戏的本质是用数学定义的虚拟宇宙，畏难的孩子也会因洞悉到“复杂现象的底层原理原来如此简单”而欢呼雀跃。}

 @item{@emph{生物}，本地宇宙中的最强智能体。
  编程很抽象，对初学者很不友好。
  但编程概念，除了从其他学科中提炼，一定也能在生活中找到更具体的原型。
  生物课会系统告诉学生这些原型的工作原理。}
]

物理和生物代表的是自然科学的两个端点，对与它们的关联的描述也适用于其他理工科。
道阻且长。

@handbook-scenario{学习习惯}

打算学编程的孩子至少要准备好：

@itemlist[
 @item{个人电脑(Windows 10+, macOS, Linux)，平板不够学编程用。}
 @item{包含基本文具的文具盒。编程不只是敲键盘，纸笔构思必不可少。}
 @item{文件夹或文件袋，用于归档老师打印的资料和习题。}
 @item{一本厚厚的笔记本作为知识便签薄(不是日记本)，既可以用来记笔记，也可以用来记录孩子从任何地方发现的有趣的@emph{知识}。}
]

以上几项物品，
在自己家应当放在一起，方便复习和练习；
上课时要一起带着。

学校教育的课程安排比较紧凑，
学生有充分的被动学习机会。
直白点说，课程表在设计上就暗合了记忆的遗忘规律。
因此，家长应当帮助孩子规划复习时间，
至少要让孩子有时间复述一两次上节课的内容。
一定不能放任孩子上完课就不管不顾，
否则很快就会忘记，然后一直觉得“编程好难啊”。

@handbook-scenario{编码训练}

标点符号是程序语言语法的重头，必须全部正确。
因此初学者会有一段不短的适应期，
而且需要有懂行的人在一旁盯着才能提高纠错效率。

初学者经常不知道自己输入的代码哪里有问题，
如果家长想帮忙，可以从以下几条线索提醒孩子纠错：

@itemlist[
 @item{程序语言只认英文标点符号，
  因此需要重点关照那些看起来差别不大的标点符号(逗号、冒号、分号等)。
  输入代码的软件通常会在中文标点处画个框提醒用户“这里有笔误”。}

 @item{注意单词大小写，养成区分大小写的习惯。比如 A 和 a 不能混用。}
 @item{注意看起来相似度较高的字母和数字。比如 l 和 1，O 和 0 等。}
 @item{注意特殊符号，比如星号(@litchar{*})、井号(@litchar{#})等。
  下划线(@litchar{_})在代码中比较常用，也可能连续出现两个或更多个。}

 @item{如果是 Python 代码，每行代码开头的空格数量也有很无理的说法。
  如果这方面出问题，需与老师给的参考程序比对，确保完全一致。}
]

@handbook-scenario{作业}

理论上说，没有计算机也能学编程，
但那样就退化成常规的学校教育了，
对孩子来说效果会大打折扣。
只教方法而不教配套的实施技术，
或者只教技术而不讲原理方法，
都不靠谱。

如果家长能接受，今后我会给相关的孩子留作业，并附上参考答案。
家长不必非得理解答案，在帮助孩子检查作业的过程中，
说不定会意外破除对编程的神秘感，
进而萌发“我也学一下好了”的想法。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
