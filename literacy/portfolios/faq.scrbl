#lang scribble/base

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{常见疑问与解答}

@handbook-scenario{学编程会不会导致孩子沉迷数字设备？}

信息技术和数字技术已经彻底渗透到年轻一代的思维模式中，
他们的大脑结构与家长和老师这一代在“原始”世界中长大的成年人已经不一样了。

家长和老师无法让孩子拒绝数字设备，
因为这些设备已经是生活的一部分，
并且代表了未来的发展方向。

家长和老师的重要工作之一是引导孩子创造性地使用数字设备，
以其为工具，锻炼思维能力，为将来的工作和生活打下坚实的基础。
让孩子了解数字设备工作的机理，
走出对数字设备的迷恋，
转而成为数字设备的掌控者，
这也是学习编程的重要意义之一。

此外，影响视力的关键因素是充足的光照、用眼习惯和遗传，
跟数字设备没有直接关系。
只要孩子注意用眼卫生(比如，保证光线充足、适时休息)，
学编程不会增加近视风险。

@handbook-scenario{如何在奥数班和编程班里作选择？}

首先，这两类课外教育有很大的共同点。
比如：都与学校的数学教育没有直接关联；
都可以训练孩子的数学思维和计算思维。

其次，奥数班的学习内容和目标一目了然；
编程班则门类繁多，且目的不一、质量参差不齐。

我个人有两条建议(但这两条建议是并列关系)：

@itemlist[
  @item{理想情况下，
  如果奥数班同时也在教学生写程序解决奥数问题，那是十分的好。
  奥数题目固然锻炼思维，但学生也需要计算机帮助验证思路，
  尤其可以省下大量浪费在笔算这样的体力活上的时间。}

 @item{如果有注重计算思维的编程班，那也是极好。
  这样的班，编程是表象，解决问题才是内核，
  而编程解决的问题通常也来自信息学奥赛和数学奥赛，
  以及其他有趣的来源。}
]

总之，这两类课并不冲突，它们“你中有我，我中有你”。
但考虑到孩子负担可能很重，就不要两个同时报了吧。

@handbook-scenario{如何在 Scratch、Python、C++ 之间作选择？}

首先，编程语言作为开发工具和教学载体，
其选择的重要性远低于制定适合的教学策略与方法;
其次，要关注孩子的兴趣、能力、其它科目如逻辑/数学/阅读的学习水平，
以制定适合这个孩子的学习计划。

通常情况下，
Scratch 适合小学阶段、Python 适合小学高年级或以上、C++适合初中或以上的学生。
但上述划分是极其粗略的，仅有参考意义。
假如教学策略和方式得当，
学有余力的孩子完全可以在小学阶段写出超越专职程序员的代码。
在真实的教学实践中，
不乏多种语言、多种硬件混用且教学效果良好的实践案例。

@handbook-scenario{什么年龄开始学编程比较合适？}

从孩子的心智发展角度来讲，
最合适的入门年龄是12-14岁。
早了并不是不行，可能不划算。

但从现实角度来讲，
这个年龄正好是学生进入中学的时候，
紧接着是长达6年的应试教育期。
因此，这个时候才开始学编程对家长和孩子的统筹规划能力要求都极高。
简单言之，对大多数学生来说可行性不高，太晚了。

我个人建议从四年级开始比较好。
因为专业编程是个长期规划，
需要足够长的时间从0迈向1，
初学者花2-3年来入门是个比较合理的期望。

其次是学习目的，比起盲目跟风卷各种比赛，
不如把重点放在跨学科学习上。
这样，孩子进入初中之后，
才真正有能力将计算机从游戏机/影碟机转变为生产力工具，
从而带动考试科目一起提高。

总之，如果能找到靠谱的老师，学生学习能力不差，
提前到10岁开始学编程没有任何问题(学校
的双语教育事实上确实降低了编程的入门年龄)。
反之，不如在中学阶段好好学习数理化。

@handbook-scenario{学编程能提高学习成绩吗？}

从培养学科素养和思维方式的角度看肯定能；
但是否能提高眼前的考试分数，影响因素就很多了。
比如老师能否激发学生的整体学习激情、
学生是否能够领悟到跨学科学习的奥义，
等等。

比如，我知道的我自己的学生里就有好几个，
虽然只上过我的某些分科课程，
但他们会因此而对“学习”本身产生兴趣，
继而带动其他课程的成绩一起提高
(实践型课程都有这样的催化作用)。

@handbook-scenario[#:tag "parent-how-to"]{家长如何在课后帮助孩子学习编程？}

理想情况下，
家长最好跟孩子一起学习，
这是唯一有效帮助孩子规划课后学习时间的前提。
因为，编程这项能力足够新颖和专业，
成年人也没有办法光靠“看”网上的评论就真的做到理解编程。

非理想情况下，
家长可以从以下角度提供辅助。

@handbook-action{学习态度}

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

@handbook-action{学习习惯}

打算学编程的孩子至少要准备好：

@itemlist[
 @item{个人电脑(Windows 10+, macOS)。平板不能用来学编程。}
 @item{包含基本文具的文具盒。编程不只是敲键盘，纸笔构思必不可少。}
 @item{文件夹或文件袋，用于归档老师打印的资料和习题。}
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
把希望寄托在找老师补课上。

@handbook-action{编码训练}

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

@handbook-action{作业}

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
