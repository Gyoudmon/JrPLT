#lang scribble/base

@require{../../portfolios/literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{Scratch 教育}

今年我接触的 Scratch 学生几乎全是卡巴转课过来的孩子；
而从本机构升阶到高级编程的老学员，
他们的编程基础也能印证 Scratch 教育中的问题。
因此，放在一起讨论。

我个人认为，学习 Scratch 的适宜年龄/年级应该是小学一年级开始，
学生最好已经能阅读 Scratch 界面上的所有汉字。
Scratch 积木的形状和颜色应该是在
学生能够读懂汉字(和运算符)的基础之上帮助学生建立语法概念，
而不是作为学生撞大运式堆作品的唯一依据。

什么是“撞大运式编程”？@margin-note*{“撞大运式编程”一词本来就是软件行业总结出来的典型坏习惯，
典型到它有资格拥有自己的名字。}
简而言之，就是不知道自己要干嘛，通过胡乱替换积木来看程序是否正常运行。
其本质一种很低效的穷举法，这种方式不会在孩子脑中留下有价值的印象。
但是这个坏习惯一旦形成，
却会实实在在影响学生的一生。

在继续讨论之前，我们先统一以下共识：

@itemlist[
 #:style 'compact

 @item{颗粒搭建对学生的阅读能力没有要求，它是完全具象的训练方法。}

 @item{Scratch 自身是将具象的积木抽象成了虚拟的积木，
  这些积木用颜色、形状和文本标签来区分，是一种半抽象半具象的训练方法。
  学生至少要有识字能力。}

 @item{专业编程就是完全抽象的文本编程，它甚至不再强调“积木”这个概念，
  学生要具备一定的阅读能力和最基础的母语和英语写作能力。}]

@handbook-scenario{学情概况}

卡巴转课过来学编程的孩子，年龄普遍偏小。

@itemlist[
 #:style 'compact

 @item{学生在卡巴时上的编程课以智能积木、机器人为主，
  比之科普/科学知识的编程模拟，
  这类课程结合了颗粒搭建和Scratch的优点，
  对小年龄段的孩子的吸引力更强。}
 
 @item{学生学习编程的时长差别较大，因而基础参差不齐。
  能力强的会自己探索，能力差的鼠标用不顺手；
  擅长学习的孩子里也有个别学生有自己独特的问题，比如：

  @itemlist[
 #:style 'compact
 
 @item{畏难情绪严重、惧怕录制口头表达的视频。}
 @item{做事纯看兴趣，没有兴趣的事坚决不做。}]}
 @item{大部分学生不会用拼音打字。个别学生识字量太少，看不懂积木标签。}
 @item{学生家长中懂编程和懂素质教育的都不多，有空和有能力帮助孩子学习的更少。}

 @item{高知家长大多也不知道自己的孩子在学习过程中的问题。
  @itemlist[
 #:style 'compact

 @item{个别高知家长并不在乎孩子的想法，
    他们觉得我们说孩子优秀是为了增加转化率。}
 
 @item{个别高知家长对机构有些微辞，还把这些教给了孩子。
    比如：洛克白不是连锁店；洛克白的老师连个办公室都没有。}]}

]

@handbook-scenario{教学实施}

我的 Scratch 课，内容大多与同事同步，
但在教学实施上致力于从长远角度解决一些疑难杂症。

最核心的疑难杂症其实只有一个：学生学完就忘。

在刚开始，我会觉得这是学生的问题，
后来跟学生接触多了，我也慢慢能从学生的角度思考问题。

“学完就忘”是我们这种“每周一次”的教学模式的正常现象，
甚至是必然现象。
我的教学实施只能给出一些延缓措施或者补救方案。

@handbook-action{学习准备}

我会建议学生有条件的自己带电脑，
没条件的也至少要备个U盘，
有空了就在家翻出来玩一玩、读一读、改一改。

哪怕在家家长不让练习，
也能防止操作同一台公共电脑的其他同学误删自己的作品；

哪怕在家家长不让练习，
也能让家长知道学生到底学了什么。

此条建议，高级班(三年级或以上)学生基本都自带电脑了；
低年级学生也陆陆续续准备U盘了。

@handbook-action{基础训练}

这是初级班的必要流程。
每节课头15-30min先不讲新课，所有学生必须先独立完成一个初级程序，
这些程序是有意筛选的基础但重要的知识点。
比如：

@itemlist[
 @item{设置四个角色，分别通过控制x和y坐标让它们朝着四个方向不断循环运动。}
 @item{克隆体的创建、运行和销毁。在绝大多数场景，学生想到其中一个，必须要自动联想到另外两个。}
 ]

有能力独立完成的学生会被安排去帮助未完成的学生。

此措施在初级班试行了一段时间，学生都很配合，效果也还可以。
有一个二年级学生，他最早的“帮助”就是代替别人做，
现在逐渐开始“教”同学了。
可能的原因包括，他从“教”玩得好的学生中体验到了什么，
然后福及了新同学。

后续课程中，这类初级程序可以就从蓝桥杯赛题里提炼。

@handbook-action{结构训练}

这是所有学生的必要流程，也是我的授课重点。

@tamer-figure["s:structure" "Scratch 代码块的结构化组织"]{
 @stone-image["2023/scratch.structure.png" #:scale 0.42]}

以@tamer-figure-ref{s:structure}为例。
左侧的代码块是最常见的组织方式，写起来容易。
因为代码都是一个积木一个积木堆起来的，
即使是小学生，也能在不知不觉中就堆出很高的代码块来。
然后，他们就再也不想看了。
或者，他们从老师那看到今天要做的作品要写这么多代码，
一定会抱怨、畏难，觉得眼睛疼。

左侧的代码的问题在哪呢？
所有积木都挤在一起，
学生不能一眼就找到划分“自然段”的线索。

再来看@tamer-figure-ref{s:structure}右侧的代码，
它通过一系列“自建积木”把主程序(当绿旗被点击时)的逻辑线明确地点出来了，
学生可以只关注程序逻辑，然后再慢慢完善细节。
重要的细节包括“初始化”、“检察玩家操作”、游戏的核心算法等。
同理，如果每个“自建积木”的逻辑也很复杂，
那就如法炮制进一步细分。
这个就是“自顶向下”的设计。

这样的结构化训练策略还有诸多好处：

@itemlist[
 #:style 'compact

 @item{如果把这样的结构化训练放在学生手册里，孩子不一定会好好书写，
  即使写了也很可能下课就丢。
  最关键的是，学生手册里写的跟代码不一样，
  他们仍然没法说清楚这段程序做了什么，
  下次仍然会忘记怎么思考(比如不记得要“初始化”)。
  按此方式组织，那就是“代码即文档”，自成一体。}

 @item{学生抄老师的代码是普遍现象。
  按此方式组织代码，即使学生只会抄，
  一段时间之后学生入门了，再回过头来看自己的作品，
  那也都是优质阅读材料。
  @focus{其实要鼓励学生抄优质代码，
   他们自己写的代码只是碰巧能运行而已，
   长期来看很难自己突破瓶颈。}
  @idea{语文、英语都知道优秀读物的价值，
   编程也一样。}}

 @item{“自建积木”本身对应着专业编程中的“函数”，
  它既是最小的功能积木，也是写作中划分自然段的手段（它不只是“数学函数”）。
  不过，现在不用跟学生强调“函数”概念，等他们熟悉了这个思路，将来再一笔带过。
  否则，这些孩子升阶到专业编程课学习“函数”时还是会两眼一抹黑，满头的问号。}

 @item{让学生有目的的练习打字。
  这个好处看着不太起眼，但如果有升阶专业编程的计划，
  这就领先同龄人很多了。
  再不济，让老师轻松一些，
  一群孩子喊着要老师帮忙打字的时候真的很让人头疼。
  还有些家长会把打字能力纳入衡量考察学习效果的依据。}
 ]

实际上课情况来看，初级班和高级班都很受用，
大多数学生也乐意花上十几分钟慢慢认识键盘、练习打字。
有学生会跟我说“xx老师教我时也说过‘要初始化’”，
但实际上他们都没有“初始化”的意识，
现在好学的孩子都在慢慢形成习惯。

@idea{总之，无论是写作、还是软件工程，
 这些专业领域总结出来的增加“可读性”和“可维护性”的经验是普适的，
 成年人都觉得乱的学习方式，孩子必然也会抗拒。}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
