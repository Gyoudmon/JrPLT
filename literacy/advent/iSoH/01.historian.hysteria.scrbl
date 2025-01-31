#lang scribble/lp2

@(require "../literacy.rkt")

@require{../../share/diagram/aoc/2024/01.hh.rkt}
@require{../../share/timeline.rkt}

@(require digimon/digitama/tamer/pseudocode)
@(require racket/math)

@(require geofun/vector)
@(require diafun/flowchart)
@(require plotfun/axis)

@(define diaflow-scale 0.50)
@(define diaflow-node-scale 0.36)
@(define diaflow-marginfigure-scale 0.42)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define hh:add1-sticker
   (lambda [id r datum unit font color]
     (define c (rgb* color (/ (+ r 1.0) 8.0)))
     (define g (geo-vc-append (geo-text "+1" font #:color c)
                              (geo-arc (* unit 0.5) pi 0.0 #:stroke c #:ratio 0.85)))

     (cons (if (eq? 'arrow datum)
               (geo-pin* 1.0 0.56 0.5 0.5 g (geo-dart (* unit 0.1) (* pi 0.5) #:fill c #:stroke #false))
               g)
           'lc)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@aoc-task[2024 1]{慌乱的历史学者}

@aoc-desc[#:keywords ["文学式编程" "函数式编程" "类型签名" "算法" "数据结构" "列表" "高阶函数" "递推关系" "迭代" "递归" "REPL" "sexp"]
          #:edition [四 "2025-01-27"]]

阅读本章时，
读者应当重点关注的是@:val{如何将解谜思路翻译成代码}，
而不是纠结于晦涩的概念和古怪的语法细节，
那些可以留到后续任务中慢慢磨。
实际上，当你入门之后回看本书时，
会发现本章的内容在专业性和严谨性方面@:err{略显粗糙}。

本章带你熟悉本书的写作风格，
同时@emph{启动}你的@tech{函数式编程}之旅。

@handbook-scenario{文学式编程}

初来乍到，
说编程跟数学关系密切可以理解，
但跟文学的关系在哪里呢？
这个关系就是@:term{写作}，
人与人的交流依托自然语言，
人驱动计算机干活依托的是程序语言。
写作的本质是用文字表达作者的思想或意图，
除了词汇量和熟练度上的差异，写作本身应该与语言无关，
但要考虑读者的接受程度。
优秀的作者还会考虑@focus{语言自身的思维和惯例}。

@handbook-deftech[#:origin "Literate Programming"]{文学式编程}讲究以人为本，
用作者自己的写作思路来写程序，同样阅读程序的人也能更容易地理解程序。
这样的程序源码分为散文和代码两部分，
后者给编译程序看，它们会自动将散落在文章各处的代码碎片组装成正确的程序；
前者（和后者共同）给人类读者看。
比如你现在正在阅读的本书就是@tech{文学式编程}的例子。
此外，@handbook-sidenote*{
 而非我笨手笨脚地复制粘贴过来。
 这书里这么多程序和图形，
 每一个都要我亲自动手修改每一处，
 浪费时间不说，关键是太业余了。
}在 Racket 自身优势的加持下，
文中示例程序的运行结果也是最终程序的直接执行结果。

@handbook-action{代码碎片}

接下来我们通过完成第一天的主线任务来具体感受一下。

散文中的代码碎片有一个起点，
一般就是任务章节中出现的第一段碎片，
它看起来是这个样子的：

@handbook-chunk[<hysteria:*>
                (module advent typed/racket
                  |<Helper: Read Location IDs>|
                  |<Puzzle 1: Find Total Distance>|
                  |<Puzzle 2: Find Similarity Score>|)]

其中用@emph{角括号}（@:pn{< >}）括起来的部分就是散落在本章各处的代码碎片的名字，
其本质跟@tech{变量}名没有什么不一样，比如本任务的起点碎片名为@racket[<hysteria:*>]。
起点碎片一般用来确定任务代码的总体框架，
本例定义了一个名为 @:mod{advent} 的模块，
并指定该模块的书写语言为 @:mod{typed/racket}@handbook-footnote{Racket
 是@:term{语言导向}的语言，
 你还可以用 @:mod{python}、@:mod{datalog} 等多种其他语言来编写 Racket 模块。}；
紧接着的两个碎片名分别指向解决本任务中的两个谜题的代码碎片。

除了起点碎片必须是章节的第一个碎片外，
其他代码碎片可以出现在起点碎片之后文章的任何地方。
而且，只有实际用到的碎片才会出现在最终程序里，
那些没被使用的碎片的价值是继续留在本书里给读者看。
它们或提供了额外信息，或提供了另一种思路。

原始故事作为我们学习编程的情景，以故事卡片的形式呈现：

@aoc-story[@racket[|<Puzzle 1: Find Total Distance>|]]{
 精灵小队碰到的第一个问题是，他们的地址清单里@aoc-emph{什么都没有}，头脑一片空白。
 最后有人提议说，最好的起点理应是首席历史学家的办公室。
  
 于是，精灵们涌进办公室，人人都确认了首席历史学家确实失踪了。
 同时，精灵们还发现了一些有意思的笔记和一份写着“重要历史地点”的清单。
 看起来很像首席历史学家出发前制定的计划，
 也许这些笔记可以帮助大家确定寻找路线。
 
 重要地点并非根据名字列出，
 而是根据不重复的数字列出，
 姑且称作@aoc-emph{地址编号(location ID)}。
 为确保大家都没遗漏，精灵小队分成了两个小组，
 各自搜寻办公室并独立写下所有的地址编号。

 两份地址编号清单出炉之后，它们显然会不尽相同，
 于是你需要从中调解两队精灵。
 假设两份清单只会有很小的差异，
 可以将两组编号逐个配对并计算它们的差距。
 即：先@aoc-emph{配对两组清单中的最小编号}，
 然后是@aoc-emph{次小编号}，以此类推。
 对于每一对编号，计算出它们的@aoc-emph{差距(distance)}，
 最后将@aoc-emph{所有差距值相加得到总差距(total distance)}。
 问：@aoc-question{What is the total distance}?}

我特意选了与原文相似的配色风格。
故事卡的标签页上写着任务目标，
@handbook-sidenote*{你可以点击标签试试看}
标签内容同时也是我们实际做任务的代码碎片的名字；
任务中的重点以高亮度字体标记，
通常它们已经包含解谜的所有细节；
因此，@:thus{最后的问题会以英文形式给出，
 方便你阅读程序代码、积累必要的词汇}。

故事中的例子会从情景部分抽出来放在正文里，作为我们思考和探索的素材。
精灵们给的两份地址清单长这样(清单内容保存在文件中，
后缀名是 @:in{.aex}，插入本书时会在最左侧增加行号)：

@(tamer-filebox #:tag '01_hh.aex (aoc-tamer-path "iSoH/01_hh.aex"))

在这个文件中，左边那列出自甲组，右边的出自乙组。
总计共有六对地址编号，与之对应的差距如此计算：
@handbook-itemlist[
 #:style 'compact
 
 @item{甲组的最小编号是 @racket[1], 乙组的最小编号是 @racket[3]，它们的差距是 @racket[2]；}  
 @item{甲组的次小编号是 @racket[2], 乙组的次小编号是 @racket[3]，它们的差距是 @racket[1]；}  
 @item{甲组和乙组的第三小编号都是 @racket[3], 因此差距是 @racket[0]；}
 @item{下一对编号是 @racket[3] 和 @racket[4]，差距为 @racket[1]；}
 @item{第五对编号是 @racket[3] 和 @racket[5]，差距为 @racket[2]；}
 @item{最后，甲组的最大编号是 @racket[4]，乙组的最大编号是 @racket[9]，差距为 @racket[5]。}
 ]于是，将以上各对差距相加即可得到总差距：
@racket[2] @:pn{+} @racket[1] @:pn{+} @racket[0] @:pn{+}
@racket[1] @:pn{+} @racket[2] @:pn{+} @racket[5] @:pn{=}
@racket[11]。

@handbook-scenario{算法}

我们先设想一下，
如果不写程序，用纸和笔你会怎么解决这个问题？
要具体到眼睛如何“阅读”清单、
手和脑又如何配合完成累加计算。

首先有两个你能感觉“确实如此”，
但可能不大好描述的事实。
精灵们做事毛手毛脚，
给出的清单顺序乱七八糟，
因此在读取完全部清单之前没法着手计算。
其次，人眼阅读清单灵活度比较大，
可以以任何角度、任何顺序阅读，
也可以无限重复阅读。
但对于计算机，
最简单、最高效的做法是按“从左到右、从上到下”的顺序阅读，
并且只读一次。
而这个顺序事实上也能给你带来诸多便利。
比如，当清单长到你没办法“咋一下”就“一眼看出”时，
唯有“有条理的步骤”才能助你完成任务。
因此，基于以上两点事实，
我们把对第一个谜题的求解分成两大步。
正好分别对应着起点碎片里的@:term{辅助任务}
@racket[|<Helper: Read Location IDs>|]
和@:term{主线任务}1
@racket[|<Puzzle 1: Find Total Distance>|]

第一步，阅读清单并构造两份地址编号@tech{列表}：@:desc{
 阅读清单第一行，
 将读到的第一个数字加入甲组地址列表；
 将读到的第二个数字加入乙组地址列表。
 接着读取下一行，重复上述过程，直到没有更多内容为止}。
如此我们就得到了两份地址编号@tech{列表}，
分别出自甲组和乙组。

第二步，基于两份地址编号@tech{列表}按要求完成累加任务。
不过，比起在杂乱无章的地址@tech{列表}中费力地寻找最小值，
不如先@:desc{将两份地址列表各自按照从小到大的顺序排个序}。
如此这般把两份新@tech{列表}对齐放着，它们的地址编号就自动配上对了。
虽然累加计算无关乎计算顺序，但方便起见，
我们就按@tech{列表}顺序一个一个累加就好：@:desc{
 从头开始同步扫一眼两组地址编号列表，
 每扫到一组编号，就把它们的差距累加到总差距中，
 直到扫完整个列表停止}。
此时的总差距即是解锁下一个谜题的钥匙。

@handbook-action{自然语言描述}

在数学和计算机科学中，
我们把@focus{解决问题的一系列明确且有限的操作步骤
 }称为@handbook-deftech[#:origin "Algorithm"]{算法}；
通常，@focus{
 这些步骤用于@handbook-deftech[#:origin "Process"]{处理}一系列@handbook-deftech[#:origin "Input"]{输入}@tech{值}，
 并产生一系列@handbook-deftech[#:origin "Output"]{输出}@tech{值}}(@fig-ref{IPO.dia})。
@tamer-figure-margin['IPO.dia "算法：输入-处理-输出"]{@(geo-scale IPO.dia diaflow-marginfigure-scale)}

如果你喜欢做饭，那你肯定听过或看过菜谱，
它会详细说清楚需要的食材、佐料(@tech{输入})，
烹饪时的用量、工序、火候等等各种细节(@tech{处理})，
保证你照着做就真的能得到它想教会你做的菜(@tech{输出})。
菜谱写的就是厨房里的@tech{算法}。

那么对比菜谱，我们上面描述的解谜步骤能被称为@tech{算法}吗？能也不能。
说它能，是因为它确实给出了有限的步骤，并且按此步骤的确存在解开谜题的可能；
说它不能，@margin-note*{
 不专业的菜谱也有这个问题。比如“适量盐”。
 }是因为@:thus{它还是太过笼统，描述不够清晰，甚至造成歧义}。
比如：如何把数字“加入”到一个不存在的@tech{列表}中？
“重复上述过程”中的“上述”具体是哪几步？
相信聪明的你可以联系上下文自动理清所有细节，
但是你能教会你的同学、乃至你不满10岁的弟弟妹妹吗？
编程就是@idea{用计算机能理解的语言教会它做事}，
计算机可比人笨得多，
你不得不@idea{教会它所有你自己脑补的细节}。

描述@tech{算法}的方式有很多种，
像上面那样把@tech{算法}@emph{写成说明文或流水账日记}的方法，
@handbook-sidenote*{俗称的“大白话描述”}
叫做@:term{自然语言描述法}。
其缺点很明显，
写起来啰嗦，读起来……各有各的见解。

@handbook-action{伪代码描述}

在计算机科学和软件工程领域一直有一小撮研究员坚信，
存在某种方法能够自动将大白话翻译成软件，
近年来人工智能的重要突破一定程度上让我们看到了这种可能。
在深入研究那些有趣的课题之前，
我们先从传统角度亲身体验一下从大白话到程序代码的层层翻译过程。

我们先只考虑调整大白话@tech{算法}的第一步，@handbook-sidenote*{
 由于初次接触@tech{函数式编程}有太多新知识体系要慢慢建立，
 第二步等理解了解谜流程之后再考虑。
 其实我也是开了上帝视角之后才发现，
 现在只考虑第一步确实会显著降低我写作、你阅读的难度。
}使它看起来更有条理、更精确，
同时调整格式让它读起来不那么吓人。

@algo-pseudocode[
 #:tag 'desc:read-location-ids "Read Location IDs（大白话版）"
 @list['|read IDs|]{尝试从文件的当前位置@:in{读取}两个@:type{自然数}}
 @list['predicate?]{@emph{若} 读到的确实都是@:type{自然数}，@emph{则}：}
 @list['|cons 1st|]{@hspace[4]将第一个@:type{自然数}加到甲组地址编号列表的头部位置}
 @list['|cons 2nd|]{@hspace[4]将第二个@:type{自然数}加到乙组地址编号列表的头部位置}
 @list['loop]{@hspace[4]@emph{回到}@algo-goto['|read IDs|]重复执行}
 @list['done]{@emph{否则} @:cmt{; 两组地址编号列表构造完毕，告知结果}}
 ]

于是，我们就得到了书写工整的@algoref{desc:read-location-ids}。

本书惯例，为方便文中引用某个步骤，
我在@tech{算法}左边增加了@:term{行号}和@:term{步骤名}。
从“引用单个步骤”的目的看，
@:term{步骤名}更容易记忆和理解。
比如，通过引用第一行的步骤名，
大白话的“@:desc{重复上述过程}”被改写成了
更精确的“@:desc{@emph{回到}@algo-goto[#:tag 'desc:read-location-ids]{read IDs}重复执行}”。
而@:term{行号}天然有序，
它既能@focus{指示你按自然数顺序阅读@tech{算法}，
除非某个步骤或规则明确地改变顺序}；
也能方便地引用多个连续步骤。
比如：我们即将讨论@algoref{desc:read-location-ids}的第 @racket[3] @:pn{-} @racket[5] 行，
文中引用它们的链接显示为@algoref[#:line (cons 3 5)]{desc:read-location-ids},
或者像下一段开头那样在行号前面同时附上@tech{算法}标签。

有别于其他几行步骤，
@algo-ref[#:line (cons 3 5)]{desc:read-location-ids}前面都留下了@focus{相同数量}的空格。
这个在@tech{算法}的书写规范中称为@tamer-defterm[#:origin "Indent"]{缩进}，
其本质同文章自然段的@:term{首行缩进}。
目的是让你能更容易地阅读@tech{算法}，
同时也提醒你@focus{此处@tech{算法}的阅读顺序可能会有所调整}。
从排版角度讲，
描述@tech{算法}采用的是@:term{悬挂缩进}，
被缩进的@tech{算法}步骤归属于它们的上一行步骤。
对@tech{缩进}更直观的解释在@Secref{algo:flowchart}。

虽然@algo-ref{desc:read-location-ids}打着大白话@tech{算法}的名义，
但读起来总感觉哪里怪怪的。
有这个感觉就对了，
每一次翻译都会使得@tech{算法}离大白话更远、
离数学和可执行代码更近。
@handbook-sidenote*{除非你就是要造一个没法跟你愉快玩耍的机器人。}
计算机能直接执行的指令应当简洁和明确，不应该产生歧义，
一个总是误解你的程序没有存在价值。

那么，准备好了没？我们即将面对不说人话的@tech{算法}描述。

@algo-pseudocode[
 #:tag 'algo:rpcl "Read Location IDs(代数版)"
 @list['initialization!]{@emph{设} @${X}、@${Y}分别是@focus{初始}代表甲、乙两组地址编号列表的@emph{空列表}}
 @list['|read IDs|]{尝试从文件当前位置@:in{读取}两个@:type{自然数}，@emph{设}为@${a}、@${b}}
 @list['predicate?]{@tt{if}@hspace[2]@:pn{@${a}和@${b}确实都是@:type{自然数}}, @tt{then}}
 @list['|cons X|]{@hspace[4]@emph{令} @focus{新}@${X = a:X}}
 @list['|cons Y|]{@hspace[4]@emph{令} @focus{新}@${Y = b:Y}}
 @list['loop]{@hspace[4]@emph{回到}@algo-goto['|read IDs|]重复执行}
 @list['No]{@tt{else} @:cmt{; 此时的 @${X}、@${Y} 分别指代甲、乙两组地址编号列表}}
 @list['done]{@hspace[4]@:cmt{; 告知结果}}
]

于是，用数学语言(和代码)翻译@algo-ref{desc:read-location-ids}后我们得到了@algo-ref{algo:rpcl}。
怎么样，汗流浃背了没？深呼吸，放轻松。
这个@tech{算法}每一步做了什么事都已经确定了(和大白话版本的描述对比即可)，
只是选用了诸多怪异的符号来书写。

无论是以大白话为主的@algo-ref{desc:read-location-ids}，
还是混合了大白话、数学语言、程序代码的@algo-ref{algo:rpcl}。
这种@emph{没有固定章法@handbook-footnote{计算机类论文、
  书籍中出现的@tech{伪代码}经常看着很像作者喜欢的程序语言的代码。
  这说明@tech{伪代码}可以写成任何样子，
  反而像本书这样照顾年少初学者的写法才是另类。
 }，却又处处透露着死板}的描述@tech{算法}的方式称为@tamer-deftech[#:origin "Pseudocode"]{伪代码}描述。
像真的代码一样一板一眼，又不能像真的代码一样直接喂给计算机去执行。

@handbook-action[#:tag "algo:flowchart"]{流程图描述}

@tech{伪代码}步骤名的另一个妙用是作为@tech{流程图}的标签，
以图形化的形式更直观地表达@tech{算法}步骤及其执行顺序。
比如，把@algo-ref{algo:rpcl}画成流程图就长@fig-ref{flow:rpcl}那样。

@tamer-figure!['flow:rpcl
               @list{@algo-ref{algo:rpcl} 流程图，后接@fig-ref{flow:puzzle1}或@fig-ref{flow:puzzle2}}
               @(tamer-delayed-figure-apply #:values geo-scale #:post-argv '(0.50)
                                            make-hh-helper.dia 'flow:puzzle1 'flow:puzzle2)]

@tamer-deftech[#:origin "Flowchart"]{流程图}使用@emph{实线箭头}指示下一步活动，
使用@emph{几何形状}指示活动类型。
常用图形有：

@handbook-sidenote*{标准@tech{流程图}没有规定颜色，
 但本书惯例，图形和箭头的颜色也遵循固定风格。这里不详述。}

@itemlist[
 #:style 'compact

 @item{@dia-flow-node[#:scale diaflow-node-scale]{^跑道形}
  指示@tech{算法}的开始和结束。
  每个@tech{算法}都必定有一个开始，应当@focus{至少}有一个结束。
  在绘制时通常会“向下”或“向右”流动。}
 
 @item{@dia-flow-node[#:scale diaflow-node-scale]{>>平行四边形} 指示@tech{算法}的@tech{输入}和@tech{输出}。
  这里更强调的是“获取输入”和“展示输出”这两个动作。}
 
 @item{@dia-flow-node[#:scale (* diaflow-node-scale 1.18)]{菱形?} 指示@tech{算法}的条件分支。
  也即@tech{算法}走到了岔路口，要根据条件决定选择哪条分支继续。
  @focus{分支不体现先后顺序}，仅在每条分支的第一个箭头上附上条件成立与否的标签。
  比如：@algo-ref[#:line 'predicate?]{algo:rpcl}
  根据条件“@:desc{读到的都是@:type{自然数}}”是否成立
  来决定是继续读文件(@tt{Yes})，还是计算总差距(@tt{No})。

  @handbook-itemlist[
 #:style 'compact
 
 @item{@focus{分支在@tech{伪代码}中就体现为@tech{缩进}。}}
 @item{循环的本质就是@emph{箭头往回指}，因此也离不开条件分支。}]}

 @item{@dia-flow-node[#:scale diaflow-node-scale]{长方形} 是最常见也最务实的活动类型，代表实际要干的活。
  如果步骤比较复杂，长方形两边会加个竖线，表明该步骤可以继续分解成另一个更小的任务。}
 
 @item{@dia-flow-node[#:scale diaflow-node-scale]{六边形!} 表明@tech{算法}依赖重要的准备工作。
  虽然解谜类@tech{算法}通常无需准备，或有其他系统代劳。
  但是初学者经常忘记给变量设置初始值，
  因此我会在本书强调@emph{初始化}这个步骤。
  你看，你是不是没发现这个步骤并没有出现在大白话描述的@algo-ref{desc:read-location-ids}里。}
 ]

当你要跟其他人协作解决复杂问题时，
或者像本书这样帮助初学者自学编程时，
@handbook-sidenote*{像不像语文、英语课上的缩句和扩句练习？}
在问题分析阶段搭配使用@tech{伪代码}和@tech{流程图}是比较好的描述@tech{算法}的方式。
@tech{流程图}帮你理清和记忆线索，
@tech{伪代码}详细解释线索中每一步的关键点；
当你个人能力足够强，且不涉及协作或教学时，
@tech{流程图}就不是必须的了。
因为它过于古早，描述能力太弱；
而且画起来既麻烦又很占地方。

在正式学编程之前，我花了这么多篇幅来讲“语言无关”的@tech{算法}思想。
其根本原因是@:thus{各种各样的计算机@tech{算法}事实上都起源于世界各民族的纸笔@tech{算法}}。
不过，@focus{由于本书的核心是@tech{函数式编程}，
 超越了大白话@tech{伪代码}的常规描述能力。
}因此，阅读本书时，
@:thus{你仍会经常发现将@tech{伪代码}翻译成真实代码时存在诸多思维转换和提升。}

此外，@idea{你所掌握的不同@emph{类型}的程序语言，
 你的数学功底，以及其他知识储备，它们都会互相影响，
 并最终反映在你写出来的@tech{伪代码}上。
}实际上，能完成任务的@tech{算法}何其多，
我有意写得贴近本书的学习目标。
比如，按照正常人用纸笔解谜的过程，
他们更可能把读到的数字加到@tech{列表}末尾，而不是开头。

@handbook-action{类型化程序语言描述}

回看@algo-ref{algo:rpcl}和@fig-ref{flow:rpcl}，
它们真的能帮你记忆关键线索吗？
确实很勉强，上面我给各个步骤起名时做了个@:err{很不好的示范}，
简单来说就是@:err{词不达意}，或@:err{过于言简意赅}了。
这个问题值得我说得更详细一些：

@handbook-itemlist[
 #:style 'compact

 @item{首先，@focus{使用英文命名并不是造成你理解困难的原因，
   因为你的词汇量只会越来越多。
   }当你适应英文阅读时，
  你很可能也会嫌弃我@emph{竟然用中文写@tech{伪代码}，
   岂有此理，取关拉黑！}}
 
@item{其次，科学教科书里出现的@:term{概念名}、
  @:term{常量}和@tech{变量}往往都会粗暴地用一个(或两个)字母来表示。
  这个习惯有其可取之处，因为那些字母都有明确定义，
  大多数情况下甚至在整个学科内都没有歧义；@handbook-sidenote*{
   或者，有没有可能这就是数学让人头疼的关键因素之一？
  }作为对比，数学习题就喜欢很随意地雇佣
  @${a}， @${b}， @${c}， @${x}， @${y}， @${z}，
  它们除了表示“未知”外再也不提供任何信息。
  因为它们只是为出题服务，
  你做完就忘也完全没有问题，
  复习时也是连着题目一起看，
  而不会只看这些字母。}

 @item{等到我们设计@tech{算法}时，
  @focus{命名问题可能会很突出}。
  @tech{伪代码}跟数学习题一样，
  会包含足够信息让你理解各个字母的含义，
  因此我在@tech{伪代码}里也会用单个字母表示未知数。
  而当@tech{伪代码}被翻译成程序代码之后，@handbook-sidenote*{
   这方面千万别偷懒，也不要盲从各种比赛题目，
   包括带赛教练、所谓刷题刷出来的金牌选手。
   用我的方法试试，对比之后才会形成你自己的见解。
   }代码就成了你复习时最关键的材料(它们通常不会跟题目/问题一起保存)。
  这时候@focus{可读性比什么都重要，你的代码读起来应该像(语法独特的)说明文。}
  @:thus{除了定义明确的概念可以沿用数学惯例外，
   其他未知数的名字都应仔细斟酌}，
  即使只用一个字母，也应优先用单词首字母，
  而不是随便来一堆@${a}， @${b}， @${c}， @${d}……}
]

@aoc-complain{
 来做个小测试，快速说出下面这些字都是什么颜色？
 @centered{@:err{蓝} @:cmt{绿} @:id{紫} @:type{橙}}
 怎么样，感觉脑袋里有小人打架了没？
 @focus{你会优先考虑字的意思，然后才是其他信息}。
 而胡乱选用的字母本身并没有具体的意思……
 
 @focus{科学家只会给重要的概念起名字，
  然后赋予这个名字一个专属字母符号}，
 数学也不例外。
 比如：圆周率@${\pi}，自然常数@${e}，等等。
 “起名字”这事在编程过程中就像呼吸一样自然，
 @:thus{且重要，能反应出你是否真的理解了问题。
  经常训练这个亦可以激活你的词汇量}，
 就当是在玩一种更实用的字谜游戏了。}

@handbook-scene{读取-判断-扩列 循环}

所有的@tech{算法}也都应该有个名字，
而@:term{循环}是绝大多数程序的典型特征。
因此，不妨把@algo-ref{algo:rpcl}称作@:term{读取-判断-扩列 循环}，
缩写为 @:id{rpcl}(read-predicate-construct loop)。
于是：

@handbook-chunk[|<read-predicate-construct loop>|
                (let rpcl (<initialization!>)
                  |<read IDs>|
                  (if <predicate?>
                      <地址编号扩列，迭代下一轮>
                      <done>))]

@focus{@tech[#:key "函数式编程"]{函数式}代码惊人的短小精悍}有没有？
之后的活就是把那些与步骤名对应的代码碎片一个个翻译成真实代码。
注意区分，
代码碎片用@emph{角括号}(@:pn{< >})界定，
@tech{伪代码}步骤名用@emph{角引号}(@:pn{« »})界定。
本例中它们大致上一一对应，
除了@focus{用晦涩的中文}写出来的那句。

@algo-ref{algo:rpcl}看着已经很数学了，
但如果我说它还可以进一步数学化你会怎么想？
前面我们强调了@tech{算法}步骤的@:term{简洁}和@:term{精确}，
下一步就该考虑@:term{严谨}了，
也即在@focus{任何}情况下都不出错。
而且最好是@focus{让计算机自己能确保这件事，
 而不是浪费我们宝贵的时间去一遍遍检查}。
再怎么改@tech{伪代码}也还是要我们亲自去确认，听着就很费时间。
所以，这个目标我们直接放到真实代码中来，
这也是@idea{在智能时代@:err{不能}只学数学而不学计算机科学}的原因。
这方面，@focus{计算机科学家的方法甚至比数学家的都更优越}。

程序语言确保@tech{算法}严谨的地基是@focus{给代码加上类型标记}。
于是，@:type{Typed} Racket 定义@tech{变量}需要同时提供三个信息：
@:term{变量名}，(可选的)@:term{变量类型}和@:term{初始值}。
格式为“@:term{变量名} @:pn{:} @:term{变量类型} @:term{初始值}”。

@algo-ref[#:line 'initialization!]{algo:rpcl}没有强调类型，
在这补上：

@handbook-chunk[<initialization!>
                [A.IDs : (Listof Natural) null] (code:comment "甲组精灵写下的地址编号列表")
                [B.IDs : (Listof Natural) null] (code:comment "乙组精灵写下的地址编号列表")]

本例中的两个@tech{变量}的类型均为@:term{自然数列表}(@:type{(Listof Natural)})，
初始值@:term{空列表}写作 @:val{null}。
分号(@:pn{;})及其后面到本行结尾的内容是@tamer-deftech[#:origin "Comment"]{注释}，
这是写给人类读者看的，在计算机看来相当于空格。

此外，本段代码碎片将@tech{变量} @${X}和@${Y}重命名成了 @:var{A.IDs} 和 @:var{B.IDs}。

@aoc-complain{
 嗯，看起来就很奇怪的名字。
 绝大多数程序语言都会喋喋不休地跟你强调“什么样的字符不能用来给变量命名”，
 相当无语有没有？
 在 Racket 里你基本不用操心这个问题，
 @:thus{只要不与语法冲突就都是有效的名字}。
 比如，@:in{3x-b+C}, @:in{D:/NOI/readme.cpp}, @:in{sin72º}, @:in{《语文》}
 等等各种人类一看就懂的名字都没有问题。
 这里的 @:var{A} 和 @:var{B} 也不是乱起的，
 它们就是@:var{甲}、@:var{乙}在英语里的合理翻译。
  
 至于特殊符号在变量名中有无特别的含义？
 没有，都是语言社区或代码作者自己的风格。
 本例中，你可以沿用其他对象导向语言中的通行做法将点(@litchar{.})解读为对象
 @:var{A} 和 @:var{B} 的@:term{成员访问操作符}，
 直译为@litchar{的}。
 只不过 Racket 没有把它设计成硬性语法规则，
 这里我个人选用了这种更为大众化的惯例。}

@algo-ref[#:line '|read IDs|]{algo:rpcl}就是字面直译，
@:desc{对清单文件 @:var{locin} 执行 @racket[read] 操作，给结果起名为 @${a}；
 重复一次，给结果起名为 @${b}}：

@handbook-chunk[|<read IDs>|
                (define a : Any (read locin))
                (define b : Any (read locin))]

@tamer-figure-margin['read.dia @list{@algoref[#:line '|read IDs|]{algo:rpcl}}]{
 @(geo-scale read.dia diaflow-marginfigure-scale)}

函数 @:id{read} 的求值结果@handbook-footnote{Racket 不太强调@:term{返回值}这个说法，
 而更偏好@handbook-deftech[#:origin "Evalutes to a Result"]{求值结果}。
 如果你已经习惯了其他语言的叫法，那就按你自己的习惯来。
 }可以是@emph{任何}（@:type{Any}）合理类型的@tech{值}，
其中包括特殊值 @tamer-defterm[#:origin "End of File"]{eof}，
代表@:name{文件结尾}，也即清单读完了。
注意，此时我们要假装自己是英语母语人士，
将文件里的@:term{连续空格}(包括@:term{换行})视作单词、数字和符号的分隔符。
因此只管@emph{读}，不用考虑换行。
这也意味着，@:id{read} 自己知道@emph{当前位置}在哪里，
就像我们在看书时，眼睛会跟着一起移动(@fig-ref{read.dia})。

本例中，我们可以放心地假设，
@${a} 和 @${b} 要么是@emph{自然数}，要么是 @tech{eof},
不存在其他可能。
于是，@algo-ref[#:line 'predicate?]{algo:rpcl} 就是对结果提问：@:desc{
 @${a}是自然数吗？@${b}是自然数吗？}
并且要同时回答 @racket[#true](@emph{是})才算条件达成(@fig-ref{predicate.dia})：

@handbook-chunk[<predicate?>
                (and (exact-nonnegative-integer? a)  (code:comment "a 是自然数吗？")
                     (exact-nonnegative-integer? b)) (code:comment "b 是自然数吗？")]

@tamer-figure-margin['predicate.dia @algoref[#:line '|predicate?|]{algo:rpcl}]{
 @(geo-scale predicate.dia diaflow-marginfigure-scale)}

咦，这个提问里为啥没有出现@emph{自然数}(@racket[natural])?
这是因为 Racket 的原生数值类型最接近数学的数系@handbook-footnote{
 确实也有别的语言(比如 Smalltalk)的数值类型也很全面，
 但只能是并列第一，而不能更全面了。
 因为这已经是技术上的极限。
 此外，像 Lean 等数学研究型语言如何定义数值类型是另一个话题，请勿混淆。}：
所有的数都是@:term{复数}，复数之下有@:term{实数}，
实数之下有@:term{有理数}和特殊浮点数，
有理数之下，有@:term{整数}、@:term{分数}、可数值化的@:term{浮点数}。

在这个前提下有一个例外：@racket[1] 和 @racket[1.0]。
数学上，它俩都有可能是@:term{精确数}(exact number)，
也都有可能是@:term{近似数}(inexact number)。
而在 Racket 中，前者一定是@:term{精确数}，后者一定是@:term{近似数}。
因此，问句变成了@emph{@${a}/@${b}是精确的非负整数(exact nonnegative integer)吗}，
更突出了@emph{自然数}的现代定义，且消除了争议。

像@handbook-sidenote*{Racket 的@tech{谓词函数}通常以问号(@:pn{?})结尾。}
@racket[exact-nonnegative-integer?] 这样@emph{
 接受一个任意类型的参数，
 得到一个布尔型（@:type{Boolean}）的结果（@racket[#true] 或 @racket[#false]），
 用以检查输入参数是否满足某些条件}的函数称为@handbook-deftech[#:origin "Predicate Function"]{谓词函数}，
可类比一般疑问句中的谓语，但句法上要简单很多。

@aoc-complain{是吧？Racket 代码读起来很像（语法怪异的）英语写的数学说明文，
 但一眼看上去又不像数学证明。嗯，
 Racket 的命名风格偏向严谨，极少瞎搞缩写，比较合我的口味。
 @idea{对英语语感尚不稳定的初学者来说也是件微不足道的好事}。}

按照正常说话顺序，“@:desc{若 @algoref[#:line 'predicate?]{algo:rpcl}，
 则 @algoref[#:line (cons 4 6)]{algo:rpcl}，
 否则 @algoref[#:line (cons 7 8)]{algo:rpcl}}”，
在得到 @${a} 和 @${b} 的回答之后，
我们会先处理@emph{条件成立}时要做的事：

@handbook-chunk[|<地址编号扩列，迭代下一轮>|
                (rpcl (cons a A.IDs)  (code:comment "构造新的甲组编号列表，保证 a 成为原列表的头部")
                      (cons b B.IDs)) (code:comment "构造新的乙组编号列表，保证 b 成为原列表的头部")]

这段代码碎片的名字相当晦涩，写出来倒是意外地简单。
它蕴含了@tech{函数式编程}的基本原理，暂不详述。
先忍耐一下，如果实在憋不住，
可以先从@Secref{sec:fp}看起，
到@Secref{sec:rloop}结束。

于是，就剩最后一句了。
当@emph{条件不成立}时，
@algo-ref{algo:rpcl}就直接结束了：

@handbook-chunk[<done>
                (values A.IDs B.IDs)]

@tamer-figure-margin['values.dia @list{恒等函数 @${f(x) = x}}]{
 @(geo-scale values.dia diaflow-marginfigure-scale)}

函数 @:id{values} 有点奇怪，
它的求值结果就是一切你给它的@tech{值}，
什么多余的事情都不做，
常常用作@handbook-deftech[#:origin "Identity Function"]{恒等函数
 }(@fig-ref{values.dia})。这的特殊之处在于，
Racket 是为数不多@focus{直接允许@tech{函数}拥有多个结果@tech{值}}的语言@handbook-footnote{
 Python、C++ 2017等现代语言也可以在语法上做到“看起来有多个返回值”，
 其本质是在它们内部使用了@emph{元组}这样的集合类型。
 达到语法上以假乱真的过程也稍显繁琐，
 至少有“打包”，“返回”，“解包”三个步骤。
}，而且你很幸运，
碰到的第一个@tech{算法}就天然需要利用这个特性来@tech{输出}两个@tech{列表}。

有趣的事情又来了，正常情况下，
@tech{算法}都会产生@tech{输出}结果。
在@tech{函数式编程}中这一点尤其本分，
因为@focus{除了极个别跟“定义”有关的语句外，@handbook-sidenote*{
 自信点，这确实就是你在数学课上学到的那个代数表达式。
}其他每一条语句都是@:term{代数表达式}}，
它们天然就有结果@tech{值}。
如此，谁做@tech{算法}的最后一条语句，
谁的@tech{值}也就顺理成章、
自然而然地成了整个@tech{算法}的@tech{输出}结果。@handbook-sidenote*{
 前文我是不是说过，Racket 不太强调“返回值”这个说法？
}这也是为什么我把@algo-ref[#:line 'done]{algo:rpcl}的描述写成了@tech{注释}。

@aoc-complain{对于常规编程课教授的语言，
 它们的语句是执行指令的动作序列，
 经常会搞出个 @:kw{return} 来表达“这就是结果”。
 函数式语言不需要专门的 @:kw{return} 语句。}

顺带一提，借助 @:id{values} 函数，
@algo-ref[#:line '|read IDs|]{algo:rpcl}也可以简化为一句话：

@handbook-sidenote*{注意，这段代码碎片没有包含在任何其他碎片中，因此不会出现在最终代码里。}
@handbook-chunk[|<read IDs/Values>|
                (define-values (a b) (values (read locin) (read locin)))]

@handbook-scene{类型签名}

从解谜的角度来说，我们已经打好草稿了；
从答题的角度来说，还缺少一步：把草稿誊写到答题卡上。

先看一下代码碎片@racket[|<read-predicate-construct loop>|]组装完成后的样子：

@handbook-sidenote*{同样，本段代码碎片也没有包含在任何其他碎片中，因此不会出现在最终代码里。}
@handbook-chunk[<rpcl>
                (let rpcl ([A.IDs : (Listof Natural) null]  (code:comment "甲组精灵的地址编号列表")
                           [B.IDs : (Listof Natural) null]) (code:comment "乙组精灵的地址编号列表")
                  (define a : Any (read locin))
                  (define b : Any (read locin))

                  (if (and (exact-nonnegative-integer? a)  (code:comment "a 是自然数吗？")
                           (exact-nonnegative-integer? b)) (code:comment "b 是自然数吗？")
                      (rpcl (cons a A.IDs)  (code:comment "构造新的甲组编号列表，保证 a 是头部")
                            (cons b B.IDs)) (code:comment "构造新的乙组编号列表，保证 b 是头部")
                      (values A.IDs B.IDs)))]

全景下的@racket[|<read-predicate-construct loop>|]就剩一个疑点了：
@:stx:def{let} 就是数学解答题和证明题中常用的@:term{设}或@:term{令}，
如果顺便给 @:stx:def{let} 起个名字，
就会同时定义一个同名@tech{函数}，@focus{并立即调用它}。
本例中，我们定义的@tech{函数}叫做 @:id{rpcl}，带两个参数。
之所以在定义时就火急火燎地调用它，
是因为它只是个@emph{临时}@tech{函数}，
写完就随手扔进垃圾桶了，
这种匪夷所思的事经常发生在你@emph{不得不记笔记}时。
而习惯较好的做法是把它放置在另一个正式@tech{函数}里，方便随叫随用。
这个正式@tech{函数}便是本节一开始就提到的@:term{辅助任务}碎片：

@handbook-chunk[|<Helper: Read Location IDs>|
                (define read-location-ids <函数read-location-ids的类型签名>
                  (λ <函数read-location-ids的参数列表>
                    |<read-predicate-construct loop>|))]

这段代码碎片用 @:stx{define} 和 @:stx{λ}
严肃定义了一个叫做 @:id{read-location-ids} 的函数，@handbook-sidenote*{
 λ 是第十一个希腊字母的小写，在函数式语言里代表的正是函数本体。
 在 Racket 中也可写成它的英语单词： @racket[lambda]。
}除了在它内部包含了@racket[|<read-predicate-construct loop>|]本体外，
还需标注清楚它的@tech{类型签名}和参数列表。
所谓@handbook-deftech[#:origin "Type Signature"]{类型签名}，
简单来说就是@tech{函数}的@:term{定义域}(输入参数)和@:term{陪域}(可能的输出结果)应当满足的类型约定。
标注@tech{类型签名}的方式有很多，
比较常见的是像下面这样：

@handbook-chunk[<函数read-location-ids的类型签名>
                : (-> Input-Port (Values (Listof Natural) (Listof Natural)))]

跟@tech{变量}类型一样，
@tech{函数}的类型信息也跟在用空格隔开的单个冒号（@:pn{:}）后面。
@focus{@:pn{(-> )}表明这个类型代表的是@tech{函数}，
里面的最后一个类型代表@:term{陪域}类型；
其余类型按顺序@emph{依次}表示每一个@tech{输入}参数的类型。}
本例中@tech{输入}参数只有一个，
而那个大写的 @:stx{Values} 带出了两个结果@tech{值}。
于是，函数 @:id{read-location-ids} 的功能可以简单表述为：
@:thus{给定一个类型为@:term{输入流}（@:type{Input-Port}）的@tech{输入}参数，
 得到两个类型为@:term{自然数列表}（@:type{(Listof Natural)}）的结果。}
完美匹配了辅助任务的目标：
@:desc{阅读谜题提供的清单文件，得到两组精灵写下的编号列表。}

@focus{@tech{类型签名}只强调参数的类型，不强调参数的名字}。
因为计算机不懂名字的寓意，它们更擅长通过类型来理解程序；
而人类读者更习惯通过@emph{有意义的名字}来理解内容。
因此，我们把类型为 @:type{Input-Port} 的参数命名为 @:var{locin}（即
@litchar{loc}ation ID @litchar{in}put-port 的缩写）：

@handbook-chunk[<函数read-location-ids的参数列表>
                [locin]]

你注意到代码碎片@racket[|<read IDs>|]里也用到@tech{变量} @:var{locin} 了吗？
这也是 @:id{rpcl} 草稿函数实锤的另一个重要原因：
它知道如何解谜，但是没有花精力去关注谜题里的数据到底从哪来。
当它被放置在 @:id{read-location-ids} 里时，
就自动共享了@:term{输入流} @:var{locin}。
@:desc{读取数据}就像@:desc{打开水龙头就有水流出来}一样自然。

最后再唠叨一句，
在将@tech{伪代码}翻译成程序之前，
我首先强调了要给@tech{算法}起名。
不知你注意到了没，
@algo-ref{algo:rpcl}本来也是有名字的，
在它自己序号右边，
那个醒目的@racket["Find Location IDs(大白话版)"]。
只不过在实际程序中，
一个名字可能不是那么地够用。

@handbook-scenario{REPL}

函数 @:id{rpcl} 的命名借鉴了Lisp语系传统的@handbook-deftech[#:origin "Read-Eval-Print Loop" #:abbr "REPL"]{
 读取-求值-打印循环}。即，每一轮循环要做的事是：
@:desc{读取用户输入的表达式，评估此表达式的结果，再把结果打印出来。}
这个循环看起来不太起眼，但在很多场合能极大的提高你的编码效率。
因而时至今日，好多语言都抄了一个类似的东西。
比如运行 @exec{idle.exe}，那就是 Python 的 @tech{REPL}，
只是它用了另一个更常见的名字，叫做@:term{Shell}。

@handbook-action{值}

在本书中，Racket 的 @tech{REPL} 既用于举例，
也用于直接运行我们主线任务中的代码。
它看起来是这个样子的：

@tamer-repl[(code:comment "这是一个 Flonum 类型的值")
            1.618]

提示符（@:pn{>}）后面的就是用户输入的表达式，
如@tech{注释}所言，本例演示的是 Racket 的@tech{值}。
Racket 读取到一个表达式之后，
会对该表达式求@tech{值}（即，尝试化简该表达式），
然后打印出@tech{值}的类型信息(@:out{@:pn{- :}和后面的紫色文字})和内容（@:res{下一行的蓝色文字}）。

所谓@handbook-deftech[#:origin "value"]{值}，
指的就是@emph{不可再化简的表达式}。
所以上面的 @racket[1.618] 的@tech{值}就是它自己。
这段话看起来很绕口，
要不我们就索性再捋一捋，
程序中几个最基础但又特容易弄混的概念，
以及@tech{函数式编程}中的@tech{函数}的独特之处。开始：

@tamer-repl[(code:comment "给上面的值起个名字,叫做Φ")
            (define Φ : Flonum 1.618)
            (code:comment "于是你可以用变量名Φ来代指1.618 (即：化简Φ得到最终值1.618)")
            Φ
            (code:comment @#,list{@tech{谓词函数} natural? 会被替换成(-> Any Boolean)类型的值})
            natural?
            (code:comment "λ是定义函数的表达式，下面这句会被替换成(-> Any Any)类型的值")
            (λ [arg] arg)
            (code:comment "给上面的函数值起个名字叫做id")
            (define id : (-> Any Any) (λ [arg] arg))
            (code:comment "于是你可以用变量名id来代指(λ [arg] arg)")
            id
            (code:comment @#,list{也可以用变量名id将(λ [arg] arg)用作函数（想起@tech{恒等函数}了没？）})
            (id Φ)]

迷糊了没？迷糊就对了。
简单来说，在@tech[#:key "函数式编程"]{函数式语言}里，
@tech{函数}既可以用来操作其他的@tech{值}，
它们自己也跟普通的@tech{值}一样，是一种特殊的@tech{值}。
在 Racket 中，这类特殊的@tech{值}统称为
@:term{procedure}(也即：可执行的过程@handbook-footnote{
 实际上，程序语言中从来没有“函数”这个东西，
 有的都只是“过程”(或“子程序”)。
 但，既然不存在真正的函数，那就继续叫它们函数好了。
 正经地说，至少在主打函数式编程的语言里，
 “函数”是个更合适的名字，
 区分那几个术语意义不大，
 更不能帮你理解你真正想表达的概念。
 真实世界就是很复杂，
 已经起错了的名字也没法说改就改，
 不要浪费时间去争论这个。})。
不给@tech{值}起名，它们就都是只存在于草稿纸上的一次性用品，
只有用 @:stx{define} (或其他定义变量的语法)给它们起名之后，
它们才算获得了正式身份。
注意看上面例子中的 @racket[id] 和 @racket[(λ [arg] arg)]，
它们的@tech{值}是有差别的：
前者是 @procedure{id}，
冒号(@:pn{：})点出了它的名字是 @:id{id};
后者是 @:res{#<procedure>}，并没有名字。
像这种没有被正式起名的@tech{函数}也被称为@:name{匿名函数}。

@handbook-action{输入重定向}

于是，
接下来我们实际运行代表@emph{辅助任务}@racket[|<Helper: Read Location IDs>|]的函数 @:id{read-location-ids}，
看看它到底会@tech{输出}些啥：

@handbook-sidenote{@(tamer-filebox (aoc-tamer-path "iSoH/01_hh.aex") #:path-centerized? #true)}
@tamer-repl[#:requires ["../aoc.rkt"]
            ($ read-location-ids < "iSoH/01_hh.aex" > list)]

于是，我们看到了一个大@tech{列表}包着两个小@tech{列表}，
小@tech{列表}里分别存储着两组精灵写下的地址编号。
例题数据在侧边栏里了，你对比一下，
地址编号的存储顺序是不是跟数据文件里的相反？

本书50个谜题的例题数据和谜题数据都以文件形式提供，
想来重复“打开/关闭”如此多的文件是件相当无趣的事，
不如把语法简化为格式“@exec{$ @${func} @${args ...} @:pn{<} @${filename}}”好了。
@margin-note*{前文我是不是提过，Racket的主特性是“语言导向编程”，
 并且已经有一个复刻自 Python 的子语言？
 }这也是 Racket 自带的特殊能力，
我有意把它的语法改得像 Shell：
使用@:term{标准输入重定向操作符}(@:pn{<})将文件内容喂给函数 @:id{read-location-ids}。
换句话说，前文中提到的那个类型为 @:type{Input-Port} 的唯一的@tech{输入}参数 @:variable{locin}，
其源头就在这。

@aoc-complain{
 场面略有些尴尬。
 “允许函数存在多个结果@tech{值}”这事看着多少有点不纯粹，
 现代语言都在尽量回避它。
 于是，出于某些技术上的限制，
 我没法直接在本书的正文部分演示它，
 不得不扩展语法，
 使用@:term{标准输出重定向操作符}(@:pn{>})将本例的两个结果@tech{值}收集到另一个@tech{列表}中。
}

至此，你完成了第一个可以把玩的 Racket 程序。

@handbook-scenario{数据结构}

计算机科学领域有个相当著名的公式，@focus{程序 @:pn{=} @tech{算法} @:pn{+} @tech[#:key "数据结构"]{结构}}。
它道出了编程学习的核心任务和目标，
其中@tech{算法}已经重点介绍过了，理论上会写说明文就能理解；
@:term{结构}主要指的是@handbook-defterm[#:origin "Data Structure"]{数据结构}，
即@emph{按什么样的格式来组织、存储和操作数据}。

@handbook-action{列表}

谜题故事中、@tech{伪代码}中、以及前面的正文中，
我们已经多次反复提及了“@tech{列表}”这个词，
但并没有违和感。
@handbook-sidenote*{
 嗯？细细想来，汉语似乎并不喜欢直接使用“列表”这个词。
 近年来的网络词汇“扩列”中涉及“好友列表”，
 更像是技术术语搭上了社交软件本土化的顺风车才破圈的。
 有点意思哈。}在日常生活中，
@tech{列表}也是个相当常见的东西，
比如购物小票上的物品清单、餐厅的菜单；
不少人喜欢在特别的时间点写下自己的待办事项、心愿单，等等。

另外，以免你尚未意识到，
本例中的@tech{列表}普通到毫无特色，
就是一列数，这种@emph{
 由数构成的@tech{列表}就是大家熟知的@handbook-deftech[#:origin "Number Sequence"]{数列}
 }@handbook-footnote{严格来说，数列是序列的一种，列表也是序列的一种。
 但这两个“序列”分属于数学和计算机科学两个领域，
 既相关又不完全相同。
 这时候强调这个对初学者不好，但我们确实碰到了术语的混乱。
 为避免读者畏难，我建议大家就按字面意思理解。}。

是吧？无需多言，
直接上@handbook-defterm[#:origin "List"]{列表}的基本特征：

@handbook-sharp-box[
 @handbook-itemlist[
 #:style 'ordered

 @item{一种容器，可以存放很多项内容，项的数量称为@handbook-deftech[#:origin "Length"]{长度}；}
 @item{@tech{长度}没有理论上限，但受限于存储媒介的大小；}
 @item{内容@focus{按顺序}存放，可通过@:term{自然数}@:term{索引}来引用某一项内容；}
 @item{内容@focus{可以重复}出现，重复内容靠存放位置来区分彼此。}
 ]]

这些特征说清楚了@tech{列表}作为一种@:term{数据类型}应该长什么样，
但却没有规定它具体该以什么@:term{结构}形式出现。
比如：你的心愿单是写在日记本里，还是写在许愿签上？
在这两种情况下，
分别该如何添加新愿望、处理已经完成的心愿？

在不同语言、不同场景下，
@tech{列表}并非都叫这个名，
其@:term{存储结构}也可能很不一样。
比如：Python 的列表（@:id{list}）是@:term{顺序表}（类比日记本）；
C++ 的列表（@:id{std::list}）是@:term{链表}（类比许愿签）。
它俩涵盖了绝大多数情况。而在 Racket 中，
列表（@:id{list}）除了可以当成@:term{链表}来用外，
它还是更为重要的基础设施，
术语叫做@:term{广义表}。

对于常规语言的学习者和使用者，
他们很难直观感受到@tech{数据结构}。
但对于使用 Racket 入门编程的初学者，
情况就不一样了。
在语法层面，我们能@emph{直接看}到@tech{数据结构}，
还有什么比@emph{直接用眼睛看}更直观的吗？
上面的“大@tech{列表}套着小@tech{列表}”就是个典型案例：
配对的 @:pn{()} 指示了@tech{列表}的@emph{位置}和@emph{边界}，
以及包含关系。实际上，
它们共同构成的较大的@tech{数据结构}已经超越@tech{列表}本身的范畴了。

且听我讲个古早的传奇故事。

@handbook-action[#:tag "sec:sexpr"]{S-expression}

你已经注意到了，Racket 代码写起来很奇怪，括号套着括号。
是的，它确实奇怪，这也是它不流行的原因之一，
但@focus{这种写法与数学的渊源却颇为深刻}。

高级程序语言起源于上世纪50年代，
但那些无外乎是对机器语言的直白翻译@handbook-sidenote*{
 用英文单词来替代@racket[0]@racket[1]串}，
语法也是互相借鉴。
总之，没有特别亮眼的创新，
特别是缺乏数学的美感。

Racket 是 Lisp 语系的一种方言，
也可以一直追溯到50年代，人工智能研究领域。
至于 Lisp 到底是怎么来的，恐怕已经永远消失在历史的迷雾中了。
现在公认的看法是麻省理工学院的 John McCarthy 于1959年立项，
@focus{直接基于数学而发明的全新的程序语言}。
动机很简单，影响却极为深远。
@idea{程序编制者应该关注问题本身，而不是被迫淹没在琐碎的机器细节里，
 而从人类思维到机器思维之间的脏活，该由程序语言来搞定}。

在 Lisp 的所有方言里，Scheme 这一支最为看重数学理论的优雅和纯粹，
在此基础之上，@focus{为了帮助青少年学习语言无关的编程技能和计算机科学基础知识}
@handbook-sidenote*{2010年代起 Racket 开始弥补在实用性方面的不足，
 我本人也有幸为这个过程尽了一些绵薄之力。
 }，Racket 从自己的教育理念和教学实践中逐渐脱离 Scheme 成为Lisp新兴的第三大方言。

总之，@:thus{现今存在的绝大多数语言，它们宣传中那些出神入化的所谓“新”东西，
都是 Lisp/Racket 玩剩下了才被那些语言的作者发现而抄过去的。}

Lisp 全称@:term{表处理语言}（@litchar{LIS}t @litchar{P}rocessor），
言下之意，@tech{列表}是其基础数据类型，
用它@focus{求解问题的思路也就转化成了对@tech{列表}的操作}。
具体来说，我们只需要弄清楚@emph{对列表做什么}而
不用浪费时间去思考@emph{具体怎么做}。
这就是思维方式的提升。

Lisp/Racket 特殊在，它们连语法都写成了广义表的形式。
任意一类括号（@:pn{() [] @|"{}"|}）都能用来界定广义表，
广义表里的元素可以是常规数据，也可以是另一个广义表，甚至是自己套自己。
这种写法称作@handbook-deftech[#:origin "Symbolic Expression" #:abbr "sexp"]{符号表达式}，
是@focus{函数式递归在文法上的实现}。

@handbook-action{前缀记法 vs. 中缀记法 vs. 五花八门}

我们从幼儿园就开始学习的算式，
其写法属于中缀记法，总是把操作符写在操作数的中间；
而 @tech{sexp} 是一种前缀记法，
@focus{总是把操作符写在操作数的前面}。
前缀记法的优点很多，比如无需考虑运算符优先级；
方便计算机处理。
最关键的是，
它@focus{确实更契合数学运算的语义}。

初等数学中的二元运算符往往也能推广到多元场景。
以加法为例，@:desc{计算十个数的和}，与@:desc{计算两个数的和}，
在计算法则上没有任何区别。
但用中缀记法，需要写十个数、九个加号：
@$${ a + b + c + d + e + f + g + h + i + j }
而写成 @tech{sexp}，只需要十个数、一个加号（以及一对括号充当边界）：
@centered{@racket[(+ a b c d e f g h i j)]}

@handbook-sidenote{@$${
 \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} \\
 \sin^2\theta + \cos^2\theta = 1
}}

实际上，数学学得越深，数学表达式的写法就越五花八门。
比如一元二次方程求根公式和三角恒等式就已经混进去了不少奇怪的东西，
肯定不能都算中缀记法。

还有一些二元运算，没有约定俗成的记号，就只能写成函数形式。
比如求最大公约数的@${ \gcd(a, b) }，
这不就很像前缀记法了吗？括号不能省，还得增加逗号来分隔参数。

@:thus{@tech{sexp} 统一了以上（和其他）所有奇奇怪怪的写法：
 广义表的第一个元素代表操作符（函数），
 其余所有元素都是该操作符（函数）的操作数（参数）}，漂亮。

@handbook-action{累加运算：apply 函数}

在我们这个宇宙中，@:name{累加}运算无处不在。
数学家显然也觉得中缀记法在处理很多个操作数时写起来太麻烦了，
就为@:term{累加运算}专门发明了个简洁的@handbook-deftech[#:origin "Summation"]{求和式}，
配合@tech{数列}或@tech{函数}的名字可显著提高书写的便利：@handbook-sidenote*{
 等号右边的@${...}就是造成@:term{累加}运算写起来太丑的元凶。
}@$${\sum_{i=m}^n a_i = a_m + a_{m+1} + a_{m+2} + ... + a_{n-1} + a_n}
你看，等号左边的表达式很明显也是前缀记法：@handbook-sidenote*{
 Σ 是第十八个希腊字母的大写，英语写作 sigma。
}运算符 @:pn{Σ} 在前，
@tech{数列}或@tech{函数}作为操作对象写在后面。
这个表达式的语义就是等号右边的展开式：
对@tech{数列} @${(a_i)}求和，
从第@${m}项开始到第@${n}项结束。
典型情况下，@${m=1}，
其结果就是@tech{数列}@emph{前@${n}项}的和。

将@:term{求和式}翻译成代码的方法很多很多，
先看最能体现@tech{列表}在@tech{函数式编程}中地位的这种：

@tamer-repl[(code:comment "多操作数加法")
            (+ 1 2 3 4 5 6)
            (code:comment "将多个操作数搜集到列表中，再对该列表应用加法运算")
            (apply + (list 1 2 3 4 5 6))]

在第二个表达式中，
加法操作符（@:id{+}）前面多了一个 @:id{apply} 函数，
后面的参数从多个@:term{操作数}变成了一个@tech{列表}。
函数 @:id{apply} 的功能是将列表里的数按顺序取出，
再一口气全部喂给 @:id{+} 操作符。
以上两个 @tech{sexp} 除了写法不一样，
它们做的事完全相同，
都是@:desc{对多个操作数执行累加运算}。

@handbook-action{高阶函数}

单看这个例子，你现在肯定在想，
@:desc{多操作数加法不是就已经够好用了么，
 为啥还要多此一举再整个@tech{列表}出来}？
因为@tech{列表}可以通过其他途径得到，
一般不会像例子里这样直接写死在代码里。
比如：

@tamer-repl[(code:comment @#,list{读取甲、乙两组精灵的地址编号，并将结果起名为 @:var{IDs}})
            (define IDs ($ read-location-ids < "iSoH/01_hh.aex" > list))
            IDs
            (code:comment @#,list{函数 @:id{first} 和 @:id{second} 可分别获得列表中的第一项和第二项})
            (apply + (first IDs))
            (apply + (second IDs))]

@tamer-figure-margin['apply.dia @list{@:id{apply}函数。
                      @:id{+}函数放置位置靠近底部是为了凸显@:id{apply}的求值结果就是其内部实际干活的@tech{函数}的结果。}]{
 @(geo-scale apply.dia diaflow-marginfigure-scale)}

函数 @:id{apply}自身很好理解，它只需两个参数：
一个@tech{函数}和一个@tech{列表}，
并且那个@tech{函数}能直接接收@tech{列表}中的所有值(@fig-ref{apply.dia})。

@tamer-repl[(code:comment @#,list{@:id{+} @tech{类型签名}里的 @:pn{*} 表明它接收任意数量的 @:type{Number} 型@tech{值}})
            +]

函数 @:id{apply} 引出了@tech{函数式编程}中的另一个重要概念：@tech{高阶函数}。
@handbook-deftech[#:origin "Higher-Order Function"]{高阶函数}的@tech{输入}参数中至少有一个必须是其他@tech{函数}，
或者求值结果是一个@tech{函数}。
它们是@tech{函数式编程}中实现代码@emph{复用}的强力工具，
也是初学者理解@tech{函数式编程}的一道砍。

从现在起，请将这句话刻在脑子里：
@focus{当你从初等数学进入计算机科学时，
 @:term{运算}操作的@tech{值}的类型就远远不只是“数”这么单一了。
 @tech{值}从@emph{信息}中来、到@emph{信息}中去。
}

@handbook-scenario{列表操作}

第一个@emph{主线任务}@racket[|<Puzzle 1: Find Total Distance>|]，
也就是大白话@tech{算法}的第二步，
其内容都可以归结为是对@tech{列表}的几种操作：

@algo-pseudocode[
 #:tag 'desc:find-total-distance "Find Total Distance(大白话版)"
 @list['|sort 1st|]{将甲组的地址编号列表按升序@emph{排序}}
 @list['|sort 2nd|]{将乙组的地址编号列表按升序@emph{排序}}
 @list['|for each|]{@emph{对于}两组列表中的@emph{每一对地址编号}，执行：}
 @list['accumulate]{@hspace[4]计算差距并累加到总差距中}
 @list['output]{@:cmt{; 告知结果}}
]

相对于@algo-ref{desc:read-location-ids}的晦涩，
@algo-ref{desc:find-total-distance}倒是直白很多。
由@tech{缩进}可知，
@algo-ref[#:line 'accumulate]{desc:find-total-distance}归属于@algoref[#:line 'for-each]{desc:find-total-distance}，
它们共同构成了一种符合人类思维习惯的@:term{循环结构}。
换句话说，人类读者在阅读时不用考虑往回跳转，
读一遍就可以继续往后；
计算机会自动重复阅读，
直到循环结束才继续下一行。
代数化之后会更直观地体现这个差别：

@tamer-figure-margin['flow:puzzle1
                     @list{@algo-ref{algo:find-total-distance} 流程图}
                     @(tamer-delayed-figure-apply #:values geo-scale #:post-argv (list diaflow-marginfigure-scale)
                                                  make-hh-p1.dia 'flow:rpcl)]

@algo-pseudocode[
 #:tag 'algo:find-total-distance "Find Total Distance(代数版)"
 @list['input]{@emph{设} @${X}、@${Y} 分别是甲、乙两组地址编号列表}
 @list['|sort x|]{@emph{令} @focus{新}@${X} @:pn{=} @:pn["("]@racket[sort] @${X}@:pn[")"]}
 @list['|sort y|]{@emph{令} @focus{新}@${Y} @:pn{=} @:pn["("]@racket[sort] @${Y}@:pn[")"]}
 @list['sum]{@emph{计算} @${\sum_{i=1}^n |X_i - Y_i|}}
 @list['output]{@:cmt{; 告知结果}}
]

@algo-ref[#:line 'for-each]{desc:find-total-distance
 }和@algoref[#:line 'accumulate]{desc:find-total-distance
 }被替换成了一句@tech{求和式}。
按理说，如此简单的@tech{算法}，
我们本就无需同时提供大白话版和代数版，
本例的大白话版描述可以更优雅地帮你理解@focus{
 @tech{求和式}不是简单表达式，它自带循环结构}。

@handbook-action{累加运算：For Each 循环}

@algo-ref[#:line 'sum]{algo:find-total-distance}复杂在它涉及到了两个@tech{数列}，
没法像前面那样直接向函数 @:id{apply} 借力。
不过呢，这种情况天然适合从翻译的角度来理解，
就如步骤名@algo-ref[#:line '|for each|]{desc:find-total-distance}所言，
正常的英语交流可以用@tt{for each}句式来表达“@emph{对于集体中的每一个}”。
于是，我们有：

@handbook-chunk[<sum>
                (for/sum : Natural ([a (in-list A.sorted-IDs)] 
                                    [b (in-list B.sorted-IDs)])
                  <accumulate>)]

这段代码碎片咋一看有点抽象，
细细读来依稀可见“@tt{sum ... for each ... in list ...}”句式:

@handbook-itemlist[
 #:style 'compact

 @item{语法 @:stx:def{for/sum} 表明可能存在很多种 @tt{for each} 循环，@handbook-sidenote*{
   注意，@litchar{/} 是 Racket 合法的命名字符，
   可解读为名字主体部分(在本例中指@:stx:def{for})的变体。
   }本例中的循环的目的是 @:sym{sum}。即：@:desc{
   累加@racket[<accumulate>]的@tech{值}，
   并最终得到一个类型为 @:type{Natural} 的结果}。}
 @item{每对中括号(@:pn{[]})代表一个 @tt{for each} 子句，每次循环都按顺序取出一个数，并给起个名字。
  是不是在找@tt{each}这个单词在哪？它就指代每一个取出来的@tech{值}：@tt{each a}、@tt{each b}。
  为避免啰嗦，就没必要专门写出来了。}
 @item{函数 @:id{in-list} 表明该子句中的数来源于某个@tech{列表}。
  本例中，提供数的@tech{列表}一定要先排好序。}
 ]

此外，这段代码碎片连@tech{缩进}也跟@algo-ref{desc:find-total-distance}保持一致，
继续@:desc{处理每次循环取出的所有数}，本例中即 @${a} 和 @${b}：

@handbook-chunk[<accumulate>
                (abs (- b a)) (code:comment @#,${|a - b|})]

函数 @:id{-} 就是减法运算符; 函数 @:id{abs} 用于计算绝对值。

注意，语法 @:stx:def{for/sum} 自带@emph{累加}语义，
因此，我们只需要计算本轮循环中待累加的@tech{值}，
不用管这个@tech{值}会被加到哪去。

@handbook-action{主线任务1：find-total-distance 函数}

有了之前完成@emph{辅助任务}的经验，
现在你应该知道从哪看@algo-ref{algo:find-total-distance}的名字了，
与之对应的@tech{函数}名是@:id{find-total-distance}。
同样也是因为简单，无需再为该@tech{函数}配个草稿函数。

@aoc-complain{
 数据型@tech{变量}名通常会以名词或名词的修饰语为主；
 @tech{函数}型@tech{变量}名通常会以谓语为主，
 因为它们通常代表某个动作或某项提问(如@tech{谓词函数})。
 如果名字包括多个单词，
 Lisp/Racket 习惯把分隔单词的空格替换成连字符(@:pn{-})而@:err{不建议}用下划线(@:pn{_})@handbook-footnote{
  这也是 @tech{sexp} 带来的好处，单词中的连字符不会被误读成减号。
  其他语言不得不用下划线以示区分。}。

 顺便，语言词汇的发展遵循一个规律：
 @focus{复合词常常由独立词组演变而来，
  演变的中间阶段就是把空格替换成连字符}。
 比如：@tt{wild life} ⇒ @tt{wild-life} ⇒ @tt{wildlife}。}

@tech{算法}名字应该是对其功能的简短描述：
@:desc{阅读谜题提供的清单文件，计算甲、乙两组精灵所写地址编号的总差距}。
然后从功能描述中提炼出@tech{类型签名}：@:type{(-> Input-Port Natural)}。
即，@:desc{给定一个类型为@:term{输入流}（@:type{Input-Port}）的@tech{输入}参数，
 得到一个类型为@:term{自然数}（@:type{Natural}）的结果}。
于是，配上@algo-ref{algo:find-total-distance}的步骤，
即可得到本任务函数 @:id{find-total-distance} 的全貌：

@handbook-chunk[|<Puzzle 1: Find Total Distance>|
                (define find-total-distance : (-> Input-Port Natural)
                  (λ [locin]
                    |<Read Location IDs>|
                    |<sort IDs>|
                    <sum>))]

虽说本任务是接着@emph{辅助任务}来的，
但为了方便后续检查和提交任务结果，
我还是选择从读取谜题数据开始。
当然，之前的@emph{辅助任务}肯定没有白做，
为它定义的函数 @:id{read-location-ids} 作为本任务的前提，
刚好对应了@algo-ref[#:line 'input]{algo:find-total-distance}：

@handbook-chunk[|<Read Location IDs>|
                (define-values (A.IDs B.IDs) (read-location-ids locin))]

嗯，这里的名字都直接沿用了之前的习惯。
而且，你看，在真实的 Racket 代码中，
@tech{函数}真的可以有多个结果@tech{值}。

@handbook-scene{排序： sort 函数}

看起来很麻烦的@:desc{
 将地址编号列表按升序@emph{排序}
 }其实就跟说出这句话一样简单@handbook-sidenote*{
 如果没有现成的@:id{sort}函数，
 有想过你自己会如何对@tech{数列}排序吗？}：

@handbook-chunk[|<sort IDs>|
                (define A.sorted-IDs (sort A.IDs <)) (code:comment "按升序排列甲组地址编号列表")
                (define B.sorted-IDs (sort B.IDs <)) (code:comment "按升序排列乙组地址编号列表")]

@tamer-figure-margin['sort.dia "函数式 sort"]{
 @(geo-scale sort.dia diaflow-marginfigure-scale)}

函数 @:id{sort} 就是字面意思@:desc{对列表排序}。
注意，它也是一个@tech{高阶函数}，
通过指定@emph{小于}关系运算符来完成对@emph{升序}的配置。
本例中参与比较的都是@:term{自然数}，使用正常的@emph{小于号}(@:pn{<})即可。
其求值结果就是排好序的@focus{新}@tech{列表}，
@focus{并保持原列表不变}。
本例中的新列表拥有新名字以突出@emph{已排序}(sorted)。

此外，由于函数 @:id{sort} 的功能明确，
Racket 能自己@emph{推导}出结果的@emph{类型}，
因此新定义的两个@tech{变量}的@:term{类型}信息可以省略。

@handbook-scene{测试谜题1解法}

第一个谜题求解完毕，先用例题数据检验一下：
@tamer-repl[($ find-total-distance < "iSoH/01_hh.aex" #:expect 11)]

求值的最终结果是 @racket[11]，符合预期。
@handbook-sidenote*{如果结果不对，本书也将不复存在。}

任务数据保存在后缀名为 @litchar{.aoc} 的文件中：
@tamer-repl[($ find-total-distance < "iSoH/01_hh.aoc")]

提交这个答案，解锁第二个谜题。

@aoc-story[@racket[|<Puzzle 2: Find Similarity Score>|]]{
 你的分析只不过是确认了一件大家都知道且害怕知道的事：
 这两组地址清单是真的不一样。或者，是吗？

 精灵历史学者们显然不愿承认是自己组犯了错，
 也不认可对方解读首席历史学家手稿的方式。
 在一片吵闹声中，你发现了件有趣的事：
 两组地址清单里出现了不少重复编号！
 也许大家都理解错了，那些数字压根就不代表地址。

 这一次，你需要找出左边那列每一个数字出现在右边的次数，
 并计算其总的@aoc-emph{相似度(Similarity Score)}。即，
 将左边的每一个数字@aoc-emph{乘上}它在右边出现的次数，
 最后再全部@aoc-emph{加}起来。
  
 问：@aoc-question{What is their similarity score}?}

精灵们是这样的，请先耐下心来习惯他们的毛手毛脚。
于是，根据题意:
@handbook-sidenote{@(tamer-filebox (aoc-tamer-path "iSoH/01_hh.aex") #:path-centerized? #true)}
@handbook-itemlist[
 #:style 'compact
 @item{左边第一个数字是 @racket[3]，在右边出现了三次，所以相似度是 @racket[3] @:pn{*} @racket[3] @:pn{=} @racket[9]；}
 @item{左边第二个数字是 @racket[4]，在右边仅出现一次，所以相似度是 @racket[4] @:pn{*} @racket[1] @:pn{=} @racket[4]；}
 @item{左边第三个数字是 @racket[2]，没在右边出现，所以相似度是 @racket[2] @:pn{*} @racket[0] @:pn{=} @racket[0]；}
 @item{第四个数字 @racket[1]，也没在右边出现，相似度也是 @racket[0]；}
 @item{第五、第六个数字与第一个数字一样，相似度也都是 @racket[9]。}
 ]所以，总相似度是：
@racket[9] @:pn{+} @racket[4] @:pn{+} @racket[0] @:pn{+}
@racket[0] @:pn{+} @racket[9] @:pn{+} @racket[9] @:pn{=}
@racket[31]。

@handbook-action{主线任务2：find-similarity-score 函数}

在对谜题1的求解过程中，
我们体验到了@tech{列表}带来的便利。
现在尝试从@emph{操作列表}的角度来描述第二个@emph{主线任务}@racket[|<Puzzle 2: Find Similarity Score>|]:

@algo-pseudocode[
 #:tag 'algo:find-similarity-score "Find Similarity Score"
 @list['definition]{@emph{设} @${\mathcal{W}_Y(x)} 表示@${x}在@${Y}中出现的次数}
 @list['input]{@emph{设} @${X}、@${Y} 分别是左、右两列地址编号列表}
 @list['|weighted sum|]{计算 @${\sum_{i=1}^n x_i \cdot \mathcal{W}_Y(x_i)}}
 @list['output]{@:cmt{; 告知结果}}
]

@tamer-figure-margin['flow:puzzle2
                     @list{@algo-ref{algo:find-similarity-score} 流程图}
                     @(tamer-delayed-figure-apply #:values geo-scale #:post-argv (list diaflow-marginfigure-scale)
                                                  make-hh-p2.dia 'flow:rpcl)]

能看出来，谜题2比谜题1还简单一些，无需@emph{排序}，会@emph{数数}足已。
因此不必强行分别给出其解谜@tech{算法}的大白话版和代数版了，
直接上该任务@tech{函数} @:id{find-similarity-score} 的全貌：

@handbook-chunk[|<Puzzle 2: Find Similarity Score>|
                (define find-similarity-score : (-> Input-Port Natural)
                  (lambda [locin]
                    |<definition>|
                    |<Read Location IDs>|
                    (for/sum : Natural ([x (in-list A.IDs)])
                      <score>)))]

同样，本任务也是接着@emph{辅助任务}来的，
因此直接包含代码碎片@racket[|<Read Location IDs>|]获得两个@tech{数列}；
语法 @:stx:def{for/sum} 自带@emph{累加}语义，
因此，每轮循环给@emph{相似度}的打分环节就是最直白的乘法运算：

@handbook-chunk[<score>
                (* x (weight x))]

好吧，这段代码碎片里的乘法不算特别直白。
它认为@algo-ref[#:line 'definition]{algo:find-similarity-score}定义的@tech{函数}
@${\mathcal{W}_Y(x)}的全名应该叫 @:id{weight}，
直译为“重量”，在数学和统计学中引申为“权重”。
所以，看出来谜题2中的数学部分其实是@:term{加权求和}了吗？

@handbook-scene{数权重：count 函数}

@:term{加权求和}作为一种统计学方法，
其核心是如何计算@:term{权重}。
因此，程序语言没法直接提供这样一个@tech{函数}，
需要我们根据问题情景自己定义。
任务做到现在，
定义@tech{函数}这事早就不陌生了，
而且这一次@tech{函数}名已经确定了，
照搬自统计学术语。
接下来是描述其功能：@:desc{
 给定左边@tech{列表}里的一个数，
 得到这个数在右边@tech{列表}中出现的次数}。
由此可以提炼出其@tech{类型签名}：@:type{(-> Natural Natural)}。
即，@:desc{给定一个 @:type{Natural} 型的@tech{输入}参数，
 得到一个 @:type{Natural} 型的@tech{输出}结果}。

在本例中，确定@:term{权重}的方法就是最直白、最原始的@:term{数数}，
毕竟我们才刚刚开始解决精灵们的问题，
手头已有的信息只够我们做此尝试。于是，

@handbook-chunk[<definition>
                (define weight : (-> Natural Natural)
                  (lambda [x]
                    <count>))]

好消息是，在@tech{列表}中数数是常见操作，
提供@tech{列表}的语言不顺便提供 @:id{count} 函数就太磕碜了。
坏消息是，@:id{count} 也是一个@tech{高阶函数}，
你不得不思考得深一些。不过，
鉴于 @:id{count} 函数的@tech{类型签名}对初学者来说太过犯规，
这里就不放出来了，我针对本题给个简单版本：
@handbook-sidenote*{注意，这段代码碎片没有包含在任何其他碎片中，因此不会出现在最终代码里。}

@handbook-chunk[|<count 函数 简易类型签名>|
                (-> (-> Any Boolean) (code:comment "参数1类型：过滤函数")
                    (Listof Natural) (code:comment "参数2类型：自然数列表")
                    Natural)]

跟着注释不难看出，
@:id{count} 函数接受一个过滤用的@tech{谓词函数}和一个 @:type{(Listof Natural)} 型的@tech{值}，
得到一个 @:type{Natural} 型的结果，而该结果正是原列表中能够满足过滤函数的项的数量。
理清这一点后，如何@emph{数数}就不难理解了：

@handbook-chunk[<count>
                (count <数数用谓词函数> B.IDs)]

函数 @:id{count} 实际做的事是：@:desc{
 按顺序把@tech{列表}@:var{B.IDs}里的项取出来，
 问@tech{谓词函数}这一项是否符合要求，
 如果是就@${+1}，不是就当没看见}(@fig-ref{count.dia})。

@tamer-figure-margin['count.dia @list{@:id{count}函数}]{
 @(geo-scale count.dia diaflow-marginfigure-scale)}

代码碎片@racket[<definition>]的位置隐藏着一些有趣的点：

@handbook-itemlist[
 #:style 'compact

 @item{@focus{你可以在一个@tech{函数}内部定义另一个@tech{函数}}。
  这事咋一看很奇妙，仔细一想却又很稀松平常。
  定义@tech{函数}的本质只是在给@tech{函数}起名字，
  这在@tech{函数式编程}中就像呼吸一样自然。
  即使你不做，函数式语言也很可能瞒着你悄悄做了不少。
  比如：

 @itemlist[
 #:style 'compact
 @item{给@:stx:def{let}起名就是在@emph{定义}一次性函数；}
 @item{不起名直接用@:sym{λ}就@emph{产生}了一个@tech{匿名函数}。}
 ]}
 
 @item{@focus{内部@tech{函数}自动共享外部@tech{函数}的@tech{变量}}。
  所以，我们不必每次都告诉 @:id{weight} 要从中数数的那个@tech{列表}是哪个。}
 ]

那么，是不是该给 @:id{count} 函数定义一个@tech{谓词函数}了？
不是，不用@emph{定义}，只需@emph{产生}一个@tech{匿名函数}即可，
因为这个@tech{谓词函数}只有 @:id{count} 函数会用到。
换句话说，@focus{@tech{匿名函数}和@tech{高阶函数}是绝配}，
今后我们还会学习其他更数学的产生@tech{匿名函数}的方法。
于是，

@handbook-chunk[<数数用谓词函数>
                (λ [[y : Natural]] : Boolean
                  (x . = . y))]

嗯？你困惑等号(@:pn{=})在这的用法吗？
它就是用于检查两个数是否相等的@emph{关系运算符}：

@tamer-repl[(:query-type/args = Number Number)]

此外，因为@tech{匿名函数}没有名字，
我们在标记其@tech{类型签名}时就不能用之前的方法了，
但仍然还是用冒号(@:pn{:})语法：
直接给每个@tech{输入}参数标记类型，
最后在参数列表后面跟上@tech{输出}结果的类型。
这种方法也适用于其他定义@tech{函数}的场合。

@handbook-scene{测试谜题2解法}

谜题2求解完毕，先用例题数据测试一下：
@tamer-repl[($ find-similarity-score < "iSoH/01_hh.aex" #:expect 31)]

然后是任务数据：
@tamer-repl[($ find-similarity-score < "iSoH/01_hh.aoc")]

提交这个答案，解锁明天的任务。

@handbook-scenario[#:tag "sec:fp"]{函数式编程}

第一天漫长的挑战终于完成，可以正式了解一下什么是@tech{函数式编程}了。

@handbook-deftech[#:origin "Functional Programming"]{函数式编程}讲究@emph{
 总是通过定义一系列函数来教会计算机解决问题}，
这里的“函数”指的是@focus{严格意义上的数学函数}。
如果中学数学课不会导致你瞌睡，
那你已经能够理解数学函数的典型特征：

@handbook-sharp-box[
 @handbook-itemlist[
 #:style 'ordered
 
 @item{通过@focus{代数变换}规则和流程，将一系列@tech{输入}@tech{值}转化为一个@tech{输出}@tech{值}。}
 
 @item{有@:term{定义域}和@:term{陪域}来约束@tech{输入}和@tech{输出}。}

 @item{@focus{不依赖函数外信息}。
  任何人在任何时间、任何角落，
  只要@tech{输入}相同，@tech{输出}也一定相同。}
 
 @item{@focus{不产生对函数外可见的影响}。
  小到修改了某个全局@tech{变量}的值，
  或者弹个窗口告诉你任务完成，
  大到让病毒逃离实验室结果毁灭了全世界。}

 @item{可以通过复合产生@tech{高阶函数}。}
]]

发现特征1和前面介绍的@tech{算法}很相似了吗？
到目前为止，我们一直在做一件事：
@focus{变着花样用不同方式、从不同角度描述解谜的@tech{算法}}。
当用高级程序语言来描述@tech{算法}时，
@tech{算法}会被翻译成一个个叫做@handbook-deftech[#:origin "Function"]{函数}的东西。

违背特征3@emph{或}特征4的函数视作有@handbook-deftech[#:origin "Side Effect"]{副作用}。
根据函数有无@tech{副作用}，
我们至少可以把@:term{思维范式}和@:term{代码风格}粗略划分为@tech{指令式编程}和@tech{函数式编程}。

@handbook-action{指令式 @${x = x + 1}}

如果你上过常规编程课，
你学的就是@focus{有}@tech{副作用}的@handbook-defterm[#:origin "Imperative Programming"]{指令式编程}，
把@tech{算法}翻译成一条条指令发给计算机执行。
比如，@tech{变量}代表的是内存中的某个存储位置，
一句“@${x = x + 1}”涵盖了“读取”、“加1”、“写入”三条指令(@fig-ref{flow:mutation})。

@tamer-figure-margin['flow:mutation "经典“赋值运算”"
                     (geo-scale hh-mutate.dia 0.36)]

@aoc-complain{
 这对于从小学着代数长大、做了无数卷子却没多少工科实践类课程的初学者来说，
 它相当于在做题时要不断“用修正液涂掉旧答案、写上新答案”，
 @:thus{检查时草稿纸上已经没有任何做题痕迹了}。
 另一方面，数学老师普遍要花大力气才能让学生正确理解@emph{
  等号（@:pn{=}）@:err{不表示}“经过计算得到一个值”}，
 然后编程老师说@emph{要先计算等号右边的值，再放回左边的存储空间里}，
 就……很拧巴……

 别气馁，我也经历过这样的瞎折腾。
 好消息是，@tech{函数式编程}会带我们回归数学思维。
 因其本质是@focus{代数替换}，
 因此@:thus{等号（@:pn{=}）就是关系运算符，没有@:term{赋值}语义}。
 比如，@racket[<数数用谓词函数>]。}

那么问题来了，既然“修改变量值”也算@tech{副作用}，
@tech{函数式编程}中岂不是就没有@tech{变量}只有@tech{常量}了？
好问题，但是答案出奇的简单：
程序中的@handbook-deftech[#:origin "Constant"]{常量}其实只是@focus{约定了}@:term{不可变}的@tech{变量}。
但我觉这个答案还应该说得更细致一些：

@handbook-itemlist[
 #:style 'compact
 
 @item{@focus{函数式语言的@handbook-deftech[#:origin "Variable"]{变量}也是数学变量}，
  即方程、代数表达式中出现的诸如@${x}、@${y}、@${z}之类代表@:term{未知数}的符号。
  在不同的前提条件下，它们可以取不同的@tech{值}，是为“变”；
  然而，一但条件确定，它们的@tech{值}也就不再改变，
  进而导致对函数的求值结果也不会改变。

  @aoc-complain{打个比方，
   数学@tech{变量}相当于身份证上的姓名，
   不同人的名字可以相同可以不同，
   但每个人的名字在第一次确定之后（一般）不可更改，
   当交谈中提到你认识的人的名字时，
   你脑海里浮现的也是@focus{记忆中}那个人活灵活现的形象；
   与之相对的，指令式语言的@tech{变量}相当于酒店房间的门牌号，
   房间里面有谁取决于何人能入住，
   提到这个门牌号时，很可能已经不是原来的住户了。}}

 @item{指令式语言的@tech{变量}之所以代表存储位置，
  根本原因是因为我们现在@focus{最常见的计算机组成结构的核心仍然是存储设备，
  一切程序代码和数据都必须安排存储空间才能参与计算，函数式语言也不例外}。
  因此当你要深究函数式程序在执行时都偷偷瞒着你做了啥见不得人的事时，
  你会惊讶地发现，它们的@tech{变量}也都有自己在内存的地址。
  差别在于，@focus{函数式语言不提供方法（或者明确建议你不要）直接修改变量的值，
   如果你遵守这个约定，你的程序运行起来会更高效和可靠}。
                               
  @aoc-complain{在此语境下打个比方，
   指令式语言的@tech{变量}仍是酒店房间的门牌号，
   函数式语言的@tech{变量}则是家庭地址的门牌号@handbook-footnote{
    身份证上确实写着你户籍地的家庭地址}。
   显然，酒店房间人来人往，你不能假设里面住着谁；
   但家庭地址比较稳定，你可以合理假设里面的住户不会轻易改变。
   现在你要找人，或者寄快递，哪类地址更可靠？}}
 
 @item{不与真实世界交流的程序，
  连接受键盘输入、将结果显示在屏幕上都做不到。
  这样的程序没有任何价值。
  也就是说，@:err{@tech{副作用}不可避免}，
  真实世界的@tech{函数式编程}必定分为两个部分：

  @handbook-itemlist[
 #:style 'compact

 @item{纯净无@tech{副作用}的部分，@idea{这部分占比应该尽可能大}，让数学感受到她应有的排面。}
 @item{有@tech{副作用}的一小部分，专门去做那些数学搞不定的脏活@handbook-footnote{
     或者可以像 Haskell, Lean 那样，借用更高深的数学理论(甚至自己开创一个数学分支专门用于)将@tech{副作用}隐藏起来。}。}]
  }]

@idea{初学@tech{函数式编程}应该以回归数学思维为主，@handbook-sidenote*{
 此结论也适用于用数学观点看待科学
 }(至少暂时)不要被机器牵着鼻子走，
 但心里要清楚数学不是全能之神，
 该变通的时候还是要变通的}。
实际上，现代高级程序语言很少只能用一种风格，
差别在于他们建议的主风格是什么，
又提供了多少便利帮助你写出他们建议风格的代码。

本例中，唯一有@tech{副作用}的代码是 @racket[(read locin)]，
因为它依赖谜题提供的数据文件，违背了特征3。

@handbook-action{函数式 @${x = x + 1}}

@handbook-sidenote*{这很可能是你第一次碰到@emph{大白话@tech{伪代码}描述能力不够}的地方。
 人类对话用的“自然语言”并不擅长描述数学、科学这样“不自然”的东西。}
用数学语言该如何表达@tech{指令式编程}的@${x = x + 1}这样的迷惑操作呢？
@algo-ref[#:line (cons 4 5)]{
 algo:rpcl}和@algo-ref[#:line (cons 1 2)]{
 algo:find-total-distance}都在不断定义@focus{新}@tech{变量}，
这……看着就很灾难，太不优雅了。

@handbook-scene{递推关系}

不妨换个思路，开个上帝视角，@focus{一个@tech{变量}，
 随着时间的推移而被赋予不同的@tech{值}，
 我们把这些@tech{值}按时间先后顺序写下来，
 就是个典型的@tech{数列}}，记作@${x_n}。
这个思路的问题在于，
很多场景下，我们只能知道@tech{变量}的初始值(或初始的几个值)，
而无法事先知道它所有的后续取@tech{值}。
倒是会经常尴尬地说“以此类推”或“重复上述过程”，
很不讲究，计算机理解不了直接撂挑子不干你受不受得了？

数学提供的解决方案是@handbook-deftech[#:origin "Recurrence Relation"]{递推关系}:
找到一个@tech{函数}充当@:name{递推公式}，
使它能够根据@tech{变量}的当前@tech{值}计算出下一个@tech{值}。
于是，指令视角的@${x = x + 1}在@tech{函数式编程}中的等价表述是@${x_{n+1} = x_n + 1}。
按照数学惯例，以@emph{下标}形式书写的@emph{索引} @${n} 是@emph{正整数}；
@${x_1} 作为该数列的@emph{首项}，其@tech{值}应当已知；
最后，@:term{递推公式}是 @focus{@${+ 1}}。

等式 @${x_{n+1} = x_n + 1} 强调的是@emph{后一项由当前值递推而来}，
换个说法也就是@emph{当前值可由前一项递推而来}。
于是，如果@${x_1 = 0}，我们有
@$$={x_n =
 0             & n = 1,\\
 x_{n - 1} + 1 & n \ge 2.}
随着时间的推移，
@tech{变量} @${x} 的@tech{值}依次是 @${0, 1, 2, 3, 4, 5, ... }。
现在把它们直观化为@fig-ref{numberline}的@emph{数轴}：

@tamer-figure!['numberline
               @list{数轴，一生二、二生三、三生万物}
               @(plot-axis #:tick-range (cons 0 7) #:reals '(0 1 2 3 4 5 6 (7 . arrow))
                           #:real->sticker hh:add1-sticker
                           #:axis-color 'Orange
                           #:axis-label "x"
                           400 0.0 42.0)]

箭头方向和颜色渐变方向都说明了@focus{右边是正向}。
这条数轴取名为@${x}，
表明@${x_n}的@tech{值}就是数线下方与@${x}齐平的每个@emph{非负整数}@handbook-footnote{
 按照数轴惯例，@emph{自然数}指的是数轴上 @racket[0] 点右边的等距离点位。}点：@handbook-sidenote*{
 今后当你困扰于复杂的递推关系时，别忘了回忆起这里我们开始的地方，没准就悟了呢。
}从 @racket[0] 开始，每应用一次@emph{递推公式}，
就往右边跳一步，进而落在了下一个@emph{自然数}点上。
每次递推都碰巧相当于完成了一次数数，这的神奇之处在于，
虽然@emph{自然数}无穷无尽我永远也数不完，
但只要你敢报个数，
我就肯定能接着你的数往大了数，
你说气不气人？

@handbook-scene{迭代}

道理我都懂，可是这跟@tech{函数}有什么关系？
估计是“@${+ 1}”这个写法让你困惑了，
给它取个高级点的名字吧，
叫做 successor(@emph{后继}，意为“下一个”)，缩写为 @${s}。
其功能是，当我们给这个@tech{函数}喂一个@emph{非负整数}时，
它就会吐出这个数的“下一个”@emph{整数}给我们。
即，@$$[#:tag "successor"]{s(x) = x + 1}
于是，从首项开始，一遍一遍重复使用它：@$${
 s(0) \rightarrow 1,\\
 s(1) = s(s(0)) \rightarrow 2,\\
 s(2) = s(s(1)) = s(s(s(0))) \rightarrow 3,\\
 ...}
现在可以把“以此类推”替换成@${x_n = s(x_{n-1})}了，有@tech{函数}了吧？

而且，这个过程有个名字，叫做@handbook-deftech[#:origin "Iteration"]{迭代}，
它强调的是@emph{重复使用某个@tech{算法}}的动态过程，
@focus{每一轮重复产生的@tech{值}都是下一轮重复的@:term{初始值}}。

@handbook-scene{递归}

递推思路顺便带来了两个看似平平无奇、实则奥妙无穷的能力：

@handbook-itemlist[
 #:style 'compact

 @item{从@:term{初始值}开始，可以一直计算@tech{变量}的“下一个”@tech{值}，
  术语叫做@handbook-deftech[#:origin "Forward Substitute"]{前代}。
  于是，@:thus{我们得以用有限的语言来描述和理解无限的概念}。}

 @item{可以一直反推@tech{变量}的“上一个”@tech{值}，直到到达@:term{初始值}，
  术语叫做@handbook-deftech[#:origin "Back Substitute"]{回代}。
  于是，@handbook-sidenote*{俗称套娃
   }@:thus{我们得以将复杂问题拆解成规模更小的同类子问题以各个击破}。}]

以上能力在数学和计算机科学两个领域都应用广泛，最次可以@focus{教会计算机解决复杂问题}，
更是@focus{教计算机通过归纳法验证@tech{算法}正确性、证明数学定理}的地基。
在程序语言中，它俩统一结晶为@handbook-deftech[#:origin "Recursion"]{递归}，
实现为@tech{函数}就是那类@emph{会直接或间接调用自己}的@handbook-deftech[#:origin "Recursive Function"]{递归函数}。

顺带一提，@tech{递推关系}强调的是@emph{@tech{数列}中一个或多个连续项之间的函数关系}。
常见情形下，它就是前一项和当前项的关系，但这不是硬性约束。
@:term{斐波那契数列}就是个经典例外，它用前两项来计算当前项，
把它写成@tech{递归函数}就是:
@$$={Fib(n) =
 1                   & n = 1,\\
 1                   & n = 2,\\
 Fib(n-1) + Fib(n-2) & n \ge 3.}
在等号(@:pn{=})右边，@tech{函数} @${Fib} 自己出现了两次。
实际上，除了这种有规律的，
@tech{递归函数}还可以用任何合理的参数随意调用自己。

时至今日，@tech{递归函数}已经是高级程序语言的标配。但是，
@tech[#:key "指令式编程"]{指令式语言}的@tech{递归函数}除了套娃开销大外，
还有个致命缺陷@handbook-footnote{指令式语言也可以有尾递归优化，
 但你用的那个语言有没有这个优化就全看运气了。}：
套娃次数太多会招致程序奔溃。
因此，@:thus{@tech[#:key "指令式编程"]{指令式语言}往往优先使用循环来解决递归问题}；
@tech[#:key "函数式编程"]{函数式语言}通过@:term{尾递归}技术根除了这个缺陷。
反倒是普通循环不被函数式待见，因为普通循环必然要修改@tech{变量}，
摆明了就是要跟@focus{数学变量不可变}原则对着干啊。

一言以蔽之，
@:thus{@tech{递归函数}就是用数学语言描述的循环，
 每一轮循环都是在调用@tech{函数}自身}。

@handbook-action{数列也是函数！}

@fig-ref{numberline}只刻画了旧@tech{值}和新@tech{值}之间的@tech{函数}关系，
也能得到我们需要的全部@tech{值}。
但如果我问，程序运行到某一时刻时，@tech{变量}的@tech{值}是多少？
这才是我们要定义@tech{变量}、收集全部@tech{值}的动力。
要回答这个问题，我们需要对@fig-ref{numberline}稍微变个形，
由此得到了@fig-ref{nl:sub1}，
你看看有哪里不一样？

@tamer-figure!['nl:sub1
               @list{数轴，但@:err{不该这么用}}
               (plot-axis #:tick-range (cons 0 8) #:reals sub1
                          #:real-position 0.618 #:real-anchor 'cb
                          #:real-color 'Crimson
                          #:axis-color 'RoyalBlue
                          #:real-exclude-zero? #true
                          #:axis-label "n"
                          400 0.0 42.0)]

@fig-ref{nl:sub1}的数轴的名称变为了@${n}，
表明线下方与@${n}齐平的数字代表的是@${x_n}各@tech{值}的@:name{索引}，
也就是“第@${n}个@${x}”的@${n}；
@${x_n} 的@tech{值}则被标在了线的上方。
如果这些@tech{值}不是数字，那可能问题不大，
但像现在这样肯定不行，
数轴的同一个点位怎么能对应着两个不同的数字呢？
太有歧义了。

因此，再变一次形我们就得到了@fig-ref{tl:sub1}。
当然，这已经不是数学上用到的那个@emph{数轴}了，
为表区分，姑且叫做@:name{时间轴}吧，
@${n}代表“第@${n}个需要知道变量@tech{值}的时刻”。

@handbook-sidenote{
 到这里如果你还是不能理解@tech{函数}是什么，
那就先看看@fig-ref{tl:sub1}找找灵感，
哪些元素能启发你理解@tech{函数}这个概念?
@linebreak[]
@linebreak[]
本书会将只有一个@tech{输入}参数的@tech{函数}画成类似沙漏的形状，
沙漏两端的箭头表明@tech{输入}@tech{值}从一端进入，
在内部经过一番折腾(@tech{处理})，
从另一端出来一个@tech{输出}@tech{值}。}

@tamer-figure!['tl:sub1
               @list{函数式@tech{变量}的@tech{时间轴}模型}
               (plot-axis #:tick-range (cons 0 8) #:reals sub1
                          #:real-position -2.0 #:real-anchor 'ct
                          #:real->sticker (make-timeline-real->sticker "x(n)" 8)
                          #:real-color 'DarkCyan
                          #:real-exclude-zero? #true
                          #:axis-label "n"
                          400 0.0 42.0)]

一个只会数数的程序没多大的用处，
但如果每数到一个数，
就把它扔给一个@tech{函数}，
静静等待这个@tech{函数}吐出另一个数，
情况就不一样了，
因为你有机会得到任何数。
@:thus{这其实就是@tech{数列}的本质：
 一个专门用于将@emph{正整数}转化为别的数的@tech{函数}。
}即，@racket[(-> Positive-Integer Complex)]。
@tech{函数}名也跟@tech{数列}名相同，
写法上略有差异：从 @${x_n} 变为 @${x(n)}。
在@tech{数列}术语中，
这样的@tech{函数}也被称为@:name{通项公式}。

现在，你可以用很文艺的方式跟你的小伙伴炫耀@tech{函数式编程}的基本特征了：
在@tech{函数式编程}的世界里，@idea{每个@tech{变量}都自带一条@tech{时间轴}，
@tech{时间轴}上的关键节点对应着该@tech{变量}在程序运行时的不同取@tech{值}。
}而运行程序的过程也像极了真实世界的时间流逝，
@idea{今日事今日毕，明天又是崭新的一天；
如此往复，虽从未曾惊扰昨日，一切已然悄悄变化。}

@handbook-action[#:tag "sec:rloop"]{递归式循环}

再看代码碎片@racket[|<read-predicate-construct loop>|]，
它定义了一个名为 @:id{rpcl} 的二元@tech{递归函数}，
用于构造谜题中的地址编号@tech{列表}。

当@tech{变量}是数值类型时，
其@tech{时间轴}构成一个@emph{数}的@tech{列表}（@:type{(Listof Number)}），
每个@tech{值}都是@emph{数}（@fig-ref{tl:sub1}），
因而@:term{递推公式}经常就是大家已经烂熟于心的算术运算；
@:id{rpcl} 的参数已经是@emph{自然数列表}类型了，
其@tech{时间轴}应该构成一个@emph{自然数列表}的@tech{列表}（@:type{(Listof (Listof Natural))}），
每个@tech{值}也都是一个@emph{自然数列表}。
更直观的类比即@tab-ref{rrcmp}：

@tamer-table!['rrcmp
              "递推关系：从数值类型到列表类型"
              @tabular[
 #:sep @hspace[1]
 #:column-properties '(right center)
 #:row-properties '((top-border bottom-border) bottom-border)
 (list (list @bold{递推函数} @bold{@tech{值}类型} @bold{@emph{初始值}} @bold{@tech{值}序列类型} @bold{操作符} @bold{必要操作数})
       (list @${s(x) = x + 1}
             @:type{Natural}
             @racket[0]
             @:type{(Listof Natural)}
             @:id{+}
             @racket[1])
       (list @:id{rpcl}
             @:type{(Listof Natural)}
             @:val{null}
             @:type{(Listof (Listof Natural))}
             @:id{cons}
             @smaller{@racket[(read locin)]}))]]

@handbook-itemlist[
 #:style 'compact

 @item{@tech{列表}类型的@:term{初始值}是 @:val{null}，代表@emph{空列表}；}
 @item{@tech{列表}类型的@:term{操作符}是 @:id{cons}(即 @litchar{cons}truct)，
  用于构造“下一个”新@tech{列表}。@handbook-sidenote*{
   @algo-ref[#:line (cons 4 5)]{algo:rpcl}的列表构造语法借用自 Haskell。
   }并且保证@emph{必要操作数}被添加到原列表的头部。}
 ]

这有个细节，@:term{递推公式}只接收一个@tech{输入}参数，
并且@:err{不宜}选@tech{恒等函数}。
因此，单独一个 @:id{+} 不是数值类型的@:term{递推公式}；
@:id{cons} 也不是@tech{列表}类型的@:term{递推公式}。
@:id{rpcl} 本尊才是本例用于构造@tech{列表}的@:term{递推公式}，
在其内部雇佣了 @:id{cons}，
另一个@emph{必要操作数}则由 @:id{read} 函数从@:term{输入流}中读取。

@:id{rpcl} 复杂就复杂在，它有@tech{副作用}，@handbook-sidenote*{
就当读取两次文件，一次只构造一个@tech{列表}。
}并且要同时构造两个@tech{列表}。
好在，这两个@tech{列表}互相之间没有干扰，
我们可以放心地把它当成@emph{两个}一元函数。
根据@tech{函数式编程}对@tech{递归函数}和普通循环的偏好，
以乙组精灵的地址编号@tech{列表}为例，
我们对比一下@eqref[#:label "递推函数"]{successor}和 @:id{rpcl} 的@tech{迭代}过程。
@:id{rpcl} 首秀发生在@racket[<|initialization!|>]。

@handbook-sidenote{@(tamer-filebox (aoc-tamer-path "iSoH/01_hh.aex") #:path-centerized? #true)}

@racketblock[
 (code:comment @#,list{类比： @${s(0) \rightarrow 1}})
 (rpcl null) #,(elem @${ \rightarrow (\text{list}~4)})]

之后会在@:desc{每一轮循环结束时再次对@focus{自己}求值以启动下一轮循环}。
而@tech{迭代}的性质决定了，
@:desc{本轮循环结束时的变量@tech{值}即是下轮循环中变量的@emph{当前值}}。

@racketblock[
 (code:comment @#,list{类比： @${s(1) \rightarrow~2}})
 (rpcl (list 4)) #,(elem @${ \rightarrow~(\text{list}~3~4)})
 (code:comment @#,list{类比： @${s(2) \rightarrow~3}})
 (rpcl (list 3 4)) #,(elem @${ \rightarrow~(\text{list}~5~3~4)})]

继续可得其完整@tech{时间轴}：

@tamer-figure!['tl:cons
               @list{@tech{时间轴}，@tech{逆序}构造乙组精灵写的地址编号列表}
               (plot-axis #:tick-range (cons 0 7) #:reals #(null (4) (3 4) (5 3 4) (3 5 3 4) (9 3 5 3 4) (3 9 3 5 3 4))
                          #:real-position -2.0 #:real-anchor 'ct
                          #:real->sticker (make-timeline-real->sticker "ID(n)" 7 0.618)
                          #:real-color 'DarkCyan
                          #:real-exclude-zero? #true
                          #:axis-label "n"
                          400 0.0 48.0)]

前文在引入@tech{时间轴}模型的时候，有个点说得比较模糊，
@:desc{程序运行之后的某个时刻}到底是什么时候？一秒、一小时、一天？
显然都不是，我们重点在强调用@tech{递归函数}替代@:term{普通循环}，
那这个@focus{时刻}当然就是指@focus{每一轮循环开始的时候}。

最后再来对比一下，@:id{rpcl}
内部代码碎片@racket[|<地址编号扩列，迭代下一轮>|]是如何雇佣 @:id{cons} 计算“下一个”@tech{值}的：

@racketblock[
 (code:comment @#,list{@${s(x)} 本体在等号右边：@${x + 1}，交换操作数即： @${1 + x}})
 (code:comment @#,list{如果中缀表达式看不出来，换成前缀写法:})
 (+    1 x)
 (cons b B.IDs)]

注意，@:id{rpcl} 不像@eqref[#:label "递推函数"]{successor}那样可以无限@tech{迭代}下去，
文件读完就结束了。
因此，@:id{rpcl} 的完整本体需要自行判断结束条件。

总之，
变量 @:var{A.IDs} 和 @:var{B.IDs} 的@tech{值}至始至终都没有变过，
只不过它们在各自循环中的使命完成之后就不再现身，
而被下一轮循环中的自己@:term{替换}掉了。
就好比电影续作中的主角还在，
也仍然叫那个名，
但是换了个新演员。

@handbook-action[#:tag "sec:iloop"]{迭代式循环}

本节内容的一大假设是@focus{@tech{变量}在程序生命周期内的所有取值都可以通过某个@tech{函数}计算得到}。
那么，这个假设有多靠谱呢？存不存在就是没有规律的@tech{变量}？

为了让你有事可做，@handbook-sidenote*{
 如果你碰到了允许你挑错的教材或老师，那你是有福的。
 别憋着，舞起来吧。
 }数学课布置的作业一定会给你有规律的@tech{数列}来刁难你。
@focus{但规律并不是@tech{数列}的固有属性}，
确实存在没有规律的@tech{数列}。
比如，任意无理数小数点后第@${n}位数字构成的@tech{数列}。
这些@tech{数列}没有规律，
但你总有方法可以@emph{一个个}写出它的前@${n}项。

放到程序中来，少数无规律@tech{数列}可以写死在代码里，
更多的是从文件、网络、数据库等存储设备里读取。
本书涉及的所有谜题数据都来自于文件，
我们解谜过程的第一步就是读取它们，
然后保存在诸如@tech{列表}这样的@tech{数据结构}里。

前文我们费了一番劲终于能够理解，
@:id{rpcl} 函数如何卖力地完成了“读取数据”的壮举。
于是你得到了两个无规律但干净纯粹的@tech{数列}，
然后把它们喂给 @:id{sort} 函数、@:id{count} 函数
和 @:stx:def{for/sum} 语法，
并最终得到谜题答案。

你猜，它们会怎么处理这俩@tech{数列}？
@handbook-sidenote*{@:id{sort} 函数有点复杂，暂且不表。
 但你入门后仍然可以用不超过本章的知识来解释。
}在数学上，@tech{数列}可以无限长，
但在程序中，
一个确定的@tech{列表}一定是有限的，
也就一定有办法从头到尾获得它的每一个@tech{值}。
是不是嗅到@tech{迭代}的味道了？
是的，像 @:stx:def{for/sum} 这样的语法结构，
像 @:id{count} 这样处理@tech{列表}的@tech{高阶函数}，
它们要么是典型的迭代式循环，
要么内部包含一个典型的迭代式循环。

如果你熟悉@tech{指令式编程}，你很可能在想，
@:desc{咦，这不就是@emph{普通循环}吗？我可太熟了}。
不，你没有，在@tech[#:key "函数式编程"]{函数式语言}里，
@focus{它们完成@tech{迭代}的方式都是@tech{递归}，
 而且@tech{迭代}完之后原始@tech{列表}仍维持原样}。
写出来就跟我们自己写的 @:id{rpcl} 函数很像，
毕竟，@tech{列表}(和其他@tech{数据结构}都)有不少标准操作，
重复代码写多了谁都会腻的，
语言不解决这个问题的后果会很严重。

@aoc-complain{
 @tech{递归}和@tech{迭代}是@tech{算法}设计的基础工具，
 但如何正确区分这两个术语却不是那么容易。
 不管怎么说，在解决实际问题的过程中，它俩经常互相协作：

 @handbook-itemlist[
 #:style 'compact

 @item{你在@focus{纸上}用@tech{迭代}法从@tech{递推关系}中归纳出@tech{通项公式}，
   其本质是将@tech{递归函数}转化为等价的不带@tech{递归}的普通函数，
   使得你和计算机都能一步到位直接计算出结果，而不必专门写个循环程序来求解。}

 @item{你直接把@tech{递归函数}交给计算机、让它计算某个值，
   计算机知道如何@tech{迭代}出结果。}
 
 @item{你经常会嫌计算机太笨，但是自己又懒、不想去找精炼的描述……
   只好把@emph{繁琐的@tech{迭代}方法}写下来，直接告诉计算机：
   如此如此，这般这般，就能怎么怎么滴。}]

 以上三个例子，显然都跟@tech{指令式编程}和@tech{函数式编程}@:err{没有}直接关系。
 @tech{指令式编程}训练往往会误导人，把@tech{迭代}狭义地理解为“普通循环”,
 这或许就是@idea{语言影响、塑造思维}的有力证据吧，
 还是负面证据，哈哈哈哈哈……
}

@handbook-scenario{闭幕}

文中出现了好几处理论性较强的段落，
你现在看不懂才是正常现象，
有待我们在后续任务中慢慢体会。
最重要的是，
@idea{我在给你种@tech{函数式编程}的种子，
 相信他日定会发芽}。

冒险越来越深入了……

@handbook-action{类型签名到底是干嘛用的？}

且慢，好像忘了什么事。
我们提了如此多次的@emph{类型}，
可它到底起了啥作用？
先看例子：

@tamer-repl[(code:comment @#,list{函数 @:id{find-similarity-score} 要求一个 @:type{Input-Port} 型的@tech{输入}参数})
            find-similarity-score
            (code:comment @#,list{于是，你把它给整不会了})
            (eval:error (find-similarity-score (string #\好 #\嗨 #\哟)))]

简而言之，@:thus{类型允许我们用精练的语言快速准确地理清算法的逻辑脉络，
 这也让我写书变得容易很多}。
本文的前三版都没有引入类型，写起来那叫一个痛苦。
在你理解了程序之后，
@focus{如果你只做正确的事，类型系统会悄悄隐匿于幕后；
一旦你碰到一个教你使坏的人，类型系统会直接帮你把那人的恶意击杀于萌芽中}。

@handbook-sidenote*{多写点 Python 程序，
 踩过坑摔疼了之后就能更深刻地理解我的用心良苦了。
}在类型的护航下，只要程序能正常启动，
就说明程序没有出现@focus{语言层面的错误}；
但是@focus{类型也不是万金油，
 它无法保证程序结果一定符合问题情景}。
比如，不小心多数了一个数……

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
