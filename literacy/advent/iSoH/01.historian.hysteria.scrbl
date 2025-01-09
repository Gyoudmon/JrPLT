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

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define rr:add1-sticker
   (lambda [id r datum unit font color]
     (define c (rgb* color (/ (+ r 1.0) 8.0)))
     (define g (geo-vc-append (geo-text "+1" font #:color c)
                              (geo-arc (* unit 0.5) pi 0.0 #:stroke c #:ratio 0.85)))

     (cons (if (eq? 'arrow datum)
               (geo-pin* 1.0 0.56 0.5 0.5 g (geo-dart (* unit 0.1) (* pi 0.5) #:fill c #:stroke #false))
               g)
           'lc)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@aoc-task[2024 1]{历史学者癔症了}

@aoc-desc[#:keywords ["文学式编程" "算法" "函数式编程" "递归函数" "类型签名" "REPL" "sexp" "列表" "高阶函数"]
          #:edition [四 "2025-01-08"]]

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
用作者自己的写作思路来写程序，同样阅读程序的人也能更容易理解程序。
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
一般就是任务章节中出现的第一个碎片，
它看起来是这个样子的：

@handbook-chunk[<hysteria:*>
                (module advent typed/racket
                  |<Helper Task: Read Location IDs>|
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

 两份地址编号出炉之后，它们显然会不尽相同，
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
 @item{最后，甲组的最大编号是 @racket[4]，乙组的最大编号是 @racket[9]，差距为 @racket[5]。}]
于是，将以上各对差距相加即可得到总差距：
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
@racket[|<Helper Task: Read Location IDs>|]
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
 并产生一系列@handbook-deftech[#:origin "Output"]{输出}@tech{值}}。
如果你喜欢做饭，那你肯定听过或看过菜谱，
它会详细说清楚需要的食材、佐料(@tech{输入})，
烹饪时的工序、火候、材料用量等等各种细节(@tech{处理})，
保证你照着做就真的能得到它想教会你做的菜(@tech{输出})。
菜谱就是厨房里的@tech{算法}。

那么对比菜谱，我们上面描述的解谜步骤能被称为@tech{算法}吗？能也不能。
说它能，是因为它确实给出了有限的步骤，并且按此步骤的确存在解开谜题的可能；
说它不能，是因为@:thus{它还是太过笼统，描述不够清晰，甚至造成歧义}。
比如：如何把数字“加入”到一个不存在的@tech{列表}中？
“重复上述过程”中的“上述”具体是哪几步？
相信聪明的你可以联系上下文自动理清所有细节，
但是你能教会你的同学、乃至你不满10岁的弟弟妹妹吗？
编程就是@idea{用计算机能理解的语言教会它做你已经会的事}，
但是，计算机可比人笨得多，
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
 #:tag 'read-location-ids "Read Location IDs（大白话版）"
 @list['|read IDs|]{尝试从文件的当前位置@:in{读取}两个@:type{自然数}}
 @list['predicate?]{@emph{若} 读到的确实都是@:type{自然数}，@emph{则}：}
 @list['|cons 1st|]{@hspace[4]将第一个@:type{自然数}加到甲组地址编号列表的头部位置}
 @list['|cons 2nd|]{@hspace[4]将第二个@:type{自然数}加到乙组地址编号列表的头部位置}
 @list['loop]{@hspace[4]@emph{回到}@algo-goto['|read IDs|]重复执行}
 @list['done]{@emph{否则} @:cmt{; 两组地址编号列表构造完毕，告知结果}}
 ]

于是，我们就得到了书写工整的@algoref{read-location-ids}。

本书惯例，为方便文中引用某个步骤，
我在@tech{算法}左边增加了@:term{行号}和@:term{步骤名}。
从“引用单个步骤”的目的看，
@:term{步骤名}更容易记忆和理解。
比如，通过引用第一行的步骤名，
大白话的“@:desc{重复上述过程}”被改写成了
更精确的“@:desc{@emph{回到}@algo-goto[#:tag 'read-location-ids]{read IDs}重复执行}”。
而@:term{行号}天然有序，
它既能@focus{指示你按自然数顺序阅读@tech{算法}，
除非某个步骤或规则明确地改变顺序}；
也能方便地引用多个连续步骤。
比如：我们即将讨论@algoref{read-location-ids}的第 @racket[3] @:pn{-} @racket[5] 行，
文中引用它们的链接显示为@algoref[#:line (cons 3 5)]{read-location-ids},
或者像下一段开头那样在行号前面同时附上@tech{算法}标签。

有别于其他几行步骤，
@algo-ref[#:line (cons 3 5)]{read-location-ids}前面都留下了@focus{相同数量}的空格。
这个在@tech{算法}的书写规范中称为@tamer-defterm[#:origin "Indent"]{缩进}，
其本质同文章自然段的@:term{首行缩进}。
目的是让你能更容易地阅读@tech{算法}，
同时也提醒你@focus{此处@tech{算法}的阅读顺序可能会有所调整}。
从排版角度讲，
描述@tech{算法}采用的是@:term{悬挂缩进}，
被缩进的@tech{算法}步骤归属于它们的上一行步骤。
对@tech{缩进}更直观的解释在@Secref{algo:flowchart}。

虽然@algo-ref{read-location-ids}打着大白话@tech{算法}的名义，
但读起来总感觉哪里怪怪的。
有这个感觉就对了，
每一次翻译都会使得@tech{算法}离大白话更远、
离数学和可执行代码更近。
@handbook-sidenote*{除非你就是要造一个没法跟你愉快玩耍的机器人。}
计算机能直接执行的指令应当简洁和明确，不应该产生歧义，
一个总是误解你的程序没有存在价值。

那么，准备好了没？我们即将面对不说人话的@tech{算法}描述。

@algo-pseudocode[
 #:tag 'alg:rpcl "Read Location IDs(未知数版)"
 @list['initialization!]{@emph{设} @${x}、@${y}分别是@focus{初始}代表甲、乙两组地址编号列表的@emph{空列表}}
 @list['|read IDs|]{尝试从文件当前位置@:in{读取}两个@:type{自然数}，@emph{设}为@${a}、@${b}}
 @list['predicate?]{@tt{if}@hspace[2]@:pn{@${a}和@${b}确实都是@:type{自然数}}, @tt{then}}
 @list['|cons x|]{@hspace[4]@emph{令} @focus{新}@${x = a:x}}
 @list['|cons y|]{@hspace[4]@emph{令} @focus{新}@${y = b:y}}
 @list['loop]{@hspace[4]@emph{回到}@algo-goto['|read IDs|]重复执行}
 @list['No]{@tt{otherwise} @:cmt{; 此时的 @${x}、@${y} 分别指向甲、乙两组的地址编号列表}}
 @list['done]{@hspace[4]@:cmt{; 告知结果}}
]

于是，用数学语言(和代码)翻译@algo-ref{read-location-ids}后我们得到了@algo-ref{alg:rpcl}。
怎么样，汗流浃背了没？深呼吸，放轻松。
这个@tech{算法}每一步做了什么事都已经确定了(和大白话版本的描述对比即可)，
只是选用了诸多怪异的符号来书写。

无论是以大白话为主的@algo-ref{read-location-ids}，
还是混合了大白话、数学语言、程序代码的@algo-ref{alg:rpcl}。
这种@emph{没有固定章法@handbook-footnote{计算机类论文、
  书籍中出现的@tech{伪代码}经常看着很像作者喜欢的程序语言的代码。
  这说明@tech{伪代码}可以写成任何样子，
  反而像本书这样照顾年少初学者的写法才是另类。
 }，却又处处透露着死板}的描述@tech{算法}的方式称为@tamer-deftech[#:origin "Pseudocode"]{伪代码}描述。
像真的代码一样一板一眼，又不能像真的代码一样直接喂给计算机去执行。

@handbook-action[#:tag "algo:flowchart"]{流程图描述}

@tech{伪代码}步骤名的另一个妙用是作为@tech{流程图}的标签，
以图形化的形式更直观地表达@tech{算法}步骤及其执行顺序。
比如，把@algo-ref{alg:rpcl}画成流程图就长@fig-ref{flow:rpcl}那样。

@tamer-figure!['flow:rpcl
               @list{@algo-ref{alg:rpcl} 流程图}
               @(geo-scale hh-sp1.dia 0.50)]

@tamer-deftech[#:origin "Flowchart"]{流程图}使用箭头指示下一步活动，
使用形状指示活动类型。
常用图形有：
@handbook-sidenote*{标准@tech{流程图}没有规定颜色。
 但本书惯例，图形和箭头的颜色也遵循固定风格。
 这里不详述。}

@itemlist[
 #:style 'compact

 @item{@dia-flow-node[#:scale diaflow-node-scale]{^跑道形}
  指示@tech{算法}的开始和结束。
  每个@tech{算法}都必定有一个开始，应当@focus{至少}有一个结束。
  在绘制时通常会“向下”或“向右”流动。}
 
 @item{@dia-flow-node[#:scale diaflow-node-scale]{>>平行四边形} 指示@tech{算法}的@tech{输入}和@tech{输出}。}
 
 @item{@dia-flow-node[#:scale (* diaflow-node-scale 1.18)]{菱形?} 指示@tech{算法}的条件分支。
  也即@tech{算法}走到了岔路口，要根据条件决定选择哪条分支继续。
  @focus{分支不体现先后顺序}，仅在每条分支的第一个箭头上附上条件成立与否的标签。
  比如：@algo-ref[#:line 'predicate?]{alg:rpcl}
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
  你看，你是不是没发现这个步骤并没有出现在大白话描述的@algo-ref{read-location-ids}里。}
 ]

当你要跟其他人协作解决复杂问题时，
或者像本书这样帮助初学者自学编程时，
@handbook-sidenote*{像不像语文、英语课上的缩句和扩句练习？}
在问题分析阶段搭配使用@tech{伪代码}和@tech{流程图}是比较好的描述@tech{算法}的方式。
@tech{流程图}帮你理清和记忆线索，
@tech{伪代码}详细解释线索中每一步的关键点。

当你个人能力足够强，且不涉及协作或教学时，
@tech{流程图}就不是必须的了。
因为它过于古早，描述能力太弱；
而且画起来既麻烦又很占地方。

在正式学编程之前，我花了这么多篇幅来讲“语言无关”的@tech{算法}思想。
其根本原因是@:thus{各种各样的计算机@tech{算法}事实上都起源于世界各民族的纸笔@tech{算法}}。
不过，@focus{由于本书的核心是@tech{函数式编程}，
 超越了大白话@tech{伪代码}的常规描述能力。
}因此，阅读本书时，@:thus{你仍会经常发现将@tech{伪代码}翻译成真实代码时存在诸多思维转换和提升。}

此外，@idea{你所掌握的不同@emph{类型}的程序语言，
 你的数学功底，以及其他知识储备，它们都会互相影响，
 并最终反映在你写出来的@tech{伪代码}上。
}实际上，能完成任务的@tech{算法}何其多，
我有意写得贴近本书的学习目标。
比如，按照正常人用纸笔解谜的过程，
他们更可能把读到的数字加到@tech{列表}末尾，而不是开头。

@handbook-action{高级程序语言描述}

现在，我们对@tech{算法}这个概念已经有了个最基本的感性认识。
不过，回看@algo-ref{alg:rpcl}和@fig-ref{flow:rpcl}，
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
   简直岂有此理，取关、拉黑！}}
 
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

 @item{等到我们设计@tech{算法}时，命名问题可能会很突出。@handbook-sidenote*{
   这方面千万别偷懒，也不要盲从各种比赛题目，
   包括带赛教练、所谓刷题刷出来的金牌选手。
   用我的方法试试，对比之后才会形成你自己的见解。
   }@tech{伪代码}跟数学习题一样，
  会包含足够信息让你理解各个字母的含义，
  因此我在@tech{伪代码}里也会用单个字母表示未知数。
  而当@tech{伪代码}被翻译成程序代码之后，
  代码就成了你复习时最关键的材料(它们通常不会跟题目/问题一起保存)。
  这时候@focus{可读性比什么都重要，你的代码读起来应该像(语法独特的)散文。}
  @:thus{除了定义明确的概念可以沿用数学惯例外，
   其他未知数的名字都应仔细斟酌}，
  即使只用一个字母，也应优先用单词首字母，
  而不是随便@${a}， @${b}， @${c}， @${d}。}
 
 @item{@idea{科学家只会给重要的概念起名字，
   然后赋予这个名字一个专属字母符号}。
  数学中的重要概念也有它们的专属字母，
  比如：圆周率@${\pi}，自然常数@${e}，等等。
  @idea{命名很重要，但也很难，
   能反应出你是否真的理解了问题。
   经常训练这个也可以激活你的词汇量}。}
]

@handbook-scene{读取-判断-扩列 循环}

所有的@tech{算法}也都应该有个名字，
而@:term{循环}是绝大多数程序的典型特征。
因此，不妨把@algo-ref{alg:rpcl}称作@:term{读取-判断-扩列 循环}，
缩写为 @:id{rpcl}(read-predicate-construct loop)。
于是：

@handbook-chunk[|<read-predicate-construct loop>|
                (let rpcl (<initialization!>)
                  |<read IDs>|
                  (if <predicate?>
                      <地址编号扩列，递推下一轮>
                      <done>))]

@focus{@tech[#:key "函数式编程"]{函数式}代码惊人的短小精悍}有没有？
之后的活就是把那些与步骤名对应的代码碎片一个个翻译成真实代码。
注意区分，
@tech{伪代码}步骤名用@emph{角引号}(@:pn{« »})界定，
代码碎片用@emph{角括号}(@:pn{< >})界定。
本例中它们大致上一一对应，
除了@focus{用晦涩的中文}写出来的那句。

@algo-ref{alg:rpcl}看着已经很数学了，
但如果我说它还可以进一步数学化你会怎么想？
前面我们强调了@tech{算法}步骤的@:term{简洁}和@:term{精确}，
下一步就该考虑@:term{严谨}了，
也即在@focus{任何}情况下都不能出错。
而且最好是@focus{让计算机自己能确保这件事，
 而不是浪费我们宝贵的时间去一遍遍检查}。
再怎么改@tech{伪代码}也还是要我们亲自去确认，听着就很费时间。
所以，这个目标我们直接放到真实代码中来，
这也是@idea{在智能时代@:err{不能}只学数学而不学计算机科学}的原因。
这方面，@focus{计算机科学家的方法甚至比数学家的都更优越}。

程序语言确保@tech{算法}严谨的地基是@focus{给代码加上类型标记}。
于是，@:type{Typed} Racket 定义@tech{变量}需要同时提供三个信息：
@:term{变量名}，(可选的)@:term{变量类型}和@:term{初始值}。
先@:term{变量名}后@:term{变量类型}，中间用冒号(@:pn{:})分隔，
再跟上@:term{初始值}。

@algo-ref[#:line 'initialization!]{alg:rpcl}没有强调类型，
在这补上：

@handbook-chunk[<initialization!>
                [A.IDs : (Listof Natural) null] (code:comment "甲组精灵写下的地址编号列表")
                [B.IDs : (Listof Natural) null] (code:comment "乙组精灵写下的地址编号列表")]

本例中的两个@tech{变量}的类型均为@:term{自然数列表}(@:type{(Listof Natural)})，
初始值@:term{空列表}写作 @:val{null}。
分号(@:pn{;})及其后面到本行结尾的内容是@tamer-deftech[#:origin "Comment"]{注释}，
这是写给人类读者看的，在计算机看来相当于空格。

此外，本段代码碎片将@tech{变量} @${x}和@${y}重命名成了 @:var{A.IDs} 和 @:var{B.IDs}。

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

@algo-ref[#:line '|read IDs|]{alg:rpcl}就是字面直译，
@:desc{对清单文件 @:var{locin} 执行 @racket[read] 操作，给结果起名为 @${a}；
 重复一次，并给结果起名为 @${b}：}

@handbook-chunk[|<read IDs>|
                (define a : Any (read locin))
                (define b : Any (read locin))]

函数 @:id{read} 的求值结果@handbook-footnote{Racket 不太强调“返回值”这个说法，
 而更偏好“求值结果”(evalutes to a result)。
 如果你已经习惯了其他语言的叫法，那就按你自己的习惯来。
 }可以是任何（@:type{Any}）合理类型的@tech{值}，
其中包括特殊值 @tamer-defterm[#:origin "End of File"]{eof}，
代表@:name{文件结尾}，也即清单读完了。
注意，此时我们要假装自己是英语母语人士，
将文件里的@:term{连续空格}(包括@:term{换行})视作单词的分隔符。
因此只管“读”，不用考虑换行。
这也意味着，@:id{read} 自己知道“当前位置”在哪里，
就像我们在看书时，眼睛会跟着一起移动。

本例中，我们可以放心地假设，
@${a} 和 @${b} 要么是@emph{自然数}，要么是 @tech{eof},
不存在其他可能。
于是，@algo-ref[#:line 'predicate?]{alg:rpcl} 就是对结果提问：@:desc{
 @${a}是自然数吗？@${b}是自然数吗？}
并且要同时回答 @racket[#true](@:desc{是})才算条件达成：

@handbook-chunk[<predicate?>
                (and (exact-nonnegative-integer? a)  (code:comment "a 是自然数吗？")
                     (exact-nonnegative-integer? b)) (code:comment "b 是自然数吗？")]

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
数学上，前者是@:term{精确数}(exact number)；
后者可能是@:term{精确数}，也可能是@:term{近似数}(inexact number)。
而在 Racket 中，前者也是@:term{精确数}，但后者一定是@:term{近似数}。
因此，问句变成了@emph{你是精确的非负整数(exact nonnegative integer)吗}，
更突出了@emph{自然数}的现代定义，且消除了争议。

像@handbook-sidenote*{Racket 的@tech{谓词函数}通常以问号(@:pn{?})结尾。}
@racket[exact-nonnegative-integer?] 这样@emph{
 接受一个任意类型的参数，
 得到一个布尔型（@:type{Boolean}）的结果（@racket[#true] 或 @racket[#false]），
 用以检查输入参数是否满足某些条件}的函数称为@handbook-deftech[#:origin "Predicate Function"]{谓词函数}，
可类比一般疑问句中的谓语，但句法上要简单很多。

是吧？Racket 代码读起来很像英语写的数学说明文，
但一眼看上去不像数学证明。嗯，
Racket 的命名风格偏向严谨，极少瞎搞缩写，比较合我的口味。
@idea{对英语语感尚不稳定的初学者来说也是件微不足道的好事}。

按照正常说话顺序，“@:desc{若@algoref[#:line 'predicate?]{alg:rpcl}，
 则@algoref[#:line (cons 4 6)]{alg:rpcl}，
 否则@algoref[#:line (cons 7 8)]{alg:rpcl}}”，
在得到 @${a} 和 @${b} 的回答之后，
我们会先处理@emph{条件成立}时要做的事：

@handbook-chunk[|<地址编号扩列，递推下一轮>|
                (rpcl (cons a A.IDs)  (code:comment "构造新的甲组编号列表，保证 a 成为原列表的头部")
                      (cons b B.IDs)) (code:comment "构造新的乙组编号列表，保证 b 成为原列表的头部")]

这个代码碎片的名字相当晦涩，写出来倒是意外地简单。
它蕴含了@tech{函数式编程}的基本原理，暂时不详述。
如果你实在忍不住好奇，
可以先从@Secref{sec:fp}看起，
到@Secref{sec:rloop}结束。

于是，就剩最后一句了。
当@emph{条件不成立}时，
@algo-ref{alg:rpcl}就直接结束了：

@handbook-chunk[<done>
                (values A.IDs B.IDs)]

有趣的事情又来了，正常情况下，
@tech{算法}都会产生一些@tech{输出}结果。
在@tech{函数式编程}中这一点尤其本分，@handbook-sidenote*{
  自信点，这确实就是你在数学课上学到的那个代数表达式。
}@focus{
 每一条@tech[#:key "函数式编程"]{函数式}代码的语句其实压根就不是执行指令的@emph{动作序列}，
 而是一个个@:term{代数表达式}，
 而有定义的@:term{代数表达式}天然有结果@tech{值}。
}因此，
我们无需专门像常规编程语言那样专门搞个 @:kw{return} 语句来表达“这就是结果”。
@focus{每一条@tech[#:key "函数式编程"]{函数式}代码都会产生@tech{值}，
 当它是@tech{算法}的最后一条语句时，
 它的@tech{值}也就顺理成章地成了整个@tech{算法}的@tech{输出}结果。
}@handbook-sidenote*{前文我是不是说过，Racket 不太强调“返回值”这个说法？
}这也是为什么@algo-ref[#:line 'done]{alg:rpcl}的描述写成了@tech{注释}。

函数 @:id{values} 有点奇怪，
它的求值结果就是一切你给它的@tech{值}，
什么多余的事情都不做，
常常用作@handbook-deftech[#:origin "Identity Function"]{恒等函数}。

@handbook-action{类型签名}

到这里，从解谜的角度来说，我们已经打好草稿了；
从答题的角度来说，还缺少一步：把草稿誊写到答题卡上。
用 @:stx{let} 定义的 @:id{rpcl} 函数是一次性的，
你写完它还没来得及用就被扔进垃圾桶了，
这种匪夷所思的事几乎不会发生在真实世界中，
但确实就有可能发生在数字世界里。
因此需要把它放置在另一个正式函数里才能随叫随用。
这便是本节一开始就提到的@:term{辅助任务}碎片：

@handbook-chunk[|<Helper Task: Read Location IDs>|
                (define read-location-ids <函数read-location-ids的类型签名>
                  (λ <函数read-location-ids的参数列表>
                    |<read-predicate-construct loop>|))]

相对于草稿纸上的函数 @:id{rpcl}，@handbook-sidenote*{
 λ 是希腊字母，在函数式语言里代表的正是函数本体。
 在 Racket 中也可写成它的英语单词： @racket[lambda]。
}这个任务碎片用 @:stx{define} 和 @:stx{λ} 严肃定义了一个叫做 @:id{find-total-distance} 的函数，
其除了在内部包含了@racket[|<read-predicate-construct loop>|]本体外，
还需标注清楚@tech{类型签名}和参数列表。

所谓@handbook-deftech[#:origin "Type Signature"]{类型签名}，
简单来说就是@tech{函数}的@:term{定义域}(输入参数)和@:term{陪域}(求值结果)应当满足的类型约定。
标注@tech{类型签名}的方式有很多，
比较常见的是像下面这样：

@handbook-chunk[<函数read-location-ids的类型签名>
                : (-> Input-Port (Values (Listof Natural) (Listof Natural)))]

@handbook-chunk[<函数find-total-distance的类型签名>
                : (-> Input-Port Natural)]

跟标注变量类型时一样，函数的类型信息也跟在用空格隔开的单个冒号（@:pn{:}）后面。
@focus{@:pn{(-> )}表明这个类型代表的是一个@tech{函数}，
里面的最后一个类型表示函数的@:term{陪域}类型;
其余类型名按顺序依次表示每一个输入参数的类型。}
于是，函数 @:id{find-total-distance} 的功能可以简单表述为：
@:thus{给定一个类型为@:term{输入流}（@:type{Input-Port}）的输入参数，
 得到一个类型为@:term{自然数}（@:type{Natural}）的结果。}
完美匹配了第一个谜题的要求：
@:desc{阅读精灵写的地址编号清单，计算得到总差距。}

@focus{@tech{类型签名}只强调参数的类型，不强调参数的名字}。
因为计算机不懂名字的寓意，它们更擅长通过类型来理解程序；
而人类读者更习惯通过“有意义的名字”来理解内容。
因此，我们把类型为 @:type{Input-Port} 的参数命名为 @:var{locin}（即
@litchar{loc}ation ID @litchar{in}put-port 的缩写）：

@handbook-chunk[<函数read-location-ids的参数列表>
                [locin]]

@handbook-chunk[<函数find-total-distance的参数列表>
                [locin]]

你注意到代码碎片@racket[<read IDs>]里也用到@tech{变量} @:var{locin} 了吗？
这也是 @:id{rpcl} 草稿函数实锤的一个重要原因：
它知道如何解谜，但是没有花精力去关注谜题里的数据到底从哪来。
当它被放置在 @:id{find-total-distance} 里时，
就自动共享了@:term{输入流} @:var{locin}。
@:desc{读取数据}就像@:desc{打开水龙头就有水流出来}一样自然。

@handbook-scenario{REPL}

函数 @:id{rpcl} 的命名借鉴了Lisp语系传统的
@handbook-deftech[#:origin "Read-Eval-Print Loop" #:abbr "REPL"]{读取-求值-打印循环}。
即，每一轮循环要做的事是：@:desc{读取用户输入的表达式，计算此表达式的结果，再把结果打印出来。}
这个循环看起来不太起眼，但在很多场合能极大的提高你的开发效率，
因为你不用单独准备一个完整的测试程序就可以立即检查函数的运行是否符合预期。
因而时至今日，好多语言都抄了一个类似的东西。
比如运行 @exec{idle.exe}，那就是 Python 的 @tech{REPL}，
只是它用了另一个更常见的名字，叫做@:term{Shell}。

在本书中，Racket 的 @tech{REPL} 既用于举例，也用于直接运行我们主线任务中的代码。
它看起来是这个样子的：

@tamer-repl[(code:comment "这是一个 Flonum 类型的值")
            3.14]

分号（@:pn{;}）及后面的黄色文字是程序注释，不是必须存在。
提示符（@:pn{>}）后面的就是用户输入的表达式，
如注释所言，本例演示的是 Racket 的@tech{值}。
Racket 读取到一个表达式之后，
会对该表达式求@tech{值}（尝试化简该表达式），
然后打印出@tech{值}的类型信息(@:out{@:pn{- :}和后面的紫色文字})和内容（@:res{最后面的蓝色文字}）。

这段话看起来很绕口。
所谓@handbook-deftech[#:origin "value"]{值}，
指的就是不可再化简的表达式。
所以上面的 @racket[3.14] 的值就是它自己。
要不我们就索性再捋一捋，程序中几个最基础但又特容易弄混的概念，
以及@tech{函数式编程}中的函数的独特之处。开始：

@tamer-repl[(code:comment "给上面的值起个名字叫做π")
            (define π : Flonum 3.14)
            (code:comment "于是你可以用变量名π来代指3.14 (即：化简π得到最终值3.14)")
            π
            (code:comment "string? 也是一个变量名，会被替换成一个(-> Any Boolean)类型的值")
            string?
            (code:comment "λ是产生函数的表达式，下面这句会被替换成一个(-> Any Any)类型的值")
            (λ [arg] arg)
            (code:comment "给上面的函数值起个名字叫做id")
            (define id : (-> Any Any) (λ [arg] arg))
            (code:comment "于是你可以用变量名id来代指(λ [arg] arg)")
            id
            (code:comment "你也可以用变量名id将(λ [arg] arg)用作函数")
            (id π)]

迷糊了没？迷糊就对了。
简单来说，在函数式语言里，
函数既可以用来操作其他的@tech{值}，
它们自己也跟普通的@tech{值}一样，是一种特殊的@tech{值}。
在 Racket 中，这类特殊的@tech{值}统称为 @:id{procedure}(也即：
可执行的过程@handbook-footnote{实际上，程序语言中从来没有“函数”这个东西，
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
注意看上面例子中的 @racket[id] 和 @racket[(λ [arg] arg)] 的@tech{值}的差别：
前者是 @procedure{id}，名字叫 @:id{id};
后者是 @:res{#<procedure>}, 没有名字，因而也被称为匿名函数。

于是，接下来我们实际运行一下主线任务中的函数来验证它是否正确：

@tamer-repl[#:requires ["../aoc.rkt"]
            ($ find-total-distance #:< "iSoH/01_hh.aex" #:expect 11)]

求值的最终结果是 @racket[11]，符合预期。
@handbook-sidenote*{如果结果不对，本书也将不复存在。}
任务数据保存在后缀名为 @litchar{.aoc} 的文件中：

@tamer-repl[($ find-total-distance #:< "iSoH/01_hh.aoc")]

第一个谜题求解完毕。提交这个答案，解锁第二个谜题。

@handbook-action{列表}

谜题故事中、@tech{伪代码}中、以及前面的正文中，
我们已经多次反复提及了“@tech{列表}”这个词，
但并没有违和感。
@handbook-sidenote*{
 嗯？细细想来，汉语似乎并不喜欢直接使用“列表”这个词。
 近年来的网络词汇“扩列”中涉及“好友列表”，
 更像是技术术语搭上了社交软件本土化的顺风车才破圈的。
 有点意思哈。}在日常生活中，
@tech{列表}是个相当常见的东西，
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

 @item{一种容器，可以存放很多项内容，项的数量称为@:term{长度}；}
 @item{容量虽没有理论上限，但受限于存储媒介的大小；}
 @item{内容@focus{按顺序}存放，可通过@:term{自然数}@:term{索引}来引用某一项内容；}
 @item{内容@focus{可以重复}出现，重复内容靠存放位置来区分彼此。}
 ]]

这些特征说清楚了@tech{列表}作为一种@:term{数据类型}应该长什么样，
但却没有规定它具体该以什么@:term{结构}形式出现。
比如：你的心愿单是写在日记本里，还是写在许愿签上？
又该如何添加新愿望、处理已经完成的心愿？

在不同语言、不同场景下，
@tech{列表}并非都叫这个名，
其@:term{存储结构}也可能很不一样。
比如：Python 的列表（@:id{list}）是@:term{顺序表}（类比日记本）；
C++ 的列表（@:id{std::list}）是@:term{链表}（类比许愿签）。
它俩涵盖了绝大多数情况。而在 Racket 中，
列表（@:id{list}）除了可以当成@:term{链表}来用外，
它还是更为重要的基础设施，
术语叫做@:term{广义表}(详见@Secref{sec:sexpr})。

@handbook-action{程序 = 算法 + 数据结构}

计算机科学领域有个相当著名的公式，@focus{程序 @:pn{=} @tech{算法} @:pn{+} @:term{结构}}。
它道出了编程学习的核心任务和目标，
其中@tech{算法}已经重点介绍过了，理论上会写作就能理解；
@:term{结构}主要指@handbook-defterm[#:origin "Data Structure"]{数据结构}，
也就是@emph{按什么样的格式来组织、存储和操作数据}，
这个却不是初学者可以直观感受的，需要系统学习。

好在，最常用的@tech{数据结构}在现代高级程序语言里都是开箱即用，
@tech{列表}就是其中最简单、最基础代表。
@handbook-sidenote*{如果没有现成的@:id{sort}函数，有想过你自己会如何对@tech{数列}排序吗？}
使得我们只需要弄清楚@emph{对列表做什么}而
不用浪费时间去思考@emph{具体怎么做}（不同@tech{列表}的做法还真不一样）。
这就是思维方式的提升。

于是，看起来最难翻译的@:desc{将地址编号列表按升序@emph{排序}}反倒是最简单的：

@handbook-chunk[|<sort IDs>|
                [A.sorted-IDs (sort A.IDs <)] (code:comment "按升序排列甲组地址编号列表")
                [B.sorted-IDs (sort B.IDs <)] (code:comment "按升序排列乙组地址编号列表")]

函数 @:id{sort} 就是字面意思“对列表排序”，
“升序”即“从小到大”，指定小于号（@:pn{<}）即可;
然后将排好序的@focus{新}@tech{列表}作为求值结果返回，
@focus{而原来的列表保持不变}。
本例中原列表的使命已经结束，
新列表拥有新名字以突出“已排序”(sorted)。
此外，因为函数 @:id{sort} 的功能明确，
这里新定义的两个@tech{变量}的@:term{类型}信息可以省略。

@handbook-scenario[#:tag "sec:fp"]{函数式编程}

为啥非要这么折腾一圈而不直接修改原列表？这是因为我们讨论的是@tech{函数式编程}啊。

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

@tamer-figure-margin['flow:mutation "经典“赋值”示意图" (geo-scale hh-mutate.dia 0.4)]

@aoc-complain{
 这对于从小学着代数长大、做了无数卷子却没多少工科实践类课程的初学者来说，
 它相当于在做题时要不断“用修正液涂掉旧答案、写上新答案”，
 @:thus{检查时草稿纸上已经没有任何做题痕迹了}。
 更拧巴的是，数学老师普遍要花大力气才能让学生正确理解@:err{
  等号（@:pn{=}）不表示“经过计算得到一个值”}，
 然后编程老师说@emph{要先计算等号右边的值，再放回左边的存储空间里}……

 别气馁，我也经历过这样的瞎折腾。
 好消息是，@tech{函数式编程}会带我们回归数学思维。
 因其本质是@focus{代数替换}，
 因此@:thus{等号（@:pn{=}）就是关系运算符，没有@:term{赋值}语义}。}

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
  打个比方，
  数学@tech{变量}相当于身份证上的姓名，
  不同人的名字可以相同可以不同，
  但每个人的名字在第一次确定之后（一般）不可更改，
  当交谈中提到你认识的人的名字时，
  你脑海里浮现的也是@focus{记忆中}那个人活灵活现的形象；
  与之相对的，指令式语言的@tech{变量}相当于酒店房间的门牌号，
  房间里面有谁取决于何人能入住，
  提到这个门牌号时，很可能已经不是原来的住户了。}

 @item{指令式语言的@tech{变量}之所以代表存储位置，
  根本原因是因为我们现在@focus{最常见的计算机组成结构的核心仍然是存储设备，
  一切程序代码和数据都必须安排存储空间才能参与计算，函数式语言也不例外}。
  因此当你要深究函数式程序在执行时都偷偷瞒着你做了啥见不得人的事时，
  你会惊讶地发现，它们的@tech{变量}也都有自己在内存的地址。
  差别在于，
  
  @handbook-itemlist[
 #:style 'compact
 
 @item{@focus{函数式语言不提供方法（或者明确建议你不要）直接修改变量的值，
     如果你遵守这个约定，你的程序运行起来会更高效和可靠}。
    在此语境下打个比方，
    指令式语言的@tech{变量}仍是酒店房间的门牌号，@handbook-sidenote*{
     身份证上确实写着你的家庭地址
    }函数式语言的@tech{变量}则是家庭地址的门牌号。
    显然，酒店房间人来人往，你不能假设里面住着谁；
    但家庭地址比较稳定，你可以合理假设里面的住户不会轻易改变。
    现在你要找人，或者寄快递，哪类地址更可靠？}

 @item{@focus{函数式语言的代码和它们在内存中的存储结构差异可以大到面目全非、毫不相干}。
    形象点说，用纸笔模拟计算，纸就是一种存储设备。
    不同学科的练习本上画的线往往都不一样，
    这些线可以启发你类比不同的存储结构。
    比如，作文本与指令式语言，数学本与函数式语言。}]}

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

@idea{初学@tech{函数式编程}应该以回归数学思维为主，
 (至少暂时)不要被机器牵着鼻子走，但心里要清楚数学不是全能之神，
 该变通的时候还是要变通的(此结论也适用于用数学观点看待科学)}。
实际上，现代高级程序语言很少只能用一种风格，
差别在于他们建议的主风格是什么，
又提供了多少便利帮助你写出他们建议风格的代码。

本例中，唯一有@tech{副作用}的代码是 @racket[(read locin)]，
因为它依赖谜题提供的清单文件，违背了特征3。

@handbook-action{函数式 @${x = x + 1}}

用数学语言该如何表达@tech{指令式编程}的@${x = x + 1}这样的迷惑操作呢？@handbook-sidenote*{
 就是写作时，别人或者你自己经常会唠叨的“无法用语言形容”。
 }注意，这很可能是你第一次碰到@emph{大白话@tech{伪代码}描述能力不够}的地方。
人类对话用的“自然语言”并不擅长描述数学、科学这样“不自然”的东西。

@algo-ref[#:line (cons 4 5)]{
 alg:rpcl}和@algoref[#:line (cons 8 9)]{
 alg:rpcl}都在不断定义@focus{新}@tech{变量}，
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
@tech{变量} @${x} 的@tech{值}依次是 @${0, 1, 2, 3, 4, 5, 6, 7, ... }。
现在把它们直观化为@fig-ref{numberline}的@emph{数轴}：

@tamer-figure!['numberline
               @list{数轴，一生二、二生三、三生万物}
               @(plot-axis #:tick-range (cons 0 7) #:reals '(0 1 2 3 4 5 6 (7 . arrow))
                           #:real->sticker rr:add1-sticker
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

道理我都懂，可是这跟@tech{函数}有什么关系？
@tech{函数}存在的首要任务是将@tech{输入}转化为@tech{输出}。
估计是“@${+ 1}”这个写法让你困惑了，
给它取个高级点的名字吧，
叫做 successor(@emph{后继}，意为“下一个”)，缩写为 @${s}。
其功能是，当我们给这个@tech{函数}喂一个@emph{非负整数}时，
它就会吐出这个数的“下一个”@emph{整数}给我们。
于是，@$${s(0) \rightarrow 1,
 @hspace[2]s(1) \rightarrow 2,
 @hspace[2]s(255) \rightarrow 256,
 @hspace[2]s(1000) \rightarrow 1001,
 @hspace[2]...}
现在不必再说“以此类推”了吧？

@handbook-scene{通项公式}

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
@handbook-sidenote*{到这里如果你还是不能理解@tech{函数}是什么，
那就先看看@fig-ref{tl:sub1}找找灵感，
哪些元素能启发你理解@tech{函数}这个概念?}

@tamer-figure!['tl:sub1
               @list{函数式@tech{变量}的@tech{时间轴}模型}
               (plot-axis #:tick-range (cons 0 8) #:reals sub1
                          #:real-position -2.0 #:real-anchor 'ct
                          #:real->sticker (make-timeline-real->sticker "x(n)" 8)
                          #:real-color 'DarkCyan
                          #:real-exclude-zero? #true
                          #:axis-label "n"
                          400 0.0 42.0)]

本书会将只有一个@tech{输入}参数的@tech{函数}画成类似沙漏的形状，@handbook-sidenote*{
 你能联想到日常生活中有哪些东西跟它很像吗？
}沙漏上下的箭头表明@tech{输入}@tech{值}从一端进入，
在内部经过一番折腾(@tech{处理})，
从另一端出来一个@tech{输出}@tech{值}。

一个只会数数的程序没多大的用处，
但如果每数到一个数，
就把它扔给一个@tech{函数}，
静静等待这个@tech{函数}吐出另一个数，
情况就不一样了，
因为你有机会得到任何数。
@handbook-sidenote*{@tech{函数}名也跟@tech{数列}名相同，
 写法上略有差异 @${x_n \rightarrow x(n)}。
 }@:thus{这其实就是@tech{数列}的本质：
 一个专门用于将@emph{正整数}转化为别的数的@tech{函数}。
}在@tech{数列}术语中，
这样的@tech{函数}也被称为@:name{通项公式}。

现在，你可以用很文艺的方式跟你的小伙伴炫耀@tech{函数式编程}的基本特征了：
在@tech{函数式编程}的世界里，@idea{每个@tech{变量}都自带一条@tech{时间轴}，
@tech{时间轴}上的关键节点对应着该@tech{变量}在程序运行时的不同取@tech{值}。}
而运行程序的过程也像极了真实世界的时间流逝，
@idea{今日事今日毕，明天又是崭新的一天；
如此往复，虽从未曾惊扰昨日，一切已然悄悄变化。}

@handbook-action{递归函数}

递推思路顺便带来了两个看似平平无奇、实则奥妙无穷的能力：

@handbook-itemlist[
 #:style 'compact

 @item{从@:term{初始值}开始，可以一直计算@tech{变量}的“下一个”@tech{值}。
  于是，@:thus{我们得以用有限的语言来描述和理解无限的概念}。}

 @item{可以一直反推@tech{变量}的“上一个”@tech{值}，直到到达@:term{初始值}。
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
@$$={F(n) =
 1               & n = 1,\\
 1               & n = 2,\\
 F(n-1) + F(n-2) & n \ge 3.}
等号(@:pn{=})右边@tech{函数} @${F} 自己出现了两次。
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
 每一轮循环都是在调用函数自身}。

@handbook-action[#:tag "sec:rloop"]{递归式循环}

理论唠叨得差不多了，重新审视代码碎片@racket[|<read-predicate-construct loop>|]中的固定格式。
@:stx:def{let} 就是数学求解中常用的@:term{设}或@:term{令}，
如果顺便给 @:stx:def{let} 起个名字，
就会同时定义一个同名的一次性函数，并立即调用它。
本例中，我们定义了一个名为 @:id{rpcl} 的二元函数，
其主要功能是构造谜题中的地址编号@tech{列表}。

注意。当@tech{变量}是数值类型时，
其@tech{时间轴}构成一个@emph{数}的@tech{列表}（@:type{(Listof Number)}），
每个@tech{值}都是@emph{数}（@fig-ref{tl:sub1}），
因而@:term{递推公式}经常就是大家已经烂熟于心的算术运算；
本例中，@:id{rpcl}的两个参数都已经是@emph{自然数列表}类型了，
其@tech{时间轴}应该构成一个@emph{自然数列表}的@tech{列表}（@:type{(Listof (Listof Natural))}），
每个@tech{值}也都是一个@emph{自然数列表}（@fig-ref{tl:cons}）。于是：

@handbook-itemlist[
 #:style 'compact

 @item{@tech{列表}类型的@:term{初始值}是 @:val{null}，代表@emph{空列表}；}
 @item{@tech{列表}类型的@:term{递推公式}是 @:id{cons}(即 @litchar{cons}truct)，
  用于构造“下一个”新@tech{列表}。@handbook-sidenote*{
   @algo-ref[#:line (cons 4 5)]{alg:rpcl}的列表构造语法借用自 Haskell。
   }并且保证该新列表的头部项是刚读到的地址编号，其他项是原列表。}
 ]

@tamer-figure!['tl:cons
               @list{@tech{时间轴}，@tech{倒序}构造乙组精灵写的地址编号列表 @(tamer-elemref #:type 'file "01_hh.aex" @racket['(4 3 5 3 9 3)])}
               (plot-axis #:tick-range (cons 0 7) #:reals #(null (4) (3 4) (5 3 4) (3 5 3 4) (9 3 5 3 4) (3 9 3 5 3 4))
                          #:real-position -2.0 #:real-anchor 'ct
                          #:real->sticker (make-timeline-real->sticker "ID(n)" 7 0.618)
                          #:real-color 'DarkCyan
                          #:real-exclude-zero? #true
                          #:axis-label "n"
                          400 0.0 48.0)]

根据@tech{函数式编程}对@tech{递归函数}和普通循环的偏好，
函数 @:id{rpcl} 的首秀发生在@racket[<|initialization!|>]，
之后会在@:desc{每一轮循环结束时再次对自己求值以启动下一轮循环}。
而循环的性质保证了，@:desc{本轮循环结束时的变量@tech{值}即是下轮循环中变量的@emph{当前值}}。
前文在引入@tech{时间轴}模型的时候，有个点说得比较模糊，
@:desc{程序运行之后的某个时刻}到底是什么时候？一秒、一小时、一天？
显然都不是，我们重点在强调用@tech{递归}替代@:term{普通循环}，
那这个@focus{时刻}当然就是指@focus{每一轮循环开始的时候}。

无论如何，变量 @:var{A.IDs} 和 @:var{B.IDs} 的@tech{值}至始至终都没有变过，
只不过它们在各自循环中的使命完成之后就不再现身，
而被下一轮循环中的自己@:term{替换}掉了。
就好比电影续作中的主角还在，
也仍然叫那个名，
但是换了个新演员。

@handbook-action{迭代式循环}

@algo-pseudocode[
 #:tag 'alg:find-total-distance "求解总差距(未知数版)"
 @list['initialization!]{@emph{设} @${x}、@${y}分别是@focus{初始}代表甲、乙两组地址编号列表的@emph{空列表}}
 @list['|read IDs|]{尝试从文件当前位置@:in{读取}两个@:type{自然数}，@emph{设}为@${a}、@${b}}
 @list['predicate?]{@tt{if}  @:pn{@${a}和@${b}确实都是@:type{自然数}}，}
 @list['|cons x|]{@hspace[4]@emph{令} @focus{新}@${x = a:x}}
 @list['|cons y|]{@hspace[4]@emph{令} @focus{新}@${y = b:y}}
 @list['loop]{@hspace[4]@emph{回到}@algo-goto['|read IDs|]重复执行}
 @list['No]{@tt{otherwise} @:cmt{; 此时的 @${x}、@${y} 分别指向甲、乙两组的地址编号列表}}
 @list['|sort x|]{@hspace[4]@emph{令} @focus{新}@${x} @:pn{=} @:pn["("]@racket[sort] @${x}@:pn[")"]}
 @list['|sort y|]{@hspace[4]@emph{令} @focus{新}@${y} @:pn{=} @:pn["("]@racket[sort] @${y}@:pn[")"]}
 @list['sum]{@hspace[4]@emph{计算} @${\sum_{i=1}^n |x_i - y_i|}}
 @list['print]{@hspace[4]@:cmt{; 告知结果}}
]


如法炮制，现在改写大白话@tech{算法}的第二步：

@algo-pseudocode[
 #:tag 'accumulate "累加总差距(大白话版)"
 @list['|sort 1st|]{将甲组的地址编号列表按升序@emph{排序}}
 @list['|sort 2nd|]{将乙组的地址编号列表按升序@emph{排序}}
 @list['|for each|]{@emph{对于}两组列表中的@emph{每一对地址编号}，执行：}
 @list['accumulate]{@hspace[4]计算差距并累加到总差距中}
 @list['print]{@:cmt{; 告知结果}}
]

@handbook-sidenote*{Racket @tech{列表}常数用@:pn{()}界定，各项以@:pn{空格}分隔。}

相对于@algo-ref{read-location-ids}的晦涩，
@algo-ref{accumulate}倒是直白很多。

由@tech{缩进}可知，
@algo-ref[#:line 'accumulate]{accumulate}归属于@algoref[#:line 'for-each]{accumulate}，
它们共同构成了一种符合人类思维习惯的@:term{循环结构}。
因此，人类读者在阅读时不用考虑往回跳转，
读一遍就可以继续往后；
计算机会自动重复阅读，
直到循环结束才继续下一行。

@handbook-chunk[<sum>
 (for/sum : Natural ([a (in-list A.sorted-IDs)]
                     [b (in-list B.sorted-IDs)])
   (abs (- b a)))]

@aoc-story[@racket[1]]{
 当你在冥思苦想求解第一个谜题时学到的爆炸性信息时，
 精灵们又开始搞事了：
 天，这么点零食迟早会被吃完的！
 这可不行，你必须再找到另外两个所带食物提供了最多卡路里的精灵。
 这样，一只精灵的零食吃完了，
 咱还有俩可以继续腐败。
  
 请帮助精灵们@question{找出携带食物所提供卡路里最多的前三只精灵，
  并计算他们所带食物所能提供的卡路里总和。}}

根据题意可知，例子中携带食物提供卡路里最多的三只精灵分别是第四只（@racket[24000] 卡）、
第三只（@racket[11000] 卡）和第五只（@racket[10000] 卡），总计 @racket[45000] 大卡。

精灵们的这两次提问都比较拗口，但是不难看出，
用第二个谜题的思路来说，第一个谜题其实就是
@:desc{携带食物所提供的卡路里最多的前一只精灵所能提供的卡路里总和}。
换句话说，这两个谜题其实是一个问题，差别只在于数量的多少。
于是，自然而然地，@focus{不同数量相同类型数据可以构成数组、列表、元组等复合类型}。
在不同的语言里，这些数据类型都可以是很趁手的选择。

@handbook-scenario[#:tag "sec:sexpr"]{S-expressions}

现在，我想先讲点特别的。
你已经注意到了，Racket 代码写起来很奇怪，括号套着括号。
是的，它确实奇怪，这也是它不流行的原因之一，
但这种写法与数学的渊源却颇为深刻。

高级程序语言起源于上世纪50年代，
但那些无外乎是对机器语言的直白翻译（用英文单词来替代@racket[0]@racket[1]串），
语法也是互相借鉴。
总之，没有特别亮眼的创新，
特别是缺乏数学的美感。

Racket 是 Lisp 语系的一种方言，
也可以一直追溯到50年代，人工智能研究领域。
不过，至于 Lisp 到底是怎么来的，恐怕已经永远消失在历史的迷雾中了。
现在公认的看法是麻省理工学院的 John McCarthy 于1959年立项，
直接基于数学而发明的全新的程序语言。
动机很简单，影响却极为深远。
@idea{程序编制者应该关注问题本身，而不是被迫淹没在琐碎的机器细节里，
 而从人类思维到机器思维之间的脏活，该由程序语言来搞定}。
在 Lisp 的所有方言里，Racket 这一支最为看重数学理论的优雅和纯粹
@handbook-footnote{2010年代起 Racket 开始弥补在实用性方面的不足}。
总之，现今存在的绝大多数语言，它们宣传中那些出神入化的所谓“新”东西，
都是Lisp玩剩下了才被那些语言的作者发现而抄过去的。

Lisp 全称@:term{表处理语言}（@litchar{LIS}t @litchar{P}rocessor），
言下之意，列表是其基础数据类型，
用它@focus{求解问题的思路也就转化成了对列表的操作}。

Lisp/Racket 特殊在，它们连语法都写成了广义表的形式。
任意一类括号（@:pn{() [] @|"{}"|}）都能用来界定广义表，
广义表里的元素可以是常规数据，也可以是另一个广义表，甚至是自己套自己。
这种写法称作@handbook-deftech[#:origin "Symbolic Expression" #:abbr "sexp"]{符号表达式}，
是@focus{函数式递归在文法上的实现}。

文法设计方面的话题我不在这展开，直接说重点。
我们从幼儿园就开始学习的算术表达式，
其写法属于中缀记法，总是把操作符写在操作数的中间；
而 @tech{sexp} 是一种前缀记法，
@focus{总是把操作符写在操作数的前面}。
前缀记法的优点很多，比如无需考虑运算符优先级；
方便计算机处理。
最关键的是，
它@focus{确实更契合数学运算的语义}。

实际上，数学学得越深，数学表达式的写法就越五花八门。
比如一元二次方程求根公式和三角恒等式@$${
 \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} \\
 \sin^2\theta + \cos^2\theta = 1
}就已经混进去了不少奇怪的东西，肯定不能都算中缀记法。

初等数学中的二元运算符往往也能推广到多元场景。
以加法为例，@:desc{计算十个数的和}，与@:desc{计算两个数的和}，
在计算法则上没有任何区别。
但用中缀记法，需要写十个数、九个加号：
@$${ a + b + c + d + e + f + g + h + i + j }
而写成 @tech{sexp}，只需要十个数、一个加号（以及一对括号充当边界）：
@centered{@racket[(+ a b c d e f g h i j)]}
还有一些二元运算，没有约定俗成的记号，就只能写成函数形式。
比如求最大公约数的@${ \gcd(a, b) }，
这不就很像前缀记法了吗？括号不能省，还得增加逗号来分隔参数。
@:thus{@tech{sexp} 统一了以上（和其他）所有奇奇怪怪的写法：
 广义表的第一个元素代表操作符（函数），
 其余所有元素都是该操作符（函数）的操作数（参数）}，漂亮。
@linebreak[]

既然花了大量时间来说@tech{列表}，
那求解第二个谜题的工具就是它了，
以后我们也会经常用它解决别的问题。
此外，在定义函数的时候，
也应当在原题的基础之上适当考虑通用性。
万一又有精灵来整活呢？
@:desc{找一个}也好，@:desc{找三个}也罢，都只是@:desc{找n个}的特定案例。
因此，相较于求解第一个谜题的 @:id{find-total-distance} 函数，
这个复数形式的 @:id{find-total-distances} 函数增加了一个类型为 @:type{Natural} 的参数 @:var{b}：


@handbook-chunk[|<Puzzle 1: Find Total Distance>|
                (define find-total-distance <函数find-total-distance的类型签名>
                  (λ <函数find-total-distance的参数列表>
                    11))]

@handbook-chunk[|<Puzzle 2: Find Similarity Score>|
                (define find-total-distances : (-> Input-Port Natural Natural)
                  (λ [locin n]
                    (let rpcl ([self:cal : Natural 0]
                               <初始化高卡路里列表>)
                      (define line : (U String EOF) (read-line locin 'any))
                      (if (string? line)
                          (let ([cal : (Option Number) (string->number line 10)])
                            (if (exact-positive-integer? cal)
                                (rpcl (+ self:cal cal) calories)
                                (rpcl 0 <更新高卡路里列表>)))
                          <高卡路里合计>))))]

从上面这段碎片里，
我们也可以大致看出求解第一个谜题时被拆得稀碎的代码组装成最终程序后的样子。

算法逻辑大同小异，区别在于新的 @:id{rpcl}
函数里代表最大值的参数类型从 @:type{Natural} 变为了 @:type{(Listof Natural)}：

@handbook-chunk[<初始化高卡路里列表>
                [calories : (Listof Natural) (make-list n 0)]]

最终结果变为了@:desc{最大值列表的和}。

@handbook-scenario{高阶函数}

前面在比较 @tech{sexp} 和算术表达式的中缀记法时提到了多操作数加法的写法，这就来了个具体例子。
数的列表即是@:term{数列}，数学里有个专门的符号来表达@:desc{数列前n项的和}：
@$${ \sum_{i=1}^n a_i }
是不是感觉大脑突然宕机了？
不用管它，用 @tech{sexp} 写要简洁得多：

@handbook-chunk[<高卡路里合计>
                (apply + <更新高卡路里列表>)]

加法操作符（@:id{+}）前面多了一个 @:id{apply} 函数，
后面的参数从多个数变成了一个列表。
函数 @:id{apply} 的功能是将列表里的数按顺序取出，
再作为操作数全部喂给操作符 @:id{+}。
这个表达式的结果就是对列表中的所有数@:desc{执行加法运算}的结果。
这时候用 @tech{REPL} 来演示更好：

@tamer-repl[(+ 1 2 3 4 5 6)
            (apply + (list 1 2 3 4 5 6))]

还没结束，这段代码碎片引出了@tech{函数式编程}中的另一个重要概念：@:term{高阶函数}。
@handbook-deftech[#:origin "Higher-Order Function"]{高阶函数}的输入参数中至少有一个必须是其他函数，
或者求值结果是一个函数。
它们是@tech{函数式编程}中实现代码复用的强力工具，
也是初学者理解@tech{函数式编程}的一道砍。
@linebreak[]

最后是针对列表设计的更新最大值逻辑：
@:desc{将当前精灵所携带的卡路里插入到列表中更小的元素之前，并踢掉列表尾部的元素}。

不太好理解的样子。
给自己一点耐心，不妨先拿出纸和笔来模拟一下这个过程。
一开始，列表里全是 @racket[0]，
无论哪只精灵的卡路里清点好了，
他都是第一名，会占据列表的第一个位置。
以后每有一只精灵准备好了，
他都要跟那只精灵比，
比输了就让出第一名，往后挤走最后一名。
如果第一名很幸运没输，
那同样的戏码会在列表的第二个位置处上演。
依此类推。

以上模拟是为了理解问题，
真正的解决之道还在于@focus{将该问题转化为对列表的操作}。
@:desc{将自己挤进列表}实际上在做的事是
@:desc{将列表拆分成两个小列表，
 一个里面的元素都比自己大(或相等)，
 另一个里面的元素都比自己小，
 然后把这三部分按大小顺序连接成一条新列表，
 并只保留前 n 个元素}。

@handbook-sidenote*{虽然 Racket 提供了不少直接用于拆分列表的函数，
 但是今天已经接触太多让人炸裂的知识了，
 到最后这个环节就不折腾了，一切从简。}
从拆分的结果来说，
相当于对列表做了两轮筛选（对应着上述数据结构基本操作中的@:term{查找}）：
@:desc{第一轮筛选出列表中所有比当前精灵卡路里大(或相等)的元素；
 第二轮筛选出列表中所有比当前精灵卡路里小的元素。}
再执行一系列基本操作（连接列表用 @racket[append]、
截取列表用 @racket[take]，不再此详述），
由此得到的新列表即为下一轮循环的初始值。

于是，@handbook-chunk[<更新高卡路里列表>
                   (let ([tops (filter <高卡路里过滤函数> calories)]
                         [tail (filter-not <高卡路里过滤函数> calories)])
                     (take (append tops (list self:cal) tail) n))]

这段看似平平无奇的代码碎片引出了一个典型的高级列表操作：@racket[filter]。
不过，鉴于 @racket[filter] 函数的@tech{类型签名}对初学者来说太过犯规，
这里就不放出来了，我针对本题给个简单版本：
@handbook-sidenote*{注意，这段代码碎片没有包含在任何其他碎片中，因此不会出现在最终代码里。}

@handbook-chunk[<filter简易类型签名>
                (-> (-> Any Boolean) (code:comment "参数1类型：过滤函数")
                    (Listof Natural) (code:comment "参数2类型：自然数列表")
                    (Listof Natural))]

跟着注释不难看出，
@racket[filter] 函数接受一个过滤用的@tech{谓词函数}和一个自然数列表，
得到另一个自然数列表，而该列表中的元素均为原列表中能够满足过滤函数的元素。
同理，@racket[filter-not] 函数与 @racket[filter] 函数的@tech{类型签名}相同，
但像它的名字一样，得到的结果列表中的元素均不能满足过滤函数。
理清这一点后，过滤高卡路里的@tech{谓词函数}就不难理解了：

@handbook-chunk[<高卡路里过滤函数>
                (λ [[peak:cal : Natural]]
                  (peak:cal . >= . self:cal))]

显然，这个@racket[<高卡路里过滤函数>]只会被用一次，
因此不必非得给它起名字。
好吧，这里用了两次，
但两次产生的函数本体虽然一摸一样，
它们也仍然是不同的@tech{值}。
可理解为同卵双胞胎。

匿名函数和@tech{高阶函数}是绝配，
经后我们还会学习其他更数学的产生匿名函数的方法。

至此，今天的任务终于完成了：

提交这个答案，解锁明天的任务。

文中出现了好几处理论性较强的段落，
你现在看不懂才是正常现象，
有待我们在后续任务中慢慢体会。
最重要的是，
@idea{我在帮你种@tech{函数式编程}的种子，
 相信他日定会发芽}。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
