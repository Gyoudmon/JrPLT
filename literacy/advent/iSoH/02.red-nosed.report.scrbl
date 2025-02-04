#lang scribble/lp2

@(require "../literacy.rkt")

@require{../../share/diagram/aoc/2024/02.rnr.rkt}
@require{../../share/timeline.rkt}

@(require digimon/digitama/tamer/pseudocode)
@(require racket/math)

@(require geofun/vector)
@(require diafun/flowchart)
@(require plotfun/axis)

@(define diaflow-scale 0.50)
@(define diaflow-node-scale 0.36)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@aoc-task[2024 2]{大红鼻子的报告}

@aoc-desc[#:keywords ["字符" "字符串"]
          #:edition [一 "2025-02-03"]]

本章没有新的数学理论知识，
主要帮助读者加深对已学知识的理解。
顺便多关注一下描述@tech{算法}时要注意的细节，
并因此引入少量较基础的数据类型。

同时，也因为有了第一章的铺垫，
本章开始的正文部分可以省略很多废话。

@handbook-sidenote{
 大红鼻子驯鹿体内的生化反应跟普通驯鹿不一样，
 它需要专门的核反应设施来供给所需分子。
 @linebreak[]
 @linebreak[]
 这些故事情境就为图一乐，
 它们可以不连续，也可以跨年联动。
 本例中提到的“那年”是指 2015年，
 鲁道夫病了，
 你帮忙校准了核反应机器用以生成适合它的药。}

@aoc-story[@racket[|<Puzzle 1: Count Safe Reports>|]]{
 幸运的是，精灵小队要去探查的第一个地址距离首席历史学家的办公室并不远。

 红鼻子驯鹿核反应站并未提供首席历史学家去向的线索，
 那边的精灵工程师一看到你就一窝蜂涌过来。
 客套中，他们提起了领头驯鹿鲁道夫(Rudolph)，
 当年多亏了你的“单电子分子合成”分析才救了它的命。

 当你驻足后，精灵工程师立即补充道，
 要是你能顺道帮他们分析一下核反应堆报告里的非常规数据就再好不过了。
 你想征求下精灵历史学者的意见，
 发现他们早就分成好几个小组去探查设施的每一个角落了，
 你确实可以闲下来看看那些非常规数据。

 非常规数据由一个个@aoc-emph{报告(Report)}构成，
 每@aoc-emph{行(Line)}一个；
 报告内容是个@tech{数列}，
 每个数字代表一个@aoc-emph{强度等级(Level)}，
 用@aoc-emph{空格(Space)}分隔。

 @(tamer-filebox #:tag '01_rnr.aex (aoc-tamer-path "iSoH/02_rnr.aex"))
 
 精灵工程师们需要知道哪些报告是@aoc-emph{安全(Safe)}的。
 红鼻子核反应堆只能承受强度等级的平稳升高或降低，
 因此，安全的报告应该同时满足以下两个条件：

 @handbook-itemlist[
 #:style 'compact
 @item{所有的强度等级要么@aoc-emph{全部递增(increasing)}，要么@aoc-emph{全部递减(decreasing)}；}
 @item{相邻强度等级的差@aoc-emph{不低于1级}、@aoc-emph{不高于3级}。}
 ]

 问：@aoc-question{How many reports are safe}?}

本例中包含6个报告，每个报告都有5个强度等级。
按如下规则检查它们是否安全：

@handbook-itemlist[
 #:style 'compact
 
 @item{@racket[7 6 4 2 1]，@emph{安全}，因为所有等级按 @racket[1] 或 @racket[2] @emph{递减}；}
 @item{@racket[1 2 7 8 9]，@emph{不安全}，因为从 @racket[2] 到 @racket[7] 跳了 @racket[5] 级；}
 @item{@racket[9 7 6 2 1]，@emph{不安全}，因为从 @racket[6] 到 @racket[2] 跳了 @racket[4] 级；}
 @item{@racket[1 3 2 4 5]，@emph{不安全}，因为从 @racket[1] 到 @racket[3] 是升级，紧接着就开始降到 @racket[2] 级了；}
 @item{@racket[8 6 4 4 1]，@emph{不安全}，因为出现了两个连续的 @racket[4] 级；}
 @item{@racket[1 3 6 7 9]，@emph{安全}，因为所有等级按 @racket[1]、@racket[2] 或 @racket[3] @emph{递增}；}
 ]于是，共计 @racket[2] 个安全的报告。

@handbook-scenario{算法设计}

第一章为了让读者能快速见到第一个可以把玩的程序，
我从@emph{主线任务}里拆出了一个@emph{辅助任务}专门用于读取文件里的数据。
简单从工程设计角度总结一下这么做的好处：

@handbook-itemlist[
 #:style 'compact

 @item{@emph{辅助任务}作为一种比较高级的@emph{@tech{算法}积木}，@focus{
   给它们起个好名字}，
  作者在@emph{主线任务}中使用时就像@focus{说大白话一样自然}；
  读者看到了也更容易理解任务逻辑。
  @focus{作者也是第一个读者}。}
 
 @item{对于“读文件”这样的@emph{辅助任务}，
  它把一类典型的@tech{副作用}隔离在一个@emph{积木}里，
  使得程序整体上看起来更贴近数学。

  @handbook-itemlist[
 #:style 'compact

 @item{@focus{@tech{函数式编程}不鼓励直接修改已经存在的@tech{值}，
     因此无论后续任务多复杂，数据都只需要读一次即可。}
    @:thus{我们不必小心翼翼，
     生怕哪一项操作不小心修改了原数据，
     结果导致后面的任务败得莫名其妙。}}]}
 ]

本章继续沿用第一章的习惯，
今后还会更深入地学习这种做法的其他好处。

今天的任务也比较简单，只有一个@emph{辅助任务}和两个@emph{主线任务}：

@handbook-chunk[<report:*>
                (module advent typed/racket
                  |<Helper: Read Reports>|
                  |<Puzzle 1: Count Safe Reports>|)]

@tamer-figure-margin['name-map "任务命名习惯举例"]{@(geo-scale name-map.dia diaflow-marginfigure-scale)}

本书对任务命名的习惯见@fig-ref{name-map}。
注意，碎片名称和算法名称多少有点“标题”性质，
因此要首字母大写。

@aoc-complain{
 数据型@tech{变量}名通常会以名词或名词的修饰语为主；
 @tech{函数}型@tech{变量}名通常会以谓语为主，
 因为它们通常代表某个动作或某项提问(如@tech{谓词函数})。
 如果名字包括多个单词，
 Racket 习惯把分隔单词的空格替换成连字符(@:pn{-})而@:err{不建议}用下划线(@:pn{_})@handbook-footnote{
  这也是 @tech{sexp} 带来的好处，单词中的连字符不会被误读成减号。
  其他语言不得不用下划线以示区分。}。

 顺便，语言词汇的发展遵循一个规律：
 @focus{复合词常常由独立词组演变而来，
  演变的中间阶段就是把空格替换成连字符}。
 比如：@tt{wild life} ⇒ @tt{wild-life} ⇒ @tt{wildlife}。}

@handbook-action{提出正确的问题}

按照昨天的解谜步骤，
你应该已经开始在大脑中构思@emph{纸笔解谜}的过程。
今天的谜题更简单，照着例题解法就能一步到位直接得出结论。
但，你知道我的习惯。@focus{能自己解谜没啥了不起的
 }，谁还不会数数了，@focus{
 能用精练有效的语言教会计算机才是境界}。

科学界一直盛传着一句话：
@idea{提出正确的问题就等于解决了问题的一半}。@handbook-sidenote*{
 好多名人说过类似的话，或者好多名人被很多人声称说过类似的话。
 谁说的不重要，我认可英雄所见略同。
}我先提几个问题(你可以借着昨天的任务对比回答)：

@handbook-itemlist[
 #:style 'ordered
 
 @item{读取文件的目的是什么？}
 @item{如何用精练的语言表述其目的？}
 @item{是否所有@emph{报告}里的@emph{等级数量}都是5个?}
 ]

回忆昨天的@emph{辅助任务}，
@:desc{读取文件}的目的是@:desc{@emph{构造}@litchar{两组}精灵写下的地址编号@tech{列表}}。
进而可用@tech{类型签名}更精练地表述为：
@centered{@racketblock[(-> Input-Port (Values (Listof Natural)
                                              (Listof Natural)))]}

于是，对于前两个问题。
本例@:desc{读取文件}的目的可类推为@:desc{@emph{构造}@litchar{一组}报告@tech{列表}}，
可得其@tech{类型签名}为 @racket[(-> Input-Port (Listof Natural))]……

确定吗？谜题里说了，@emph{报告}的内容也是个@tech{数列}。
因此，正确的@tech{类型签名}应该是

@handbook-chunk[|<type signature of read-reports>|
                (-> Input-Port (Listof (Listof Natural)))]
换句话说，本例中我们读文件的目的是构造出一个@emph{自然数列表}的@tech{列表}。

第三个问题很重要，因为你很可能被例子误导了。
例子里所有@emph{报告}都包含有固定的@racket[5]个@emph{等级}，
但这不是硬性规定，虽然谜题也没有明说。
例题中的数目不可靠，
故事中的@emph{每行一个}和@emph{用空格分隔}才是靠谱线索，
它们决定了你@focus{不能直接照搬昨天的思路}。

@handbook-action{字符与字符串}

由此因此了第四个问题：

@handbook-itemlist[
 #:style 'compact
 
 @item{计算机读文件和人类读文件有什么相似和不同?}
 ]

第一章我们“读取”文件的方法是雇佣了名字最直白的 @:id{read} 函数，
我还特别强调了一下@emph{要假装自己是英语母语人士}，
@focus{以连续空格为界}，
每次 @:id{read} 都会得到下一个单词、符号或数字，
关键是@focus{@emph{换行}也是空格的一种}。
也就是说，在不确定一行有多少个数字的情况下，
我们就必须解决这个问题：
@question{如何判断是否读到了行尾}？

提示这么明显，我们已经可以写出@tech{算法}的大致步骤了：

@algo-pseudocode[
 #:tag 'desc:read-reports "Read Reports（大白话版）"
 @list['|read line|]{尝试从文件的当前位置@:in{读取}一行}
 @list['|is a line?|]{@emph{若} 确实是个行，@emph{则}：}
 @list['|split line|]{@hspace[4]将该行@emph{拆分}成列表，作为一个报告}
 @list['|cons reports|]{@hspace[4]将该报告加到报告列表的头部位置}
 @list['loop]{@hspace[4]@emph{回到}@algo-goto['|read line|]重复执行}
 @list['done]{@emph{否则} @:cmt{; 报告列表构造完毕，告知结果}}
 ]

与之前的@tech{伪代码}相比，@handbook-sidenote*{
 信不信由你，我写的时候没控制住，一不小心笑出了声。
}本章@tech{伪代码}的步骤名用词精准了很多。
不过描述部分读起来有些怪怪的，
甚至让人摸不着头脑。
比如，啥叫“确实是个行”？

从常识来说，“行”不难理解，
就是书本上再常见不过的横着写的@emph{整排}文字，
一行写不下了要“另起一行”继续写；
读的时候也是，眼睛按“行”横扫内容，
扫到“行尾”就“另起一行”从头开始扫描。
你看，“读”和“写”都有一个“另起一行”的动作。
那计算机会如何理解这个“另一起行”呢？


那么，准备好了没？我们即将面对不说人话的@tech{算法}描述。

@algo-pseudocode[
 #:tag 'algo:rpsl "Read Location IDs(代数版)"
 @list['initialization!]{@emph{设} @${X}、@${Y}分别是@focus{初始}代表甲、乙两组地址编号列表的@emph{空列表}}
 @list['|read IDs|]{尝试从文件当前位置@:in{读取}两个@:type{自然数}，@emph{设}为@${a}、@${b}}
 @list['predicate?]{@tt{if}@hspace[2]@:pn{@${a}和@${b}确实都是@:type{自然数}}, @tt{then}}
 @list['|cons X|]{@hspace[4]@emph{令} @focus{新}@${X = a:X}}
 @list['|cons Y|]{@hspace[4]@emph{令} @focus{新}@${Y = b:Y}}
 @list['loop]{@hspace[4]@emph{回到}@algo-goto['|read IDs|]重复执行}
 @list['No]{@tt{else} @:cmt{; 此时的 @${X}、@${Y} 分别指代甲、乙两组地址编号列表}}
 @list['done]{@hspace[4]@:cmt{; 告知结果}}
]

于是，用数学语言(和代码)翻译@algo-ref{desc:read-reports}后我们得到了@algo-ref{algo:rpsl}。
怎么样，汗流浃背了没？深呼吸，放轻松。
这个@tech{算法}每一步做了什么事都已经确定了(和大白话版本的描述对比即可)，
只是选用了诸多怪异的符号来书写。

@;tamer-figure!['flow:rpsl
               @;list{@algo-ref{algo:rpsl} 流程图，后接@fig-ref{flow:puzzle1}或@fig-ref{flow:puzzle2}}
               @;(tamer-delayed-figure-apply #:values geo-scale #:post-argv '(0.50)
               @;                             make-hh-helper.dia 'flow:puzzle1 'flow:puzzle2)]

@handbook-action{类型化程序语言描述}

回看@algo-ref{algo:rpsl}和@fig-ref{flow:rpsl}，
它们真的能帮你记忆关键线索吗？
确实很勉强，上面我给各个步骤起名时做了个@:err{很不好的示范}，
简单来说就是@:err{词不达意}，或@:err{过于言简意赅}了。
这个问题值得我说得更详细一些：

@handbook-scene{读取-判断-扩列 循环}

所有的@tech{算法}也都应该有个名字，
而@:term{循环}是绝大多数程序的典型特征。
因此，不妨把@algo-ref{algo:rpsl}称作@:term{读取-判断-扩列 循环}，
缩写为 @:id{rpsl}(read-predicate-construct loop)。
于是：

@handbook-chunk[|<read-predicate-split loop>|
                (let rpsl (<init!>)
                  |<read reports>|
                  (if <string-line?>
                      |<等级报告扩列，迭代下一轮>|
                      <finish>))]

@focus{@tech[#:key "函数式编程"]{函数式}代码惊人的短小精悍}有没有？
之后的活就是把那些与步骤名对应的代码碎片一个个翻译成真实代码。
注意区分，
代码碎片用@emph{角括号}(@:pn{< >})界定，
@tech{伪代码}步骤名用@emph{角引号}(@:pn{« »})界定。
本例中它们大致上一一对应，
除了@focus{用晦涩的中文}写出来的那句。

@algo-ref[#:line 'initialization!]{algo:rpsl}没有强调类型，
在这补上：

@handbook-chunk[<init!>
                [A.IDs : (Listof Natural) null] (code:comment "甲组精灵写下的地址编号列表")
                [B.IDs : (Listof Natural) null] (code:comment "乙组精灵写下的地址编号列表")]

本例中的两个@tech{变量}的类型均为@:term{自然数列表}(@:type{(Listof Natural)})，
初始值@:term{空列表}写作 @:val{null}。

此外，本段代码碎片将@tech{变量} @${X}和@${Y}重命名成了 @:var{A.IDs} 和 @:var{B.IDs}。

@algo-ref[#:line '|read IDs|]{algo:rpsl}就是字面直译，
@:desc{对清单文件 @:var{rptin} 执行 @racket[read] 操作，给结果起名为 @${a}；
 重复一次，给结果起名为 @${b}}：

@handbook-chunk[|<read reports>|
                (define a : Any (read rptin))
                (define b : Any (read rptin))]

@tamer-figure-margin['read-line.dia @list{@algoref[#:line '|read IDs|]{algo:rpsl}}]{
 @(geo-scale read.dia diaflow-marginfigure-scale)}

函数 @:id{read} 的求值结果可以是@emph{任何}（@:type{Any}）合理类型的@tech{值}，
其中包括特殊值 @tech{eof}，也即清单读完了。
注意，此时我们要假装自己是英语母语人士，
将文件里的@:term{连续空格}(包括@:term{换行})视作单词、数字和符号的分隔符。
因此只管@emph{读}，不用考虑换行。
这也意味着，@:id{read} 自己知道@emph{当前位置}在哪里，
就像我们在看书时，眼睛会跟着一起移动(@fig-ref{read.dia})。

本例中，我们可以放心地假设，
@${a} 和 @${b} 要么是@emph{自然数}，要么是 @tech{eof},
不存在其他可能。
于是，@algo-ref[#:line 'predicate?]{algo:rpsl} 就是对结果提问：@:desc{
 @${a}是自然数吗？@${b}是自然数吗？}
并且要同时回答 @racket[#true](@emph{是})才算条件达成(@fig-ref{predicate.dia})：

@handbook-chunk[<string-line?>
                (and (exact-nonnegative-integer? a)  (code:comment "a 是自然数吗？")
                     (exact-nonnegative-integer? b)) (code:comment "b 是自然数吗？")]

@tamer-figure-margin['string-line.dia @algoref[#:line '|predicate?|]{algo:rpsl}]{
 @(geo-scale predicate.dia diaflow-marginfigure-scale)}

按照正常说话顺序，“@:desc{若 @algoref[#:line 'predicate?]{algo:rpsl}，
 则 @algoref[#:line (cons 4 6)]{algo:rpsl}，
 否则 @algoref[#:line (cons 7 8)]{algo:rpsl}}”，
在得到 @${a} 和 @${b} 的回答之后，
我们会先处理@emph{条件成立}时要做的事：

@handbook-chunk[|<等级报告扩列，迭代下一轮>|
                (rpsl (cons a A.IDs)  (code:comment "构造新的甲组编号列表，保证 a 成为原列表的头部")
                      (cons b B.IDs)) (code:comment "构造新的乙组编号列表，保证 b 成为原列表的头部")]

这段代码碎片的名字相当晦涩，写出来倒是意外地简单。
它蕴含了@tech{函数式编程}的基本原理，暂不详述。
先忍耐一下，如果实在憋不住，
可以先从@Secref{sec:fp}看起，
到@Secref{sec:rloop}结束。

于是，就剩最后一句了。
当@emph{条件不成立}时，
@algo-ref{algo:rpsl}就直接结束了：

@handbook-chunk[<finish>
                (list A.IDs B.IDs)]

@handbook-scene{类型签名}

从解谜的角度来说，我们已经打好草稿了；
从答题的角度来说，还缺少一步：把草稿誊写到答题卡上。

先看一下代码碎片@racket[|<read-predicate-split loop>|]组装完成后的样子：

@handbook-sidenote*{同样，本段代码碎片也没有包含在任何其他碎片中，因此不会出现在最终代码里。}
@handbook-chunk[<rpsl>
                (let rpsl ([A.IDs : (Listof Natural) null]  (code:comment "甲组精灵的地址编号列表")
                           [B.IDs : (Listof Natural) null]) (code:comment "乙组精灵的地址编号列表")
                  (define a : Any (read rptin))
                  (define b : Any (read rptin))

                  (if (and (exact-nonnegative-integer? a)  (code:comment "a 是自然数吗？")
                           (exact-nonnegative-integer? b)) (code:comment "b 是自然数吗？")
                      (rpsl (cons a A.IDs)  (code:comment "构造新的甲组编号列表，保证 a 是头部")
                            (cons b B.IDs)) (code:comment "构造新的乙组编号列表，保证 b 是头部")
                      (values A.IDs B.IDs)))]

全景下的@racket[|<read-predicate-split loop>|]就剩一个疑点了：
@:stx:def{let} 就是数学解答题和证明题中常用的@:term{设}或@:term{令}，
如果顺便给 @:stx:def{let} 起个名字，
就会同时定义一个同名@tech{函数}，@focus{并立即调用它}。
本例中，我们定义的@tech{函数}叫做 @:id{rpsl}，带两个参数。
之所以在定义时就火急火燎地调用它，
是因为它只是个@emph{临时}@tech{函数}，
写完就随手扔进垃圾桶了，
这种匪夷所思的事经常发生在你@emph{不得不记笔记}时。
而习惯较好的做法是把它放置在另一个正式@tech{函数}里，方便随叫随用。
这个正式@tech{函数}便是本节一开始就提到的@:term{辅助任务}碎片：

@handbook-chunk[|<Helper: Read Reports>|
                (define read-reports <函数read-reports的类型签名>
                  (λ <函数read-reports的参数列表>
                    |<read-predicate-split loop>|))]

@handbook-chunk[|<Puzzle 1: Count Safe Reports>|
                2222]

@handbook-chunk[<函数read-reports的类型签名>
                : (-> Input-Port (Listof (Listof Natural)))]

跟@tech{变量}类型一样，
@tech{函数}的类型信息也跟在用空格隔开的单个冒号（@:pn{:}）后面。
@focus{@:pn{(-> )}表明这个类型代表的是@tech{函数}，
里面的最后一个类型代表@:term{陪域}类型；
其余类型按顺序@emph{依次}表示每一个@tech{输入}参数的类型。}
本例中@tech{输入}参数只有一个，
而那个大写的 @:stx{Values} 带出了两个结果@tech{值}。
于是，函数 @:id{read-reports} 的功能可以简单表述为：
@:thus{给定一个类型为@:term{输入流}（@:type{Input-Port}）的@tech{输入}参数，
 得到两个类型为@:term{自然数列表}（@:type{(Listof Natural)}）的结果。}
完美匹配了辅助任务的目标：
@:desc{阅读谜题提供的清单文件，得到两组精灵写下的编号列表。}

@focus{@tech{类型签名}只强调参数的类型，不强调参数的名字}。
因为计算机不懂名字的寓意，它们更擅长通过类型来理解程序；
而人类读者更习惯通过@emph{有意义的名字}来理解内容。
因此，我们把类型为 @:type{Input-Port} 的参数命名为 @:var{rptin}（即
@litchar{loc}ation ID @litchar{in}put-port 的缩写）：

@handbook-chunk[<函数read-reports的参数列表>
                [rptin]]

你注意到代码碎片@racket[|<read reports>|]里也用到@tech{变量} @:var{rptin} 了吗？
这也是 @:id{rpsl} 草稿函数实锤的另一个重要原因：
它知道如何解谜，但是没有花精力去关注谜题里的数据到底从哪来。
当它被放置在 @:id{read-reports} 里时，
就自动共享了@:term{输入流} @:var{rptin}。
@:desc{读取数据}就像@:desc{打开水龙头就有水流出来}一样自然。

最后再唠叨一句，
在将@tech{伪代码}翻译成程序之前，
我首先强调了要给@tech{算法}起名。
不知你注意到了没，
@algo-ref{algo:rpsl}本来也是有名字的，
在它自己序号右边，
那个醒目的@racket["Find Location IDs(大白话版)"]。
只不过在实际程序中，
一个名字可能不是那么地够用。

于是，
接下来我们实际运行代表@emph{辅助任务}@racket[|<Helper: Read Reports>|]的函数 @:id{read-reports}，
看看它到底会@tech{输出}些啥：

@handbook-sidenote{@(tamer-filebox (aoc-tamer-path "iSoH/02_rnr.aex") #:path-centerized? #true)}
@tamer-repl[#:requires ["../aoc.rkt"]
            ($ read-reports < "iSoH/02_rnr.aex")]

@handbook-scenario{闭幕}

冒险越来越深入了……

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
