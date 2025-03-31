#lang scribble/lp2

@(require "../literacy.rkt")

@require{02.report.rkt}
@require{../../share/timeline.rkt}

@(require digimon/digitama/tamer/pseudocode)
@(require racket/math)

@(require geofun/vector)
@(require diafun/flowchart)
@(require plotfun/axis)

@(define diaflow-scale 0.50)

@(define line-charize
   (lambda [line]
     (apply string-append
            (for/list ([ch (in-string (string-append line (string #\newline)))])
              (format "~s " ch)))))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@aoc-task[2024 2]{大红鼻子的报告}

@aoc-desc[#:keywords ["字符" "字符串" "联合类型" "可选类型"]
          #:edition [一 "2025-02-10"]]

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
 精灵小队要去探查的第一个地址从首席历史学家的办公室走着就能到。

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

 @(tamer-filebox #:tag '02_rnr.aex (aoc-tamer-path "iSoH/02_rnr.aex"))
 
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

@handbook-scenario{起点碎片}

昨天为了让读者能快速见到第一个可以把玩的程序，
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
     因此无论后续任务多复杂，数据都只需要读一次即可。
    }@:thus{我们不必小心翼翼，
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

@tamer-figure-margin['name-map "任务命名习惯举例"]{@(geo-fit name-map.dia aoc-mparwidth 0.0)}

本书对任务命名的习惯见@fig-ref{name-map}。
注意，任务碎片名和算法名在本书中多少带点“标题”性质，
因此要首字母大写。

@aoc-bonus{
 数据型@tech{变量}名通常会以名词或名词的修饰语为主；
 @tech{函数}型@tech{变量}名的核心应该是谓语，
 因为它们通常代表某个动作或某项提问(如@tech{谓词函数})。
 如果名字包括多个单词，
 Racket 习惯把分隔单词的空格替换成连字符(@:pn{-})而@:err{不建议}用下划线(@:pn{_})@handbook-footnote{
  这也是 @tech{sexp} 带来的好处，单词中的连字符不会被误读成减号。
  其他语言不得不用下划线以示区分。}。

 顺便，语言词汇的发展遵循一个规律：
 @focus{复合词常常由独立词组演变而来，
  演变的中间阶段就是把空格替换成连字符}。
 比如：@tt{wild life} ⇒ @tt{wild-life} ⇒ @tt{wildlife}。}

@handbook-scenario{提出正确的问题}

按照昨天的解谜步骤，
你应该已经开始在大脑中构思@emph{纸笔解谜}的过程。
今天的谜题较简单，照着例题解法就能一步到位直接得出结论。
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

@racketblock[(-> Input-Port (Values (Listof Natural)
                                    (Listof Natural)))]

于是，对于前两个问题。
本例@:desc{读取文件}的目的可类推为@:desc{@emph{构造}@litchar{一组}报告@tech{列表}}，
可得其@tech{类型签名}为 @racket[(-> Input-Port (Listof Natural))]……

确定吗？谜题里说了，@emph{报告}的内容也是个@tech{数列}。
因此，正确的@tech{类型签名}应该是

@handbook-chunk[|<type signature@read-reports>|
                (-> Input-Port (Listof (Listof Natural)))]
换句话说，本例中我们读文件的目的是构造出一个@emph{自然数列表}的@tech{列表}。

第三个问题很重要，因为你很可能被例子误导了。
例子里所有@emph{报告}都包含有固定的@racket[5]个@emph{等级}，
但这不是硬性规定，虽然谜题也没有明说。
例题中的数目不可靠，
故事中的@emph{每行一个}和@emph{用空格分隔}才是靠谱线索，
它们决定了你@focus{不能直接照搬昨天的思路}。

@handbook-action{新类型：字符与字符串}

昨天我们读文件时使用了名字最直白的 @:id{read} 函数，
我还特别强调@emph{要假装自己是英语母语人士}，
@focus{以连续空格为界}，
每次 @:id{read} 都会得到下一个单词、符号或数字，
关键是@focus{换行符也是空格的一种}。
也就是说，在不确定一行有多少个数字的情况下，
我们就必须先解决这第四个问题：

@handbook-itemlist[
 #:style 'compact
 
 @item{“行尾”到底是什么？如何判断是否读到了“行尾”？}
 ]

一方面，@emph{空格}不是我们普通话母语(书写系统)的组成部分，@handbook-sidenote*{
 其实，在英语母语人士中，
 文盲的脑子里也没有空格。
}你很可能从未重视过它们；
另一方面，
上文提到的“每次 @:id{read} 都会得到下一个单词、符号或数字”，
这个表达不仅啰嗦，
而且……它竟然都没有提“汉字”。这……
@natural-language-isnt-natural*
即使我再加上一个“汉字”，也仍然有可能惹毛其他母语读者。
像这种@emph{企图一个个把所有可能都列举出来的}表述思路显然不靠谱，
@:thus{需要定义个新的概念来概括它们}。

从计算机的角度来说，反正它不理解任何人类交流用的自然语言，@handbook-sidenote*{
 对，包括英语。
 计算机的母语并不是英语，
 虽然它起源于英语世界。
 而为了方便人类自己互相交流，
 我们才尽量把程序写得@emph{像}英语。
 此外，这几年迅速发展中的大语言人工智能是另一个话题，请勿混淆。
}那干脆就把人类书写出来的“文字符号”全都当成一类东西就行了。
于是，

@handbook-itemlist[
 #:style 'compact

 @item{@emph{单个的}字母、数字、汉字、标点符号、空格……
  它们都叫做@handbook-deftech[#:origin "Character" #:abbr "Char"]{字符}；
  用于判断某个@tech{值}是否是@tech{字符}的@tech{谓词函数}叫做 @:id{char?}。}

 @item{@emph{一个或多个字符按书写顺序串联在一起}就成了@handbook-deftech[#:origin "String"]{字符串}。
  用于判断某个@tech{值}是否是@tech{字符串}的@tech{谓词函数}叫做 @:id{string?}。}
 ]

为避免与程序中已经存在的各种@tech{变量}名和@tech{值}混淆，
程序语言会选用不同的@emph{标点符号}来标记@tech{字符}和@tech{字符串}的@tech{值}。
在 Racket 中：

@handbook-itemlist[
 #:style 'compact

 @item{@tech{字符}类型的@tech{值}会带上前缀 @litchar{#\}。
  比如：@racket[#\A]，@racket[#\0]，@racket[#\字]，@racket[#\？]。}
 @item{@tech{字符串}类型的@tech{值}会被@focus{英文双引号}(@:pn{""})围住。
  比如：@racket["π"]，@racket["3.14"]。}
 ]

接下来给例题文件加个“滤镜”，
从@tech{字符}视角看看它内部到底长啥样：

@(tamer-filebox #:tag '02_rnr/char.aex (aoc-tamer-path "iSoH/02_rnr.aex") #:line-map line-charize)

这下文件里的所有@tech{字符}都现了原型。@handbook-sidenote*{
 为方便观察，我的“滤镜”在每个@tech{字符}中间都增加了一个空格。
}正常的是，@emph{单个}数字如约给自己加上了 @litchar{#\}；
不正常的是，
混进了 @racket[#\space] 和 @racket[#\newline] 这两个怪胎。
不过，从名字上你应该能猜到，
@racket[#\space] 就是空格，
而 @racket[#\newline] 很明显就是“新行”嘛。

上面我强调了@emph{空格}包含@emph{换行}，
实际上更严谨的说法应该是，
@emph{空格}和@emph{换行}都属于@handbook-deftech[#:origin "White Space"]{空白字符}，
即写出来不会有墨迹的看不见的@tech{字符}。
除了这俩，@tech{空白字符}还有很多，
为方便人类阅读，
Racket 在显示这些@tech{字符}的@tech{值}时就把它们的名字接在 @litchar{#\} 后面了。

至于代表@emph{文件结尾}的 @tamer-deftech[#:origin "End of File"]{eof}，
它并不作为@tech{字符}出现在文件里，
而作为一种状态指示@emph{再也没有更多@tech{字符}可读了}。

好的，回答问题的理论知识已经足够了。
简而言之，既然 @:id{read} 函数会跳过@tech{空白字符}，
那我们换个其他不会跳过@tech{空白字符}的@tech{函数}来读就是了。
比如: @:id{read-char} 函数。
顾名思义，其功能是@:desc{每次读取一个@tech{字符}}。
于是，第四个问题的答案就是@:desc{重复 @:id{read-char}，
 直到读到了 @racket[#\newline]，
 @emph{或}出现 @tech{eof} 状态}(@fig-ref{read-char.dia})。

@tamer-figure-margin['read-char.dia @list{根据@tech{字符}判定行尾}]{
 @(geo-fit read-char-for-line.dia aoc-mparwidth 0.0)}

@handbook-action{一目一行: read-line 函数}

虽然 @:id{read-char} 函数可以完成任务，
但是想来@emph{读取一行}这个需求应该很常见，
理应再配个完成该功能的@tech{函数}才合理。
嗯，有这个想法说明你渐渐找到@emph{指挥计算机干活}的感觉了。
没错，Racket 确实提供了这样的@tech{函数}，
名字你也能猜到，就是很直白的 @:id{read-line}。

@tamer-figure*['read-comparison @list{重复六次读操作： @:id{read} vs. @:id{read-char} vs. @:id{read-line}}]{
 @(let ([sep (geo-hline aoc-linewidth 8.0 #:stroke 'LightGrey)])
    (geo-vl-append #:gapsize 4.0
                   (geo-fit read.dia      aoc-linewidth 0.0)
                   sep
                   (geo-fit read-char.dia aoc-linewidth 0.0)
                   sep
                   (geo-fit read-line.dia aoc-linewidth 0.0)))}

这三个常用@:desc{读}函数读取例题文件的效果对比见@fig-ref{read-comparison}。
虚线下方的@emph{文档框}里显示着文件里的剩余内容，
内容左上角对齐，所有@tech{字符}占据相同宽度的空间；
每次@:desc{读}到的@tech{值}显示在虚线上方。于是：

@handbook-itemlist[
 #:style 'compact

 @item{@:id{read} 函数只会跳过眼前的@tech{空白字符}，
  再次碰到@tech{空白字符}时停下，留到下次读时再跳过。
  第五次 @:id{read} 之后第一行就剩@emph{换行符}了，
  但它没有墨迹，所以第二行上方空了一段。
  @:id{read} 函数比较超神，这里先不细聊，
  憋不住可以先看@Secref{sec:read}。}

 @item{@:id{read-char} 函数平平无奇，
  不急不忙一个@tech{字符}一个@tech{字符}地读。}

 @item{@:id{read-line} 函数雷厉风行，刷刷几下就全部读完了，
  留下个落寞的 @:val{@(format "~a" eof)} 标志。}
 ]

@aoc-complain{
 如果你来自 Python，请务必注意一个细节：
 @:id{read-line} 函数读到的行是个@tech{字符串}类型的@tech{值}，
 但是这个@tech{值}的末尾并@:err{不包含}@emph{换行符}本符。

 @emph{换行符}在不同操作系统里的@tech{字符}@tech{值}不尽相同。
 比如，Windows 的“换行”步骤是先“回车(回到行首)”再“另起一行”。
 也就是说，它的@emph{换行符}包含 @racket[#\return] 和 @racket[#\linefeed] 两个@tech{字符}。
 如此拧巴，没想到吧？哈哈哈哈哈哈……

 看起来有些语言觉得它有义务让你不丢失这些信息，
 就把@emph{换行符}也塞到读到的@tech{字符串}末尾了。
 是不是有种“脱了裤子那啥”的感觉……

 真是行行出奇葩……
}

@handbook-scene{联合类型}

再从@tech{类型签名}的角度细看一下它们的区别：

@tamer-repl[#:requires ["../aoc.rkt"]
            (:query-type/args read Input-Port)
            (:query-type/args read-char Input-Port)
            (:query-type/args read-line Input-Port)]

类型标记中的 @litchar{U} 用与构造@handbook-deftech[#:origin "Union Type"]{联合类型}。
即，@emph{@tech{联合类型}的@tech{值}可以是参与@tech{联合}的所有类型中任意一个的@tech{值}}。
通俗地说就是“……或……或……”类型。显然，@emph{各类型顺序不重要}。
于是，@handbook-itemlist[
 #:style 'compact
 @item{@:id{read-char}
  函数求值结果的类型可以是 @:type{Char} 或 @:type{EOF}；@handbook-sidenote*{
   @:type{(U Char EOF)}}}
 @item{@:id{read-line}
  函数求值结果的类型可以是 @:type{String} 或 @:type{EOF}。@handbook-sidenote*{
   @:type{(U String EOF)}}}
 @item{@handbook-deftech[#:origin "Any"]{任意}类型特殊在，
  它可以看成是所有类型的@tech[#:key "联合类型"]{联合}，
  本就已经包括了 @:type{EOF}，
  因此不必再特别强调 @:type{(U Any EOF)}。}
 ]

@handbook-action{字符串操作}

从@tech{字符串}的定义可以大胆猜测，
它其实就是由@tech{字符}构成的@tech{列表}，
更确切地说是连续存储的@emph{顺序表}。
昨天我们已经体验过@emph{灵活操作@tech{列表}}可以提升思维、简化问题，
现在让我们试试从@emph{操作@tech{字符串}}的角度来思考问题、描述@tech{算法}。

@handbook-scene{分割字符串： string-split 函数}

前文已经提过，汉语的书写系统不包括@emph{空格}，
在理解中文句子的过程中，不需要你主动去思考，
我们的大脑就会悄悄完成巨量的分析工作(比如提取关键词、组合语义等)@handbook-sidenote*{
我们的大脑自动干的活越多，教会计算机的难度就越高。
}。理解英语句子的第一步是要正确从@emph{字母}组成的@tech{字符串}中识别一个个@emph{单词}，
因此，英语必须要有简单可行的办法帮助读者确定@emph{单词}边界。
时至今日，这个方法就是@emph{在单词之间多写一个空格}。

于是，@emph{将句子分割成单词}这个任务对我们来讲有点不自然，
但对英语母语人士来说就很稀松平常。
提供@tech{字符串}的语言应该顺便提供完成此任务的@tech{函数}才合理。
在英语中，这个过程有个名字叫做 @emph{split}，直译为“切割”或“分裂”。
类比的话就像@:desc{抽刀沿茎的节把甘蔗切成一个个小段}。
在 Racket 中，此操作多用于处理@tech{字符串}，
因此其作为@tech{函数}的名字是 @:id{string-split}，
默认以@tech{空白字符}为分隔符(@fig-ref{split.dia})。

@tamer-repl[(string-split "Day 2 Red-Nosed Reports")]
@tamer-figure-margin['split.dia
                     @list{@:id{string-split} 函数对句子使出了@emph{影·分裂爪}。
                      虽然效果拔群！但是没有句子受到伤害！}]{
 @(geo-fit (make-split.dia "Day 2 Red-Nosed Reports")
           aoc-mparwidth 0.0)}

@:id{string-split} 函数的求值结果是一个由@tech{字符串}构成的@tech{列表}(@:type{(Listof String)})。
也就是说，
其功能是@focus{从}一个长@tech{字符串}里@focus{提取}出一个个小@tech{字符串}(而非@tech{字符})，@focus{
 且原@tech{字符串}保持不变}。这两个细节值的我特别提一下。

@aoc-bonus{
 Racket 命名惯例，
 将@emph{数据类型}作为@emph{主语}附加在@emph{动作}前面，
 而非作为@emph{宾语}放在后面。
 在英语中，@emph{非生命}物体做主语本就是常规操作。
}

@handbook-scene{类型转化： string->number 函数}

这又是个稀罕操作。对于我们人类读者，
把看起来像数字的内容都解读成数字是大脑的默认配置，
大多数情况下不影响生活质量。
但如果计算机默认也这么干，
经常就会成为你教它干活的噩梦。

@aoc-bonus{
 假如，你从来没有以任何方式学过@emph{阿拉伯数字}，
 那你读到的任何写出来的东西都会被解读成“文字”或“符号”，
 而不是“数字”@handbook-footnote{@emph{数感}是生物本能，
  你会运用数字，但与之关联的@tech{字符}应该是本土的“一二三四”或“壹貳叁肆”，
  不大可能是来自他国的“1 2 3 4”。}，
 最多可能会觉得这个字长得很奇怪。
 但对于已经生活在现代社会的你来说，
 你反而很可能已经忘了“数字”首先也是“文字”，
 仅在数学语境下它才具有“数字”功能。
 
 思考，你的@emph{学号}是什么类型的@tech{值}？
 你和你同桌的学号相加、相乘有@emph{算术}意义吗？}

严谨的语言会提供一系列类型转化@tech{函数}帮助你在需要的时候@emph{明确}执行转化操作。
那么，这一次 Racket 又会提出什么令人拍案叫绝的习惯呢？先看例子：

@tamer-repl[(code:comment @#,list{@emph{阿拉伯数字@tech{字符串}} @${\rightarrow}@emph{数字}，成功})
            (string->number "7")
            (code:comment @#,list{@emph{英语数字@tech{字符串}} @${\rightarrow}@emph{数字}，失败})
            (string->number "seven")]

惊到了没？ Racket 类型转化类的@tech{函数}，
其名称就是两个类型的名字用@tech{字符}化的箭头(@:pn{->})连接到一起。
而@tech{字符串}类型本身过于宽泛，
没法保证其@tech{值}的实际内容一定是合乎数学规范的数字。
因此，@:id{string->number} 函数的求值结果也是个@tech{联合类型}：@handbook-sidenote*{
   @:type{(U Complex False)}@linebreak[]
  如果对 @:type{Complex} 有疑问，
  可回顾昨天提到的@tech{数系}。
 }@emph{要么}是 @:type{Complex} @emph{要么}是 @:type{False}。
通俗地说就是，存在失败可能，代表失败的是@tech{布尔型}的@tech{值} @racket[#false]。

像 @:type{(U Complex False)} 这样的@tech{联合类型}也叫做@handbook-deftech[#:origin "Optional Type"]{可选类型}，
在 Racket 中记为 @:type{(Option Complex)}。

@handbook-scenario{辅助任务：read-reports 函数}

回顾昨天的解谜过程，
在分析阶段我们日常生活的词汇就已经足够用了，
因此，有一个“大白话描述”环节。
而今天的解谜任务，
我们在一个个引导性提问中逐渐认识了@tech{字符}和@tech{字符串}这两个新概念(和与之配套的操作)，
进而丰富了我们描述问题的语料库。
因此，现在可以直接给出完成@emph{辅助任务}的@tech{伪代码}：

@algo-pseudocode[
 #:tag 'algo:rpsccl "Read Reports"
 @list['|initialize reports|!]{@emph{令} @${R} = @racket[null]，代表@focus{初始}的@emph{空}报告@:type{列表}}
 @list['|read line|]{尝试从文件当前位置@:in{读取}一行，@emph{设}为@${L}}
 @list['|is line a string|?]{@emph{若}@hspace[1]@:pn{@${L}确实是一行@:type{字符串}},@hspace[1]@emph{则}}
 @list['|split line|]{@hspace[4]@emph{令} @${ s = L }分裂所得的强度等级@:type{串}@:type{列表}}
 @list['|for levels|]{@hspace[4]为@emph{收集报告} @${r}，@emph{对于} @${s} 中的每一个等级@:type{串} @${l}， @emph{执行}}
 @list['|convert level|]{@hspace[8]将@:type{字符串} @${l} 转化为@:type{自然数}}
 @list['|cons reports|]{@hspace[4]@emph{令} @focus{新}@${R =} 将 @${r} 添加为 @${R} 首项的新报告列表}
 @list['loop]{@hspace[4]@emph{回到}@algo-goto['|read line|]重复执行}
 @list['No]{@emph{否则} @:cmt{; 此时的 @${R} 即是完整的报告列表(的倒序)}}
 @list['|reports|]{@hspace[4]@:cmt{; 告知结果}}
]

比之昨天的@tech{算法}描述，@algo-ref{algo:rpsccl}有了以下两点变化：

@handbook-itemlist[
 #:style 'compact
 @item{大多数@emph{步骤名}都写得更具体了，即使只看@emph{步骤名}也能准确脑补出该步的具体任务。}
 @item{步骤描述部分混合了字母符号和大白话，我觉得这样对初学者更友好一些。}
 ]

实际上，对于“读文件”这样有副作用的@tech{算法}，
步骤里本来就充满了很多看上去跟数学毫无关系的部分，
写出来就跟代码差不多了。
我们不需要如此多“重复”描述哈。

同时，为方便你记忆@tech{算法}线索，
@algo-ref{algo:rpsccl}的@tech{流程图}见@fig-ref{flow:rpsccl}。

@tamer-figure['flow:rpsccl
              @list{@algo-ref{algo:rpsccl} @tech{流程图}} @;后接@fig-ref{flow:puzzle1}或@fig-ref{flow:puzzle2}
              @(tamer-delayed-figure-apply #:values geo-scale #:post-argv (list 0.50)
                                           make-rnr-helper.dia)]

@handbook-action{读取-判断-分割-转化-扩列 循环}

也是因为有了昨天的基础，
今天无需先@emph{打草稿}再@emph{滕答案}。
可以一步到位，
直接在@emph{辅助任务}@tech{函数} @:id{read-reports} 内部包含一个@tech{草稿函数}。

@algo-ref{algo:rpsccl} 的核心仍是@emph{读取}、@emph{判断}、@emph{扩列} 循环，@handbook-sidenote*{
 这种思路得出的@tech{草稿函数}的名字未免过于冗长了，
 下一章我们会换个更优雅的方法规避它。
}在@emph{扩列}前要多做两个准备工作：@emph{分割}和@emph{转化}。
按照昨天的命名惯例，@tech{草稿函数}可定名为
@:id{rpsccl}(read-predicate-split-convert-construct loop)。
于是：

@handbook-chunk[|<Helper: Read Reports>|
                (define read-reports : |<type signature@read-reports>|
                  (lambda [rptin] (code:comment @#,list{@emph{输入流}参数名全称 @litchar{r}e@litchar{p}or@litchar{t} @litchar{in}put-port})
                    (let rpsccl (|<initialize reports!>|)
                      (define line (read-line rptin)) (code:comment @#,list{字面直译 @algoref[#:line '|read line|]{algo:rpsccl}})
                      (if (string? line) (code:comment @#,list{借助@tech{谓词函数}直译 @algoref[#:line '|is line a string?|]{algo:rpsccl}})
                          |<iterate for constructing reports>|
                          <reports>))))]

终于到了众望所归地@:desc{把步骤名对应的代码碎片一个个翻译成真实代码}环节。
但是@tech{草稿函数} @:id{rpsccl} 的大部分步骤都比昨天的简单、直白很多，
强行为每个步骤都配一个代码碎片就显得很多此一举了。
因此，@:thus{前文已经提过的简单直译的步骤名已经以@tech{注释}形式标在其对应代码的右边了}。
剩下的三个确实都比较有说法。

首先是@algo-ref[#:line '|initialize reports!|]{algo:rpsccl}，
定义和初始化报告列表 @:var{reports}，
但要注意把数学习题命名@tech{变量}的习惯改成更具@emph{可读性}的程序@tech{变量}名，
并补上类型信息：

@handbook-chunk[|<initialize reports!>|
                [reports : (Listof (Listof Natural)) null] (code:comment "空报告列表")]

@handbook-action{函数式条件表达式：if 语法}

昨天解谜时也碰到过条件表达式，
但我们一不留神就略过了，
毕竟就是 @tt{if} @emph{语法}嘛，都能看得懂。
可如果要从@tech{函数式编程}的角度看，
你觉得 @tt{if} @emph{语法}是什么？
是@tech{函数}吗？

理论上讲，@tech{函数式编程}中的 @tt{if} 语法是形如
@$$[#:tag "if_expr"]{if(\texttt{test-expr}, \texttt{then-expr}, \texttt{else-expr})}
的@tech{函数}，而@:err{非}@tech{指令式编程}中的条件@emph{语句}。@handbook-sidenote*{
 这个差别你现在可能看不出来，但可以先记在脑子里。
 }具体来说，它要求三个参数，且@focus{都不能省略}，
第一个参数 @:var{test-expr} 用作@emph{条件测试}，
当条件成立时，@${if} @tech{函数}被化简为第二个参数 @:var{then-expr} 并继续对其求@tech{值}；
当条件不成立时，@${if} @tech{函数}被化简为第三个参数 @:var{else-expr} 并继续对其求@tech{值}。
即：@$$=[#:tag "if_def"]{ ~\\
 if(@racket[#t], @:in{then-expr}, \texttt{else-expr}) \rightarrow @:in{then-expr} \\
 if(@racket[#f], \texttt{then-expr}, @:in{else-expr}) \rightarrow @:in{else-expr}}
通俗地说就是“@tt{if @:var{test-expr} then @:var{then-expr} else @:var{else-expr}}”。
这有两件特别有趣的事:

@handbook-itemlist[
 #:style 'compact

 @item{“@tt{if ... then ... else ...}”并@:err{不是}地道的英语句式，
  只是因其简洁、清晰的特点而成了各种现代程序语言的标准。
  但根据各语言语法习惯的不同，直接写全这三个单词的语言倒还真不多。
  Racket 遵循其@tech{函数}的 @tech{sexp} 写法，
  只需要写出其中的 @:stx:def{if} 即可。}
 
 @item{@${if} @tech{函数}定义式中的 @tt{-expr} 表明三个参数各自都只能是@emph{一条} @tech{sexp}，
  而@:err{不能}直接给出@emph{多条} @tech{sexps}。
  否则不就分不清哪个是 @:var{then-expr} 哪个是 @:var{else-expr} 了嘛？
  不过，必要时我们有多种方法可以把多条 @tech{sexps} 打包成一条 @tech{sexp}。}
 ]

以上是理论说法，可以把 @:stx:def{if} 语法解读成数学@tech{函数}，
但在具体的语言中，达成此目标的具体做法未必都是程序@tech{函数}。
在 Racket 中，至少写出来在形式上看不出差别，
更深入的知识今后再细聊。

回到任务中来，
@tech{伪代码}的“@:desc{若 @algoref[#:line '|is line a string?|]{algo:rpsccl}，
 则 @algoref[#:line (cons 4 8)]{algo:rpsccl}，
 否则 @algoref[#:line 10]{algo:rpsccl}}”翻译成代码即:
@racket[(if (string? line) |<iterate for constructing reports>| <reports>)]。
该 @:stx:def{if} 语法的参数按顺序依次是：

@handbook-itemlist[
 #:style 'ordered

 @item{@tt{test-expr}： 本例中雇佣了@tech{谓词函数} @:id{string?} 来@emph{测试}读到的行是否真是@tech{字符串}}
 @item{@tt{then-expr}： 本例中对应代码碎片 @racket[|<iterate for constructing reports>|]}
 @item{@tt{else-expr}： 本例中对应代码碎片 @racket[|<reports>|]}
 ]

这次我们先完成@emph{条件不成立}时要做的事：
@algo-ref{algo:rpsccl}结束了。

@handbook-chunk[<reports>
                reports]

啊…… 这么简单的吗？
昨天的@emph{辅助任务}一次得到两个结果@tech{值}，
我们用到了 @:id{values} 函数。
今天只有一个结果@tech{值}，
就这么光秃秃的放那多少显得怪怪的。

正如@fig-ref{flow:rpsccl}把 @:var{reports} 放在@emph{平行四边形}里，
借着形状共同表达“输出 @:var{reports}”一样，
@algo-ref[#:line 'reports]{algo:rpsccl} 作为一个步骤名不应该用名词，
这里实际上代表的是“@tt{evaluate to} @:var{reports}”的简略表达。
与之对应的代码碎片也只需包含@tech{变量} @:var{reports} 自己即可。

@tech[#:key "函数式编程"]{函数式语言}通常不需要专门的 @tt{return} 语句。
谁最后被化简到，谁的@tech{值}就是最终的结果@tech{值}。
计算机直接打交道的也只有@tech{值}，
程序代码指示语言如何把 @tech{sexp} 化简成@tech{值}再交给计算机处理。
有点绕，但点破了就不觉得奇怪了，习惯就好。

@handbook-action{列表操作}

好的，本例@emph{辅助任务}最后一步，也是最重要、最有趣的一步来了。
当@emph{条件成立}时，我们需要扩增报告列表，
扩增的实际过程对应着@algo-ref[#:line (cons 4 8)]{algo:rpsccl}。
很明显，@emph{扩列}实际要做的事不止一件，
作为 @:stx:def{if} 语法的一条 @tt{then-expr} 很可能写不下。

Racket 提供了多种方法可以@:desc{把多条 @tech{sexps} 打包成一条 @tech{sexp}}，
这里我选用大家已经熟悉的 @:stx:def{let} 语法来完成此目的，
因为在@emph{扩列}的准备工作中需要定义临时@tech{变量}：

@handbook-chunk[|<iterate for constructing reports>|
                (let ([s (string-split line)]) (code:comment @#,list{字面直译 @algoref[#:line '|split line|]{algo:rpsccl}})
                  (rpsccl (cons |<convert levels>| reports)))]

@aoc-complain{
你能看出来这段代码碎片的名字就是用英语写的“@tech{迭代}，用以构造(下一个)报告列表”吗？
类似昨天@emph{辅助任务}中的代码碎片“@racket[|<地址编号扩列，迭代下一轮>|]”。
如果看不出来，你需要额外花精力补英语了，
至少要熟记本书中出现的中英文术语。}

@handbook-scene{迭代式循环：for/list 语法}

除去对@tech{函数式编程}本身的理解难度，@emph{扩列}的核心难点是完成代码碎片 @racket[|<convert levels>|]，
将分裂所得的数字@tech{字符串}@emph{挨个}转化为真正的@emph{自然数}。
从 @algo-ref[#:line '|for levels|]{algo:rpsccl} 和 @algoref[#:line '|convert level|]{algo:rpsccl}
中我们看到了熟悉的句式“for @smaller{the} @:var{level list}, for each @:var{l} in list @:var{s} do @:var{converting}”。
尝试将它逐句翻译成代码：
@tamer-hidden-repl[(define s (string-split "7 6 4 2 1"))]
@racketblock[(for/list : (Listof (Option Complex)) ([l (in-list s)])
               (string->number l))]

有那么点意思了。
昨天解谜用到的 @:stx:def{for/sum} 语法用于累加，
其类型标记是 @:type{Natural}，
其@tech{循环体}也得同步产生一个 @:type{Natural} 类型的@tech{值}；
@:stx:def{for/list} 语法也是一类 @tt{for each} 循环，
用于产生@tech{列表}类型的@tech{值}。
因此，其类型标记是 @:type{(Listof T)}，
但@tech{循环体}只需产生@tech{列表}内@emph{项}的类型 @:type{T} 的@tech{值}。
这里的 @:type{T} 可以替换成任何具体的类型，本例中是 @:type{(Option Complex)}。
更直观一些的对比见@tab-ref{tbl:forslcmp}：

@tamer-table!['tbl:forslcmp
              "For Each 循环：for/sum vs. for/list"
              @tabular[
 #:sep @hspace[1]
 #:column-properties '(center)
 #:row-properties '((top-border bottom-border) bottom-border)
 (list (list @bold{语法变种} @bold{@tech{循环体}@tech{值}类型} @bold{最终@tech{值}类型} @bold{折叠操作符} @bold{折叠初始@tech{值}} @bold{最终@tech{值}操作符})
       (list @:stx:def{for/sum}
             @:type{Natural}
             @:type{(Listof Natural)}
             @:id{+}
             @racket[0]
             @:id{values})
       (list @:stx:def{for/list}
             @smaller{@:type{(Option Complex)}}
             @smaller{@smaller{@:type{(Listof (Option Complex))}}}
             @:id{cons}
             @racket[null]
             @:id{reverse}))]]

对于 @:stx:def{for/sum} 和 @:stx:def{for/list} 这两个 @tt{For Each} 循环的变种，
它们的共同点都是在@emph{做折叠}，
把一个@tech{列表}类型的@tech{值}折叠成另一个@tech{值}。
就像把一张纸折叠成一个千纸鹤一样……抽象。

你可以回忆看看，你用纸和笔做复杂的混合运算时，
是不是经常先把好算的结果写在草稿纸上？
并且，草稿纸上每多一个数字，
题目里(或草稿纸上)就多划掉一个数字，
直到整个算式都算完。
在这个例子里，
“折叠”体现在“划掉”这个动作上，
毕竟，真的折叠草稿纸来盖住用过的数字就太傻了。

@tab-ref{tbl:forslcmp}多了一个@emph{最终值操作符}，
这是因为 @:id{cons} 函数总是把@tech{循环体}产生的@tech{值}加在“上一个”最终@tech{值}的头部，
这导致最终的@tech{列表}@tech{值}总是原列表的倒序，
这件事多少有点违背常识，也会一不小心干扰到后续分析。
因此，@:stx:def{for/list} 会很贴心的雇用 @:id{reverse} 函数把最终@tech{值}反转回来。

@aoc-bonus{
 @tab-ref{tbl:forslcmp}看起来很像@tab-ref{tbl:rrcmp}。
 是的，它们相似的根本原因是，
 在@tech{函数式编程}中，
 @tt{for each} 这样的@tech{迭代}式循环本来就是@tech{递归函数}的语法糖。
 它们内部遵循着相同的@emph{数学操作原理}。
 建议@:thus{把此观点放在脑子里，闲暇时思考思考，没准就悟了呢}。
 到那时，你就入门了。
}

@handbook-scene{列表映射：map 函数}

@:stx:def{for/list} 语法使得我们不必编写大量重复代码来@:desc{按顺序取出@tech{列表}中的每一个@tech{值}}，
而只要关注@:desc{如何操作每一个取到的@tech{值}}。
这点我们已经达成共识。

但是，如果我说 @:stx:def{for/list} 语法还是太啰嗦了，你会怎么想？
比如:

@handbook-itemlist[
 #:style 'compact
 
 @item{@tech{子句}中的 @:id{in-list} 函数就是纯粹多余。
  谁不知道@tech{值}的来源是个@tech{列表}？}
  
 @item{@tech{循环体}所做的事情仅仅只是把@emph{取出的@tech{值}喂给另一个函数}，
  然后就结束了。不免让人觉得，给这些个@tech{值}取名也很多余不是？}
 ]

嗯，当我们对@tech{列表}要做的事同时符合上述两种简单情形，
确实可以把 @tt{for each} 循环替换成相应的@tech{高阶函数}来处理。
比如，单个@emph{自然数}@tech{列表}的累加运算，
我们可以直接把它扔给 @:id{apply} 函数。

@tamer-figure-margin['map.dia @list{@:id{map} 函数}]{@(geo-fit map.dia aoc-mparwidth 0.0)}

从功能上看，
本例使用 @:stx:def{for/list} 语法只做了一件再简单不过的事：
@emph{把@tech{列表}中的每个@tech{值}挨个喂给相同的@tech{函数}，
 并把所有结果@tech{值}再按@focus{相同的顺序}收集到新@tech{列表}中}。
这个操作听起来很实用，也不像会出幺蛾子的样子，对不对？
因此，在数学和@tech{函数式编程}中，如此这般的操作值得拥有专属名字，
叫做@handbook-deftech[#:origin "Map"]{映射}。

@aoc-bonus{
 @emph{映射}和 @tt{map} 这俩都是烂大街的名字，可能会给你造成混淆。
 比如，初等数学课上干脆就把@tech{函数}和@emph{映射}当作同义词；
 计算机科学某些领域的 @tt{map} 是一种数据结构；
 英语和地理课上的 @tt{map} 是地图。

 尽管各种解释的内在有相通之处，我仍然建议你在记忆时注意区分，
 @tech{函数式编程}中的@tech{映射}是一类定义明确的@tech{高阶函数}。
}

于是，当我们心中建立起该操作对应的数学概念后，
上述啰嗦的 @:stx:def{for/list} 语法就被精简成了三个单词：
@tamer-repl[(map string->number s)]
换句话说，
@:thus{@tt{For each} 循环符合人类的语言习惯，
 写出来更具@emph{可读性};
 @:id{map} 函数迫使你像数学家一样思考，
 对初学者可能不那么一目了然}。
此外，特别强调一下，
@focus{这两种方式都属于@tech{迭代}式循环，
 也都是同等地道的@tech{函数式编程}}。

本例中的 @:id{map} 函数接收两个参数，
一个@focus{一元}@tech{函数}，一个@tech{列表}，
结果@tech{值}是另一个@tech{列表}(@fig-ref{map.dia})。
你可以试着写写看它的@tech{类型签名}：

@racketblock[(-> (-> String (Option Complex)) (code:comment "参数1类型：字符串到可能数的转化")
                 (Listof String) (code:comment "参数2类型：字符串列表")
                 (Listof (Option Complex)))]

这个一元@tech{函数}没啥特殊要求，
只要能单独接收@tech{列表}中的@focus{每一个}@tech{值}即可。

@tamer-repl[(code:comment "将列表中的每个数 +1")
            (map add1 (list 1 2 7 8 9))
            (code:comment "判断列表中各个字符是否是空白字符")
            (map char-whitespace? (list #\space #\2 #\tab #\D #\newline))]

@handbook-scene{算法严谨性：缺失的一环}

有时候，编写@tech{函数式程序}的过程，就像是在接通管道或电路，
前一段的@tech{输出}@focus{类型}必须要跟后一段的@tech{输入}@focus{类型}适配，
否则@focus{类型系统}会粗暴地打断你的程序，
根本不给运行机会。

结合前文的分析，我们碰到了一个微妙的问题。先捋一捋：

@handbook-itemlist[
 #:style 'compact

 @item{代码碎片 @racket[|<convert levels>|] 应该产生一个类型为 @:type{(Listof Natural)} 的@tech{值}}
 @item{@:id{string->number} 函数只能保证把@tech{字符串}转化为 @:type{(Option Complex)} 类型的@tech{值}}
 @item{@emph{复合}了 @:id{string->number} 函数的 @:id{map} 函数只能保证结果@tech{值}的类型是 @:type{(Listof (Option Complex)}}
 ]
发现问题出在哪了吗？
如果从文字看不出来，可以看@fig-ref{pipeline.dia}找找灵感，
从 @:var{s} 到 @:var{Maybe Levels} 到 @:var{Just Levels}。
注意，此图的重点是@tech{变量}的@emph{类型}(而非@tech{值})流经@tech{函数}管道的变化。

@tamer-figure-margin['pipeline.dia @list{代码碎片 @racket[|<convert levels>|] @emph{类型}管道的微妙问题}]{@(geo-fit pipeline.dia aoc-mparwidth 0.0)}

也就是说，
@algo-ref[#:line '|for levels|]{algo:rpsccl} 和 @algoref[#:line '|convert level|]{algo:rpsccl} 的大白话描述缺了关键一环：
需要一个形如 @racket[(-> (Listof (Option Complex)) (Listof Natural))] 的@tech{函数}在代码碎片 @racket[|<convert levels>|]
内部把管道给接通了。

@aoc-bonus{
 昨天任务结束时，你不是疑惑过“类型到底干嘛了”？这就碰到了个活的例子。
 
 之所以我没把这关键一环放在@tech{伪代码}里，
 是因为这关乎@tech{算法}的@emph{严谨性}。
 @tech{伪代码}可以描述得很严谨，
 但代价是我们得亲自用纸和笔去验证，
 这听着就很无趣且浪费时间。
 因此，@:thus{如果语言的类型系统能提醒甚至代替我们搞定严谨性，
  我们就该放宽心把它交给语言来做}。

 在没有类型系统(或类型系统只是摆设)的语言里，你可以不做这一步，
 但代价是你很可能会被“程序一运行就崩溃”搞崩溃。
 实际上，从谜题提供的文件里，我们一眼就能看出来，
 @:id{string->number} 函数一定会给我们@emph{自然数}结果@tech{值}。
 可万一有人修改了谜题文件了呢？
 或者，你测试程序时自己笔误了呢？
 就像这样：

 @tamer-repl[(define strs (string-split "9 -7i 6< 2 1"))
             (map string->number strs)]

 看，程序虽能运行，但出现奇怪的结果了吧？
 虽然@emph{复数} @racket[0-7i] 和@tech{布尔}@tech{值} @racket[#false] 都是 Racket 靠谱的@tech{值}，
 但它俩都无法参与@emph{比大小}，
 继续运行做@emph{主线任务}你就会看到程序崩溃了。
}

@handbook-scene{列表过滤：filter 函数}

好的，问题已经明确，
找到一个@tech{函数}将 @racket[(Listof (Option Complex))] 类型的@tech{输入}@tech{值}转化为
@racket[(Listof Natural)] 类型的@tech{输出}@tech{值}。
这有两个细节比较有意思：

@handbook-itemlist[
 #:style 'ordered
 @item{@emph{转化}这个动作是对@tech{列表}而言，又是从@tech{列表}到@tech{列表}的操作。}
 @item{@tech{输出}类型 @:type{Natural} 是@tech{输入}类型 @:type{Complex} 的子类型。
  换句话说，对于@tech{列表}内的每个@tech{值}，
  实际上根本不涉及@emph{转化}，而只是在做@emph{筛选}。
  即使 @:type{False} 跟 @:type{Natural} 没有直接关系，
  @:type{(Option Complex)} 也只是增大了@emph{筛选}的范围，
  对结果没有影响。}
]

第2点觉得眼熟吗？昨天我们已经做过类似的任务了。
@emph{数数}用的@tech{高阶函数} @:id{count}，
其核心是不是也是在做@emph{筛选}？
只不过它关心的是@tech{列表}中满足要求的@tech{值}的数量，
而不关心具体是哪些@tech{值}符合要求。

在@tech{函数式编程}中，
@emph{把@tech{列表}中符合要求的@tech{值}筛选出来，并搜集到新@tech{列表}中}，
这样的操作称为@handbook-deftech[#:origin "Filter"]{过滤}。
而@emph{筛选}的过程，
就是@emph{挨个把@tech{列表}中的@tech{值}喂给@tech{谓词函数}提问}的过程。
于是，你应该可以写出 @:id{filter} 函数在本例中的@tech{类型签名}了：

@racketblock[(-> (-> Any Boolean) (code:comment "参数1类型：过滤用谓词函数")
                 (Listof (Option Complex)) (code:comment "参数2类型：可能数列表")
                 (Listof Natural))]

@tamer-figure-margin['filter.dia @list{@:id{filter} 函数}]{@(geo-fit filter.dia aoc-mparwidth 0.0)}

至此，@fig-ref{pipeline.dia} 里缺失的那环就齐了：

@handbook-chunk[|<convert levels>|
                (filter exact-nonnegative-integer? (map string->number s))]

如过不是最常用的话，@:id{map} 和 @:id{filter} 是编程中极其常见的操作。
值得我们多关注一下它俩的联系和区别：

@tamer-repl[(code:comment "判断列表中各个自然数是否是偶数")
            (map even? (list 9 7 6 2 1))
            (code:comment "过滤出列表中的所有偶数")
            (filter even? (list 9 7 6 2 1))]

@aoc-bonus{
 虽然前文特别解释了类型系统的必要性，但我这里还是要强调一下：
 @focus{类型系统只能在语言层面保证算法的严谨，是很难在任务逻辑层面保证算法严谨的}。
 本例中，我们通过 @:id{filter} 函数让类型系统很开心，
 但它实际上只是剔除了错误的数据，
 进而导致读到的@tech{列表}@tech{长度}缩短，
 仍然有可能导致后续任务出错。
}

@handbook-scene{测试辅助任务}

@emph{辅助任务}完成，用例题数据测试一下：

@handbook-sidenote{@(tamer-filebox (aoc-tamer-path "iSoH/02_rnr.aex") #:path-centerized? #true)}

@tamer-repl[#:requires ["../aoc.rkt"]
            ($ read-reports #:< "iSoH/02_rnr.aex")]

可以看到，读取到的@emph{报告列表}仍然是倒序。
不过这不影响@emph{主线任务}，
也就没有必要专门反转它了。
如果你不明白为什么读到的列表是倒序，
或者不明白报告列表的构造过程，
请重新阅读@Secref{sec:rloop}并在纸上模拟它执行的过程，
该列的表格、该画的示意图都做起来。

@handbook-scenario{主线任务1：count-safe-reports 函数}

@handbook-chunk[|<Puzzle 1: Count Safe Reports>|
                (define count-safe-reports : (-> Input-Port Natural)
                  (lambda [rptin]
                    2222))]

@handbook-scenario[#:tag "sec:read"]{超神的 read 函数}

@handbook-scenario{闭幕}

继昨天初识的@emph{自然数}、@tech{数列}、@tech{函数}这几个典型的数学类型之后，
@:term{运算}可操作的@tech{值}的类型于本章又扩增了两个：
我们首次将语文范畴的@tech{字符}和@tech{字符串}也吸纳为计算机的基本数据类型。

从现在起，请将这段话刻在脑子里：@handbook-sidenote*{
 你猜，哪一科老师最喜欢强调@emph{标点符号}的用法。
 }@focus{在程序语言中，标点符号比文字重要}。
错别字、用错词都不影响你写程序，
但错了的标点符号会让计算机困惑。
因此，@:thus{在初学阶段，
 当你碰到莫名其妙的错误，
 或者程序结果不符合预期时，
 你首先应该考虑你的程序有没有忠实地反映你的意图，
 有没有不小心因为符号笔误而稀里糊涂地“鸡同鸭讲”。}
然后才考虑@tech{算法}是否有漏洞。

@aoc-complain{
 想到 Python 了没？连空格都敢来找事的奇葩！
 觉得委屈可以问候它的作者，憋坏了自己可不值当。
 有追求的少年哟，当从@emph{对流行的东西祛魅}开始。
}

冒险越来越深入了……

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
