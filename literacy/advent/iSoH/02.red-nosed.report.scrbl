#lang scribble/lp2

@(require "../literacy.rkt")

@require{../../share/diagram/aoc/2024/02.report.rkt}
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

@aoc-desc[#:keywords ["字符" "字符串" "联合"]
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

@handbook-chunk[|<.read-location-IDs: type signature>|
                (-> Input-Port (Values (Listof Natural)
                                       (Listof Natural)))]

于是，对于前两个问题。
本例@:desc{读取文件}的目的可类推为@:desc{@emph{构造}@litchar{一组}报告@tech{列表}}，
可得其@tech{类型签名}为 @racket[(-> Input-Port (Listof Natural))]……

确定吗？谜题里说了，@emph{报告}的内容也是个@tech{数列}。
因此，正确的@tech{类型签名}应该是

@handbook-chunk[|<read-reports: type signature>|
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
 
 @item{如何判断是否读到了行尾？}
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
}那干脆就把人类书写出来的“文字符号”全都当成一类东西就行了。
于是，@emph{单个的}字母、数字、汉字、标点符号、空格……
它们都叫做@handbook-deftech[#:origin "Character" #:abbr "Char"]{字符}；
@emph{一个或多个字符按书写顺序串联在一起}就成了@handbook-deftech[#:origin "String"]{字符串}。

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
至于 @tech{eof}，它并不作为@tech{字符}出现在文件里，
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

@tamer-figure!['read-comparison @list{重复六次读操作： @:id{read} vs. @:id{read-char} vs. @:id{read-line}}]{
 @(let ([sep (geo-hline aoc-linewidth 8.0 #:stroke 'LightGrey)])
    (geo-vl-append #:gapsize 4.0
                   (geo-fit read.dia aoc-linewidth 0.0)
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

类型标记中的 @litchar{U} 表示类型的@handbook-deftech[#:origin "Union"]{联合}。
即，@emph{@tech{联合}类型的@tech{值}可以是参与@tech{联合}的所有类型中任意一个的@tech{值}}。
通俗地说就是“……或……或……”类型。
于是，@handbook-itemlist[
 #:style 'compact
 @item{@:id{read-char}
  函数求值结果的类型可以是 @:type{Char} 或 @:type{EOF}；@handbook-sidenote*{
   @:type{(U Char EOF)}}}
 @item{@:id{read-line}
  函数求值结果的类型可以是 @:type{String} 或 @:type{EOF}。@handbook-sidenote*{
   @:type{(U String EOF)}}}
 @item{类型 @:type{Any} 特殊在，
  它可以看成是所有类型的@tech{联合}，
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
因此，@:id{string->number} 函数的求值结果也是个@tech{联合}类型：@handbook-sidenote*{
   @:type{(U Complex False)}@linebreak[]
  如果对 @:type{Complex} 有疑问，
  可回顾昨天提到的@tech{数系}。
 }@emph{要么}是 @:type{Complex} @emph{要么}是 @:type{False}。
通俗地说就是，存在失败可能，代表失败的是@tech{布尔型}的@tech{值} @racket[#false]。

@handbook-scenario{算法设计}

回顾昨天的解谜过程，
在分析阶段我们日常生活的词汇就已经足够用了，
因此，有一个“大白话描述”环节。
而今天的解谜任务，
我们在一个个引导性提问中逐渐认识了@tech{字符}和@tech{字符串}这两个新概念(和与之配套的操作)，
进而丰富了我们描述问题的语料库。
因此，现在可以直接给出完成@emph{辅助任务}的@tech{伪代码}：

@algo-pseudocode[
 #:tag 'algo:rpscl "Read Reports"
 @list['|initialize reports|!]{@emph{令} @${R} = @racket[null]，代表@focus{初始}的空报告列表}
 @list['|read line|]{尝试从文件当前位置@:in{读取}一行，@emph{设}为@${L}}
 @list['|string|?]{@tt{if}@hspace[1]@:pn{@${L}确实是一行字符串},@hspace[1]@tt{then}}
 @list['|split line|]{@hspace[4]@emph{令} @${ s = L }分裂所得的强度等级串列表}
 @list['|for each level|]{@hspace[4]为收集报告 @${r}，@emph{对于} @${s} 中的每一个强度等级串 @${l}， @emph{执行}}
 @list['|convert level|]{@hspace[8]将字符串 @${l} 转化为数字}
 @list['|cons reports|]{@hspace[4]@emph{令} @focus{新}@${R = cons(r, R)}}
 @list['loop]{@hspace[4]@emph{回到}@algo-goto['|read line|]重复执行}
 @list['No]{@tt{else} @:cmt{; 此时的 @${R} 即是完整的报告列表(的倒序)}}
 @list['done]{@hspace[4]@:cmt{; 告知结果}}
]

比之昨天的@tech{算法}描述，@algo-ref{algo:rpscl}有了以下两点变化：

@handbook-itemlist[
 #:style 'compact
 @item{大多数@emph{步骤名}都写得更具体了，即使只看@emph{步骤名}也能准确脑补出该步的具体任务。}
 @item{步骤描述部分混合了字母符号和大白话，我觉得这样对初学者更友好一些。@handbook-sidenote*{
   实际上，对于“读文件”这样有副作用的@tech{算法}，
   步骤里本来就充满了很多看上去跟数学毫无关系的部分，
   写出来就跟代码差不多了。
   我们不需要这么多“重复”描述，哈哈哈哈哈哈……}}
 ]

@tamer-figure['flow:rpscl
              @list{@algo-ref{algo:rpscl} 流程图，后接@fig-ref{flow:puzzle1}或@fig-ref{flow:puzzle2}}
              @(tamer-delayed-figure-apply #:values geo-scale #:post-argv (list 0.50)
                                           make-rnr-helper.dia 'read-comparison 'split.dia)]

@handbook-action{读取-判断-扩列 循环}

所有的@tech{算法}也都应该有个名字，
而@:term{循环}是绝大多数程序的典型特征。
因此，不妨把@algo-ref{algo:rpscl}称作@:term{读取-判断-扩列 循环}，
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

@algo-ref[#:line 'initialization!]{algo:rpscl}没有强调类型，
在这补上：

@handbook-chunk[<init!>
                [A.IDs : (Listof Natural) null] (code:comment "甲组精灵写下的地址编号列表")
                [B.IDs : (Listof Natural) null] (code:comment "乙组精灵写下的地址编号列表")]

本例中的两个@tech{变量}的类型均为@:term{自然数列表}(@:type{(Listof Natural)})，
初始值@:term{空列表}写作 @:val{null}。

此外，本段代码碎片将@tech{变量} @${X}和@${Y}重命名成了 @:var{A.IDs} 和 @:var{B.IDs}。

@algo-ref[#:line '|read IDs|]{algo:rpscl}就是字面直译，
@:desc{对清单文件 @:var{rptin} 执行 @racket[read] 操作，给结果起名为 @${a}；
 重复一次，给结果起名为 @${b}}：

@handbook-chunk[|<read reports>|
                (define a : Any (read rptin))
                (define b : Any (read rptin))]

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
于是，@algo-ref[#:line 'predicate?]{algo:rpscl} 就是对结果提问：@:desc{
 @${a}是自然数吗？@${b}是自然数吗？}
并且要同时回答 @racket[#true](@emph{是})才算条件达成(@fig-ref{predicate.dia})：

@handbook-chunk[<string-line?>
                (and (exact-nonnegative-integer? a)  (code:comment "a 是自然数吗？")
                     (exact-nonnegative-integer? b)) (code:comment "b 是自然数吗？")]

@tamer-figure-margin['string-line.dia @algoref[#:line '|predicate?|]{algo:rpscl}]{
 @(geo-fit read-char.dia aoc-mparwidth 0.0)}

按照正常说话顺序，“@:desc{若 @algoref[#:line 'predicate?]{algo:rpscl}，
 则 @algoref[#:line (cons 4 6)]{algo:rpscl}，
 否则 @algoref[#:line (cons 7 8)]{algo:rpscl}}”，
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
@algo-ref{algo:rpscl}就直接结束了：

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
                (define count-safe-reports : (-> Input-Port Natural)
                  (lambda [rptin]
                    2222))]

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
@algo-ref{algo:rpscl}本来也是有名字的，
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

@handbook-scenario[#:tag "sec:read"]{超神的 read 函数}

@focus{@tech{值}从@emph{信息}中来、到@emph{信息}中去。
}正如我们人类能够从“读到”或“听到”的文字段落中自动提取有用的信息一样，
Racket 的 @:id{read} 函数比你预期的要强大和前卫很多。
它首先跳过所有的空格@tech{字符}，
从第一个有墨迹的@tech{字符}开始，
直接读出@emph{单个}但完整的@tech{值}。

@handbook-scenario{闭幕}

继第一章初识的@emph{自然数}、@tech{数列}、@tech{函数}这几个典型的数学类型之后，
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
