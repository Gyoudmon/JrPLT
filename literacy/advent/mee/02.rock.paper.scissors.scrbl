#lang scribble/lp2

@(require "../literacy.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@aoc-task[2022 2]{猜拳大赛}

@aoc-desc[#:keywords ["类型" "符号类型" "枚举类型" "模式识别"]
          #:edition [三 "2024-06-22"]]

猜拳游戏是一种起源于中国，
后经日本标准化并风靡全球的简单、快捷、和平的终结分歧的游戏。
作为一种产生随机结果的手段，其效果跟抛硬币、掷骰子差不多，
多见于小孩为主要参与者的非正式场合，
但也确实存在不少真实世界的应用案例。
比如在某些体育赛事中用于决定首发队伍。以及：

@story*{
 @story{精灵们开始在沙滩上搭帐篷。
  为了让自己的帐篷能够尽可能的离食物仓库近一些，
  一场超大型的猜拳大赛正在如火如荼地进行着。}

 @story{猜拳是一种双人对决类游戏，一场比赛下来双方要来上很多轮。
  每一轮，两位选手同时决定在石头、布、剪刀三种手型里任选其一，
  然后依据如下简单规则决出胜负：
  石头砸烂剪刀、剪刀碎掉布、布以柔克刚包裹石头；
  而如果双方手型一致，则算平局。}
 
 @story{得分最高的人将获得比赛优胜，
  总得分即是每一轮得分的总和。
  每一轮得分又分为两部分：手型分（出石头得 @tamer-datum['shape-score 'rock] 分，
  出布得 @tamer-datum['shape-score 'paper] 分，
  出剪刀得 @tamer-datum['shape-score 'scissor] 分）和结局分（输了不得分，
  平局得 @tamer-datum['outcome-score 'draw] 分，
  赢了就能得到 @tamer-datum['outcome-score 'win] 分）。}
}

@handbook-scenario{类型与集合}

本书无时无刻不在提“@tech{类型}”这个词，
还稀里糊涂用它搞定了昨天的任务，
似乎也没碰到特别不好理解的地方。
在日常生活中，
@tech{类型}这个词的出镜率可能没有那么高，
但它事实上已经内化在我们的母语中了。比如，

@itemlist[
 @item{按奇偶性，自然数可分为@:type{奇数}和@:type{偶数}两大@litchar{类}；}

 @item{@:type{苹果}和@:type{梨}是两@litchar{种}不同的水果；
  我喜欢@:type{苹果}胜于@:type{梨}，
  但是刚刚吃的@:id{苹果}没有@:type{梨}好吃。}
 
 @item{当一个家里有猫的人说“我喜欢猫”时，她脑海里浮现的是@:id{自己家的猫}？还是@:type{猫}这@litchar{种}动物？}
 ]

可见，人脑能无缝切换@:type{类型名}和@:id{某类型中的某个独特个体}。

@tamer-figure!['ntower "数系和数塔" (aoc-image "number.png" #:scale 0.32)]

严肃地说，
@focus{@handbook-deftech[#:origin "Type"]{类型}是基于集合(Set)的数学概念，
各种具体的@tech{类型}不外乎是给数学中的不同类@:term{集合}起了个更直观的@:term{名字}}。
@:thus{当一个变量被标记为某种@tech{类型}时，
 那这个变量的@:term{值域}即为对应的@:term{集合}，
 可以被初始化为该@:term{集合}中的任意一个元素
 }（只要它在计算机能表示的数值范围内）。比如：

@tamer-repl[(code:comment @#,elem{@:type{Natural} 指的是@:term{自然数}集合 @${\mathbb N = \{ 0, 1, 2, 3, \ldots \}}})
            (code:comment @#,elem{且它包含的最大@:term{自然数}不受计算机体系结构的限制})
            (define n : Natural #x123456789ABCDEF0FEDCBA987654321)
            n
            (code:comment @#,elem{@:type{String} 指全体@:term{字符串}的集合})
            (define str : String "atom")
            str
            (code:comment @#,elem{@:type{Symbol} 指全体@:term{名字}的集合})
            (define sym : Symbol 'atom)
            sym]

@handbook-scenario{符号·枚举类型}

猜拳游戏是一种起源于中国，
后经日本标准化并风靡全球的简单、快捷、和平的终结分歧的游戏。
作为一种产生随机结果的手段，其效果跟抛硬币、掷骰子差不多，
多见于小孩为主要参与者的非正式场合，
但也确实存在不少真实世界的应用案例。
比如在某些体育赛事中用于决定首发队伍。以及：

@story*{
 @story{为答谢你昨天的帮助，
  有一只小精灵愿意给你一份加密策略，声称一定能帮你获得优胜。
  “左边那一列代表对手出的手型， A 指代石头，B 指代布，C 指代剪刀。右边那列……”
  不巧，这个时候别的精灵喊她过去帮忙搭帐篷了，
  她也就真的撇下你去帮忙了。}

 @story{没办法，你只好自己猜了，
  第二列肯定指我自己要出的手型:
  X 指石头，Y 指布，Z 指剪刀。
  考虑到每次都赢对手就太可疑了，
  这份策略一定是精心设计过的。}

 @story{请根据精灵给你的加密策略计算@question{按照你的理解最后能得到的总分}。}
}

鉴于今天的任务自带一系列真·解谜游戏属性。
因此，相对于昨天的任务，
本章的起点碎片除了包含两个主线任务碎片外还包含游戏规则碎片：

@handbook-chunk[<roshambo:*>
                (module advent typed/racket
                  <定义比赛规则>
                  <定义策略解密函数>
                  <定义策略规则函数>
                  <主线任务:求解谜题1>
                  <主线任务:求解谜题2>)]

@;(tamer-filebox (aoc-tamer-path "mee/02_rps.aex"))

本任务蕴含着一条隐藏信息，猜拳大赛的比赛规则实际上有两个维度组成：
@focus{猜拳游戏自身的规则}和@focus{猜拳大赛的计分规则}。
因此，碎片 @racket[<定义比赛规则>] 也要细分为两个碎片：

@handbook-chunk[<定义比赛规则>
                <定义猜拳游戏规则>
                #;<定义大赛计分规则>]

对于猜拳游戏，在不同地方虽然叫法可能不同，
但无论是选手可出的手型，还是每轮比赛的结局，都只有有限的几个@tech{值}，
这样的一组@tech{值}通常会被定义成@handbook-deftech[#:origin "Enumerated Type" #:abbr "enum"]{枚举类型}。
在 Racket 中，@tech{枚举类型}的@tech{值}就直接用@tech{符号类型}的@tech{值}表示。

这句话相当拗口，不着急，一步步来，从回顾“@tech{值}”这个概念开始：

@tamer-repl[(code:comment "这是一个 String 类型的值")
            "atom"
            (code:comment "这是一个 Symbol 类型的值")
            'atom
            (code:comment "这是一个名为 atom 的变量")
            (eval:alts atom (eval:result "" "" "undefined identifier: atom"))]

从本例中我们至少能读出以下信息：

@itemlist[
 #:style 'ordered
 
 @item{@tech{值}是不能再化简的表达式。}
  
 @item{@:type{Symbol} 的@tech{值}在语法上跟在符号@:delim{'}(键盘输入用@:delim{单引号})后面。}

 @item{@:type{Symbol} 的@tech{值}看起来跟变量名很像。}
]

第3条是点睛之笔。
@:thus{@handbook-deftech[#:origin "Symbol Type"]{符号类型}@handbook-footnote{在有些语言里，
  @tech{符号类型}也被称为原子类型(Atom Type)。
  }存在的意义就是为了让你能直接写出代表@:term{名字}的@tech{值}}，
而又不必非得把这个@:term{名字}定义成变量。
例子中的@:err{红色错误信息}说的是“未定义标识符 atom”，
之所以会有此错误信息，是因为我们确实没有定义它。
在没有@tech{符号类型}的语言里，
相同目的可以用@tech{字符串类型}代劳。

注意，例子中 @racket['atom] 的@racketoutput{类型信息}显示的不是 @:type{Symbol}，
而是它自己。这说明，@:thus{所有符号@tech{值}既属于 @:type{Symbol} 类型，
 它们各自也都专属于它们自己代表的@handbook-deftech[#:origin "Singleton Type"]{单例类型}。
}@tech{符号类型}的这个性质是它可以用于表示@tech{枚举类型}的基础。

在日常生活中，
石头(@racket['rock])、布(@racket['paper])、剪刀(@racket['scissor])
不过是万千名词中平平无奇的三个，扔词堆里就再也找不出来的那种。
在猜拳游戏中，
如果把它仨当作一般 @:type{Symbol} 类型的@tech{值}来处理，
那我们就没有任何手段限制玩家只出这三种手型了。
这有个正在耍无赖的玩家，
他的手型伸出了三根手指、另外两根握拳，
这怎么算？

用自定义@tech{枚举类型}可破。

@handbook-chunk[<定义猜拳游戏的手型和结局类型>
                (define-type RPS:Shape (U 'rock 'paper 'scissor))
                (define-type RPS:Output (U 'win 'draw 'lose))]

如此一来，我们就有机会限定玩家只能出规定的手型(结局类型同理)：

@tamer-repl[(code:comment "定义一个 RPS:Shape 的变量，并初始化为 'rock，可行")
            (define shape : RPS:Shape 'rock)
            (code:comment "定义一个 RPS:Output 的变量，并初始化为 'tie，失败")
            (eval:alts (define outcome : RPS:Output 'tie)
                       (eval:result "" "" "Type Checker: type mismatch"))]

看，失败原因是@:err{类型不匹配（type mismatch），
 期待一个类型为 @:type{RPS:Output} 的值，结果喂给它的却是 @racket['tie]}。

@:term{集合}作为一种数学对象有它们自己的运算，
跟@tech{类型}关系最密切的是@:term{并集}，其次是@:term{交集}。
我们给猜拳大赛定义的两个@tech{类型}均为名词集合的真子集： @$${
 \b{Shape} & = \{ \mathit{rock}, \mathit{paper}, \mathit{scissor} \} \\
 \b{Outcome} & =  \{ \mathit{win}, \mathit{draw}, \mathit{lose} \}
}

@handbook-chunk[<定义猜拳游戏规则>
                <定义猜拳游戏的手型和结局类型>
                
                (define (shape-score [shape : Symbol]) : Byte
                  (case shape
                    [(rock)    1]
                    [(paper)   2]
                    [(scissor) 3]
                    [else      0]))

                
                (define (outcome-score [outcome : Symbol]) : Byte
                  (case outcome
                    [(win)  6]
                    [(draw) 3]
                    [(lose) 0]
                    [else   0]))

                
                (define (round-score [sf:play : Symbol] [outcome : Symbol]) : Index
                  (+ (shape-score sf:play)
                     (outcome-score outcome)))

                
                (define (round-end [op:play : Symbol] [sf:play : Symbol]) : Symbol
                  (cond [(eq? sf:play op:play)  'draw]
                        [(eq? sf:play 'rock)    (if (eq? op:play 'scissor) 'win 'lose)]
                        [(eq? sf:play 'paper)   (if (eq? op:play 'rock) 'win 'lose)]
                        [(eq? sf:play 'scissor) (if (eq? op:play 'paper) 'win 'lose)]
                        [else 'false]))]

其实，你并不真的确定那只精灵是真想帮你，还是另有所图。不管怎么说，即使想自己找到
一个靠谱的策略，先试着跟着这个策略走也未尝不可，起码能有个比较。于是，从上面那个例子
可以得到如下信息：

@itemlist[

 @item{第一轮，对方出石头（A），你出布（Y）。你获胜，因而得到 @racket[8] 分，其中
  @tamer-datum['shape-score 'paper] 分因为手型布，
  @tamer-datum['outcome-score 'win] 分因为结局赢；}

 @item{第二轮，对方出布（B），你出石头（X）。你输了，因而得到 @racket[1] 分，其中
  @tamer-datum['shape-score 'rock] 分因为手型石头，
  @tamer-datum['outcome-score 'lose] 分因为结局输；}
 
 @item{第三轮，两人都出了剪刀，平局，因而得到 @racket[6] 分，即
  手型剪刀 @tamer-datum['shape-score 'scissor]
  分加上平局分 @tamer-datum['outcome-score 'draw] 分。}
                                       
 @item{你的最终得分是 @racket[15] （@racket[8] @racket[+] @racket[1] @racket[+] @racket[6]）。}

 ]

于是，抱着试试看的心态，我们需要一个函数 @racketid[simulate-with-guessed-strategy]
来计算@question{严格跟着那份自己理解的加密策略走能得到多高的分}：

@handbook-chunk[<主线任务:求解谜题1>
                (define simulate-with-guessed-strategy : (-> Input-Port Natural)
                  (lambda [/dev/datin]
                    <读取-模拟-累加-循环>))]

回忆昨天的思路，今天的谜题是一个@racketcommentfont{读取-模拟-累加}循环（@racketidfont{rsal}, read-simulate-accumulate-loop），
即逐行读取策略指导，每读到一行，就按照该轮策略模拟比赛，然后将结果累加。直到没有更多内容为止，函数返回总得分。

@handbook-chunk[<读取-模拟-累加-循环>
                (let rsal : Natural ([total : Natural 0])
                  (define line (read-line /dev/datin))
                  (if (string? line)
                      (let ([op:ch (string-ref line 0)]
                            [sf:ch (string-ref line 2)])
                        (rsal (+ total <模拟本轮比赛>)))
                      total))]

本谜题的总体结构比昨天的简单，但细节是魔鬼。头脑风暴一下，模拟比赛的过程似乎还挺繁琐，
要先解密策略文件：

@handbook-chunk[<定义策略解密函数>
                (define (char->shape [ch : Char]) : Symbol
                  (case ch
                    [(#\A #\X) 'rock]
                    [(#\B #\Y) 'paper]
                    [(#\C #\Z) 'scissor]
                    [else 'false]))]

一般来说，诚意十足的精灵给的策略文件不会出现乱七八糟的输入，因此可以放心直接读取。
但是在真实的软件中，一定要检查用户输入是否符合契约。这里，当出现其他字符时，
我们简单返回 @racket[#false]，告诉 @racketid[rsal] 可以跳过这一行。
这样就能正确模拟比赛、得到该轮得分了：

@handbook-chunk[<模拟本轮比赛>
                (let ([op:play (char->shape op:ch)]
                      [sf:play (char->shape sf:ch)])
                  (if (and (symbol? op:play)
                           (symbol? sf:play))
                      (let ([outcome (round-end op:play sf:play)])
                        <本轮比赛得分>)
                      0))]

注意，别忘了每轮得分都由两部分构成：手型分和结局分：

@handbook-chunk[<本轮比赛得分>
                (round-score sf:play outcome)]

接下来就可以通过传入策略文件来模拟了：

@tamer-repl[#:requires ["../aoc.rkt"]
            ($ simulate-with-guessed-strategy #:< "mee/02_rps.aoc")]

那只精灵帮其他精灵搭完帐篷后又溜出来找你了，继续之前的规则解读：“右边那一列决定了
你策略上的输赢， X 指输掉比赛，Y 指跟对手打平，Z 指你必须赢。总之，祝你好运。”

这么一解释，这文件倒是确实更像个机密策略了。比如，按照前面的例子，这次可以得到如下信息：

@itemlist[

 @item{第一轮，对方出石头（A），你需要平局（Y），也只能出石头。因而得到 @racket[4]
  分（@tamer-datum['shape-score 'rock] @racket[+] @tamer-datum['outcome-score 'draw]）；}

 @item{第二轮，对方出布（B），你出石头以策略上输掉（X）比赛，因而得到 @racket[1]
  分（@tamer-datum['shape-score 'rock] @racket[+] @tamer-datum['outcome-score 'lose]）；}
 
 @item{第三轮，你需要出石头以赢得（Z）对手的剪刀（C），最后得到 @racket[7]
  分（@tamer-datum['shape-score 'rock] @racket[+] @tamer-datum['outcome-score 'win]）。}
                                       
 @item{你的最终得分是 @racket[12] （@racket[4] @racket[+] @racket[1] @racket[+] @racket[7]）。}

 ]

于是，我们先来补全真正的策略规则函数:

@handbook-chunk[<定义策略规则函数>
                (define (char->outcome [ch : Char]) : Symbol
                  (case ch
                    [(#\X) 'lose]
                    [(#\Y) 'draw]
                    [(#\Z) 'win]
                    [else 'false]))

                (define (smart-shape op:play outcome)
                  (cond [(eq? outcome 'draw)    op:play]
                        [(eq? op:play 'rock)    (if (eq? outcome 'win) 'paper 'scissor)]
                        [(eq? op:play 'paper)   (if (eq? outcome 'win) 'scissor 'rock)]
                        [(eq? op:play 'scissor) (if (eq? outcome 'win) 'rock 'paper)]
                        [else #false]))]

然后是真策略函数 @racketid[simulate-with-designed-strategy]，
以计算@question{严格跟着那份精灵的加密策略走能得到多高的分}：

@handbook-chunk[<主线任务:求解谜题2>
                (define simulate-with-designed-strategy : (-> Input-Port Natural)
                  (lambda [/dev/datin]
                    (let rsal : Natural ([total : Natural 0])
                      (define line (read-line /dev/datin))
                      (if (string? line)
                          (let ([op:ch (string-ref line 0)]
                                [sf:ch (string-ref line 2)])
                            (rsal (+ total <模拟策略>)))
                          total))))]

这两个主函数大同小异，唯一的不同则关乎于每轮得分的构成。
对于你自己猜测的策略，其本质是@question{已知手型求结局}；
而精灵策略的本意则是@question{已知结局求手型}。

@handbook-chunk[<模拟策略>
                (let* ([op:play (char->shape op:ch)]
                       [outcome (char->outcome sf:ch)]
                       [sf:play (smart-shape op:play outcome)])
                  (if (and (symbol? op:play)
                           (symbol? sf:play))
                      <本轮比赛得分>
                      0))]

至此，任务完成：

@tamer-repl[($ simulate-with-designed-strategy #:< "mee/02_rps.aoc")]

不过，貌似就本任务中的输入数据来说，这个机密策略还不如自己瞎猜的那个。

@handbook-scenario{扩展思考}

虽然今天的任务完成了，
但是你有没有好奇过，
精灵们是如何执行她们写下的比赛策略的呢？

表面上看，猜拳是一个真·随机过程，
在这种情况下，不存在行之有效的必胜策略。
但是，人类玩家并不是真·随机数发生器，
下一轮出什么手型的影响因素很多。比如：
性格特征、心理因素、本轮结局、对手的干扰等等。
有经验的玩家可以在多次对决中逐渐摸索出对手的出手规律，
从而预判对手下一次的出手可能。

实际上，确实有不少社会科学类研究员专门研究过猜拳中的博弈策略，
并揭示出了一些有趣的结论。
比如人类的行为模式普遍存在持续的周期性；
某些对手会基于本轮结局按不同的出手循序决定下一轮出手手型。
也有人根据这些结论设计出了不少猜拳机器人，
其中最无耻的一类策略是利用高速摄像头捕捉对手的手型，
然后在毫秒级延迟中给出必杀手型。

更严肃一点的研究可以是，
将猜拳游戏中@:term{互相克制的三种手型}放到@:term{演化动力学}模型中去探索动态博弈策略。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
