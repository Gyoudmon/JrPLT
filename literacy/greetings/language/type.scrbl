#lang scribble/book

@(require "../../literacy.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{自然语言与程序语言}

毋庸置疑，当提到语文和英语时，我们知道它们都是语言类课程。
具体来说，语文代表母语，英语代表外语，
学习它们可以帮助我们更好地表达自己、与他人沟通；
同时母语承载着本民族的文化、外语承载着各自民族的文化。
于是，像汉语、英语这样，
随着人们交流的需要而自然而然形成和发展的语言
称为@handbook-defterm[#:origin "Natural Language"]{自然语言}。

@tech{自然语言}强调的是@emph{“人与人”}之间的交流和@emph{“自然而然”}的形成。
言下之意，非自然形成的语言必然不是自然语言；
非人类生物如果有交流也不能说它们的语言是自然语言。
与之相对的，人们出于特定的目的而有意创造出来的语言统称
为@handbook-defterm[#:origin "Constructed Language" #:abbr "conlang"]{人造语言}。

@handbook-defterm[#:origin "Progamming Language"]{程序语言
 }@margin-note*{从英语构词的角度讲，
 Progamming 是 Language 的修饰语，而且是名词性修饰语，
 意为“编写程序这一动作或过程”。
 “编程”和“程序设计”是同义词，但中文语感上看，
 前者作动词讲更自然一些，后者则更贴近名词。
 因此，我个人更喜欢将它译作“程序语言”，
 即“程序设计语言”的简称。
 }是一系列@emph{驱动机器做事}的@tech{人造语言}。
喏，这里并不强调“与机器的(双向)交流”，
而是强调一种使役关系：我叫你做什么你做就完了！
即使某些软件的措辞时常让你感到宾至如归，
那也是软件的作者要求这些软件“你得让用户觉得他们才是大爷”，
属实是演技派社牛。而用@tech{程序语言}“说”的话即
是@handbook-defterm[#:origin "Program"]{程序}，
它们以@handbook-defterm[#:origin "Source Code"]{源代码}的形式存在。
即，书写程序的“文字”称作“代码”。

那么，@tech{程序语言}中的“语言”跟@tech{自然语言}中的“语言”是一个意思吗？
当然是，要不然就不会叫语言了。
实际上，@tech{程序语言}产生之初的确不叫语言。
从1840年代算起，一百多年间，它们最主要的身份是数学记号(Notation),
就如同@${a + b}，@${(x, y)}之类的符号一样。
直到1950年代，“语言”才正式成为早起有影响力的@tech{程序语言}名字的一部分，
比如这个叫做@emph{算法语言}的ALGOL，即是
@litchar{ALGO}rithmic @litchar{L}anguage 的缩写。

而从“记号”到“语言”，这个过程并非改个名字这么简单，
尤其困难的是让社会其他人接纳这个称作“语言”的东西确实名副其实。
这其实也是好多学术前辈努力争取来的，
其中包括 Ada LoveLace，英国女性数学家，
被最多人认可的“历史上第一个写程序的人”。

@handbook-scenario{双语学习新选项？}

先正面回答，为什么@tech{程序语言}和@tech{自然语言}中的“语言”是一回事？

2014年，神经科学家通过@handbook-defterm[#:origin "fMRI"]{
 功能性核磁共振成像}技术首次证实了，
会写代码的人在阅读代码时激活的脑区与
人们理解@tech{自然语言}时所激活的脑区完全一致。
此结论后又于2017年被再次证实，
阅读代码更像阅读散文@handbook-footnote{
 此处的散文更侧重于“专业性较强的自然语言文本”。}，
越熟练这两种场景的可区分度就越小。

@margin-note*{@mlink{命名}是件相当考验语言功底的事。
 在编程过程中，我们需要不断地给各种概念、事物命名，
 因而学编程至少能提高英语遣词造句的能力。
}具体来说，对于工作记忆、单词提取、@mlink{物体命名}、
规划与决策等等一系列与语言有关的活动，
我们大脑的语言中枢并不区分语言的类型，
对@tech{自然语言}、@tech{程序语言}一视同仁。
而且，此结论同样适用于其他类型的语言。
比如，手语、音乐等等。

另一方面，现代教育已经无需再讨论是单语教育还是双语教育了，
因为小学低年级就已经开始学外语。这既是事实，
也因为双语教育确实好处多多，
包括听力、记忆力、专注力、创造力、
批判性思维、多任务处理、问题求解、
认知弹性等多个维度。
而且，这些认知优势的提升与年龄无关，
@fallacy{新研究不再认同“只有孩子才值得学外语”这一广为流传的谬论}。

于是，问题就转化为了选择什么样的双语教育：
@emph{母语+外国语言}还是@emph{母语+程序语言}？
美国法学界近年来不断讨论推进，
是否允许高中生提交@tech{程序语言}替代外国语作为毕业凭证？
大多数州的考虑还是，@tech{程序语言}不适合与人正常交流，
也容易误导学生外语不重要从而导致无法就业，不行；
仅有 Texas 允许学生提交@tech{程序语言}，
如果实在学不会外语的话。

@margin-note*{本节在写作风格上，
 语气严肃、措辞晦涩，
 是因为这部分内容更适合给教育工作者看。
 然而，现实的矛盾在于，
 学生作为学习的主体，
 理应比任何人都更需要知道“如何高效学习”。
 现在你先跟着本书@mlink{刻意练习}，
 回头再看时会豁然开朗。
 更长远地说，
 即使你将来不想当老师，
 也总会有自己的孩子，
 让他们自信地感叹“还是咱的家长靠谱”岂不很酷？}

总之，我们或许正处在某个关键的十字路口。
而且，我们尚不清楚为何人们学编程时会如此折磨，
但总可以尝试将编程教育编织成可以连接其他课程的课程。
技术上说，程序语言@handbook-footnote{
 此处的程序语言有别于信息技术和其他计算机科学分支，
 它特指“程序语言理论”，是纯粹数学、逻辑学和语言学的交叉学科，
 有博士学位。}本身确实比其他课程更适合达成此目标，
同时还能涵盖学校应该教却没有教的方面：
元认知、思维图式、@mlink{刻意练习}，
以及抗挫能力@handbook-footnote{
 初学编程第零难不来自于编程本身，
 而是准备好各种编程软件，
 有时这个过程真的挺让人气馁。}。

@handbook-scenario{自然语言 vs. 程序语言}

有人可能会质疑，@tech{fMRI}只能证明所有类型的语言共享同一个语言中枢，
不能充分证明语言中枢对所有语言的处理机制都相同。
没错，这个质疑合情合理，但无关紧要。
实际上，我们仅靠直觉也能猜到，
莫说@tech{自然语言}与@tech{程序语言}与音乐有不同，
就是同为@tech{自然语言}的汉语和日语也有诸多差异；
同为音乐的摇滚和民谣也不尽相同；
而英语的语调事实上与爵士乐异曲同工。
事物之间的联系扑朔迷离，
不同但能相互影响才是我们学习和研究的动力。

@handbook-action{规模}

规模即大小，此处也可理解为学习一门语言所需投入的精力多少。

相对于其他类型的语言，@tech{自然语言}可谓是怪兽级庞然大物，
至少数以万计的词汇、海量自由组合的语法规则、
还有灵活多变的语用习惯、变幻莫测的修辞手法、
更有特色方言和文化，
没个几年功夫还真拿不下。
@tech{程序语言}就语言本身而言要娇小得多，
视@tech{程序语言}的不同，
少至几个多至几十个关键字、
少至几条多至小几百条语法规则。

在本书中，你会接触到的一个专业但非主流的语言是 Racket，
其前身 Scheme 总共就只有5个关键字和8条语法规则。
你可以在此极简内核之上创造整个软件世界，
包括另一个@tech{程序语言}，比如 Python、Java等。
嗯？停，好像混进了什么奇怪的东西。
在一种@tech{程序语言}中创造另一种@tech{程序语言}，
这是个什么操作？
@focus{@tech{程序语言}的本质无非就是另一个@tech{程序语言}写出来的程序}，
目的是让我们用更熟悉或更方便的方式指挥机器干活。
而在@tech{自然语言}里就没有这种说法，
要怎么理解“在汉语里创造英语”？
理解不了就对了，因为真的很荒谬。

务实点说，
你在选择外语时，英语、日语、俄语，还是其他，必须慎重考虑，
因为一旦选定，它很大程度上就决定了你将来的事业、同事和客户。
@margin-note*{
 尤其当你的编程启蒙老师就是我的时候，你会更快领悟这一点。
 因为我不是单纯地教你学习某种具体的程序语言，
 而是教你如何应对层出不穷的“新”语言。}
但是选择@tech{程序语言}则没有这样的顾虑，
多学一门@tech{程序语言}要比多学一门外语容易很多。
接下来这句话请务必牢记在心：
@focus{计算机科学院从来不用语言划分专业，这跟外语学院有根本性不同。}
实际上，@emph{靠谱}的计算机系科班毕业生不挑@tech{程序语言}，
至少大多数人叫得出名字的语言他们随便换。
不过，具体到每一个人，他们都有自己喜欢和不喜欢的语言，
有些人并不会主动去用他们不喜欢的语言。

其实，所有的语言，在它们刚刚诞生之时都很小，
之后，它们的种子会发芽、会成长，只要时间足够，
人们终会赋予他们喜欢的语言无限的表达力。

顺带一提，即使你想多学几门外语，
也应该先掌握一门再学其他，这样所需的总时间更少。
此建议也适用于学习多门@tech{程序语言}，稍后再详细讨论。

@handbook-action{感官}

语言的感官指的是人与人交流时语言信息的传递途径。
以@tech{自然语言}为例，它可细分为好多类型，以下三种最具代表性：

@itemlist[
 #:style 'compact
 @item{@handbook-defterm[#:origin "Spoken Language"]{口语}是听觉主导：你说话、我听见；}
 @item{@handbook-defterm[#:origin "Sign Language"]{手语}@handbook-footnote{手语确实也算@tech{自然语言}。}是视觉主导：你比划、我看见；}
 @item{@handbook-defterm[#:origin "Written Language"]{书面语}也是视觉主导：你写下、我看见。}]

一般来说，像英语、日语这样的用户比较多
的@tech{自然语言}的发展规律是先有@tech{口语}再有对应的@tech{书面语}，
因而学的时候要从听、说、读、写四个维度分别发力；
用户群小的语言就很难说了，它们有可能只有语音没有文字。
这里特别说一下，日语之所以会有那么多汉字，
就是因为日语一开始没有文字，所有的交流都是口耳相传，
直到公元八世纪或更早(?-794年)才跟中国人学了写字。
@margin-note*{用“母语注音法”学外语真的是
 世界人民喜闻乐见又简单粗暴的通行法则。}
而且，最初日本人并@emph{不是}用汉字来“@emph{写}”日语，
而是用汉字来“@emph{读}”日语，
跟我们刚学外语时喜欢用汉字给英语注音简直一摸一样。

@tech{程序语言}也是视觉主导型语言，
跟@tech{书面语}相似，读代码的方式是用眼睛看，
差别在于写代码时一般不用纸和笔@margin-note*{
 有些公司可能会在面试时让你在纸上手写代码；
 或者，中学不允许带电脑去学校，我那时就是在纸上练习的。
 }，而是用键盘。
特别地，Scratch 这样的图形化@tech{程序语言}主要用鼠标
拖拽的方式写代码；
将来出现@tech{手语}型@tech{程序语言}也完全不奇怪。

嚯，终于在正式学编程之前逮着个插入代码的机会，开心！
下面两个“@sign{尖括号(>)}”后面跟着的就是示例代码，
写作语言是 Racket；效果你肯定已经注意到了，
那个醒目的箭头就是这两段代码@mlink{画}出来的。

仔细观察这两段代码，并尽可能找出它与我们平时写的文字的差别。
任何角度都行，有一个算一个，哪怕是“代码竟然有颜色”！

@margin-note{@para{第一次看到“活”的代码，
 你多惊奇都属正常。
 但请相信我这里说的每一句话都是字面意思。
 比如，那个箭头真的是这两段代码自己@mlink{画}出来的。}
 @linebreak[]
 @para{这本书里那么多代码(和运行效果)，
  每一个都“@emph{运行-复制-粘贴-配色}”岂不是很业余？
  总之，这是个只有学会编程才能解锁的特别的写作技巧，
  现在感觉到“泰裤辣”就对味了。}}

@tamer-repl[
 (require bitmap)
 (bitmap-polygon #:fill 'burlywood #:border 'green
                 #:window -1-i
                 '(0.0 -8+16i 32.0 -8-16i))]

这段代码里出现了很多英文单词、数字和奇怪的符号，
这些确实是代码给人的第一印象。
既然出现了英文单词，而且占比最多，那这段代码是英语吗？
如果你的答案是“否”，那恭喜，你的符号思维很不错；
如果你觉得“是”，那也没关系，不怪你。
毕竟，大多数有影响力的@tech{程序语言}的作者都是英语母语人士，
他们当然会@focus{优先向自己的母语借词汇用了}，
不过他们中的大多数也都约好了似的@focus{选择摒弃英语原有的语法规则}。
这个话题我们稍后再细聊。

这有一件有趣的事，
不过可能得等你学会编程后才能做。
在你自己的圈子里找一个编程高手，
然后拿一段她最熟悉的语言的代码，
大声地朗读给她听。
你猜她要多久才会想起来抽你？
“闭嘴，给我自己看”。
那啥，别误会！她真不是针对你，
也绝对不是因为你发音不准，
而是确实听不懂。
因为@tech{程序语言}不仅仅是视觉主导型语言，
而且是@emph{只能}用视觉来理解的语言。

可是，视觉主导和听觉主导，这个差别真的很重要吗？
很重要，也很关键。
往大了说，它会影响你学习的方式，在本书后续部分我们再细聊；
@margin-note*{比如，
 学@tech{程序语言}完全不应该起早贪黑“朗读代码”。
 不会真有人这么想过吧，不会吧不会吧？
 记数学公式同理。}往小了说，
你注意到代码里那些奇怪的标点符号了吗？
比如@code{()}、@code{#:}等。
语言在交流时有大量信息很难甚至无法通过词汇和句法来传达。
比如，@tech{口语}中我们可以通过改变嗓音、
使用微表情和肢体动作来表达情绪或其他感情色彩；
@tech{书面语}要达到同样的目的就要困难很多。
常见的补救方式有：

@margin-note{
 前面我夸下海口说，要帮你高效学习新语言。
 喏，要达到这个目的，最好的方法是@focus{
  像语言设计师一样思考问题}，
 而不能仅仅满足于“会用”。
 “会用”也太简单了，
 不必非得我来写书。}

@itemlist[
 #:style 'compact
 @item{给语言添加语气助词、拟声词，甚至颜文字；}
 @item{给语言添加标点符号，并详细规定每种标点符号的用法；}
 @item{给语言的文字添加附加符号，这些符号写出来就像是给普通文字戴了个装饰品。}
]

前两点都不难理解，第三点是什么情况？
举个例子，英国人喜欢给外来单词加锐音符。
比如源自法语的café（咖啡馆或类似的轻食餐厅）
和源自日语的Pokémon（宝可梦），
它们的e都戴了个小帽子，
用以表示“这个e不是英语，请还原它本源的发音”。

音乐这种纯听觉语言，
它也有着所有@tech{口语}都有的致命缺陷：
音乐虽美，却稍纵即逝。
于是，人们发明了一种又一种记谱法。
简谱还算通俗易懂，
五线谱所用的符号在外行看来就真是离了个大谱。

@tech{程序语言}没有@tech{口语}的问题，
且在不引起歧义的前提下，合理使用符号可以少写很多字，
连“@tech{书面语}要写很多字”的缺点也给一并解决了。
可以说，决定@tech{程序语言}语法的其实正是那些奇怪的符号。
@margin-note*{你猜，
 哪一科的老师最喜欢强调标点符号的用法？
}但@tech{程序语言}比@tech{自然语言}更看中语法的正确性，
甚至死板到了不能出错的程度。
那么，如此严格又是何苦呢？
既是为了让机器能按你的要求做正确的事；
也是为了让别人能看明白你的程序在干嘛，
这个“别人”也包括不久(比如两周、两个月)之后的你自己。

@focus{好的程序应该要方便人阅读，顺便也能在机器上运行}，
不知道你会怎么理解这个“顺便”。
按理说，@tech{程序语言}是人类在给机器下达命令，
似乎不关其他人什么事。
事实上也是，这个世界上有无数的程序正在运行，
它们共同构成了信息时代的软件基础。
但是，如果不考虑其他人，软件的作者会老去、会死去，
他们的程序就真的再没有人能理解了。
当新机器出现替代老机器、但新机器不再兼容旧代码时，
那些无人能理解的程序就只能抛弃掉，
那些程序所做的事也只能再找冤大头想办法重写。
所以，“方便与人交流”
既是@emph{使用}@tech{程序语言}的实践原则，
也是@emph{设计}@tech{程序语言}的指导原则。

@handbook-action{演化}

。

@handbook-scenario{先有程序语言还是先有计算机器？}

“@tech{程序语言}”和“计算机器”的关系没有“先有鸡还是先有蛋”的困惑。
有定论，@tech{程序语言}比真正意义上的计算机早出现一个多世纪。
最早的还没有被称为语言的@tech{程序语言}发明者是英国数学家和计算机先驱
Charles Babbage，他设想了一个称作“分析机”的通用机械式计算机，
用数学家更熟悉的符号@handbook-footnote{
 这个数学符号其实就是现代数学家称之为“形式语言”的东西。
 形式语言是更一般的语言类别，它既包括数学记号，
 也包括@tech{程序语言}。Babbage 的符号系统很像现在的汇编语言。
 }来编程。
不过，这个分析机直到现在都没有被真的制造出来。

Ada Lovelace 也是 Babbage 的分析社团的核心贡献者，
他们尝试让社会明白的事不仅仅关乎“记号和语言”，
而是更重要的认知理念：@idea{语言的书写符号并不等价，
 有些符号比另一些符号更便于人脑来理解}。
就他们当时的经历，他们让现在的微积分教科书统一采用了
Leibniz 的记号，而不是 Newton 的。
不过，这里我们换一个更本土化的例子来解释：对于加法运算，
“@${101 + 101 = 202}”和“@emph{壹佰〇壹加壹佰〇壹等于贰佰〇贰}”
这两种写法，你更喜欢哪一个？
这个问题也能帮你思考，为什么存在如此多的@tech{程序语言}？
而它们中的很多，似乎又仅仅只是语法不同。

从以上故事里，我们还能初步得到一个关于数学和计算机科学的模糊印象：
数学存在于纸上(和大神的脑子里)，不接地气；
计算机科学则是可以通过特殊机器来执行的数学。
像中考、高考这样的重要考试，答题卡是要先被阅卷机器扫描的。
到这里你是不是会想，等学会了编程就可以在答题卡上写一段程序，
让阅卷机器直接判满分？
嘿嘿嘿嘿，你解锁了一个很关键的思想。
不过现在先不着急，在本书后续部分我们会多次审视这个设想。

总之，我们人类是语言性生物，
@tech{程序语言}则是千百年来人类创造的最神奇的东西，没有之一。
我们现在正处在信息时代中，
而这个时代的一切数字化生存都建立在@tech{程序语言}编织的软件之上。
在本书中，“语言”一词可以是以下任意一种或多种隐喻：
你我交流的媒介；
需要习得的知识、掌握的方法、做事的工具；
甚至是一种魔法。

@handbook-reference[]
