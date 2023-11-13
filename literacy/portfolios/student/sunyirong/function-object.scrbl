#lang scribble/report

@require{../../literacy.rkt}

@(require "../../discipline/big-bang/lesson/function-object.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{函数与对象}

@lesson-desc[
 #:topic ['big-bang 'function-object]
 #:class 'lambda-girl
]

@handbook-lesson{踢猫效应}

@period-desc[
 #:goals function-object-goals 
 #:datetime ["2023-10-15" "13:30" "15:00"]]

上周的试听课和本周第一节正式课，
孙艺溶多次提到，“以前的C++课学了跟没学似的”。
这多半不是自谦的说法，
而是真的“学了、但又没留下印象”。
比如，她记得第一行要 include 某个东西，
点“构建”按钮就能编译运行程序了。
但是不知道程序的入口是 main 函数，
也不能用自己的话解释@tech{变量}是什么。

这不是学生的问题，
而且大学生、成年人中的初学者也一样是这个状态。
就现状而言，中小学编程教育没有特别可靠的教学实施。
我本人目前的研究目标就是要部分解决这个问题，
从最近几个月的实践效果来看，方向大致靠谱。
比如，我的课从不提那个众所周知的“Hello, World!”，
因为这个梗连我自己都没有代入感。

大多数人都会强调编程和数学的关系，
我的编程教育方法也确实主要源自数学教育。
但编程天然就是多学科的硬核综合，它不仅仅是数学。
@focus{它首先是语言、是写作，
 然后数学和其他学科才知道自己被安插在程序中的位置}。
所以，我的课入门就从@idea{教会计算机还原一个贴近生活的简单情境}开始，
相对于那个没有实际意义的“Hello, World!”，
这个开局能让学生感同身受：@focus{我真的教会计算机做了一件有意义的事}。

孙艺溶最近两次课对编程的代入感都源自她感觉到了“这段代码真的在做事”。
第一次是试听课时用 shell 命令创建和删除文件夹；
第二次是在“踢猫效应”的情境程序中代入了具体的名字。

所以，这是一个很好的开端。

本次课没有作业。
但我建议她多花点时间熟悉键盘。
C++ 代码里奇怪的符号太多，对初学者是个极大的考验。
而课堂学思维和知识的时间有限，
不应该浪费在纯技术问题上。

@handbook-lesson{C++ 类}

@period-desc[
 #:goals function-object-goals 
 #:datetime ["2023-10-29" "13:30" "14:30"]]

今天这节课少了30分钟，因此节奏比较赶，跳过了复习环节。
下周开始上课时间是周日下午13:00，请勿迟到。

今天这节课前30分钟是在输入代码，
孙艺溶输入速度没有问题，
但是编码习惯很差。
因此，这个过程规范了编码风格，
至少要让眼睛看起来舒服一些。

今天这节课同时也暴露了一些问题，
孙艺溶听不懂时会不吭声，或者心不在焉，
但是问起来又能复述出来。
这个复述肯定没有进入长期记忆，
属于一下课就忘的那种。
因此，我希望家长能一起参与进来合理规划课后学习计划，
详细说明请参考附录的@seclink["parent-how-to"]{《家长如何帮助孩子学编程》}。

还有一个小问题，
孙艺溶不知道自己电脑的登陆密码。
今天解释代码环节，她长时间看投影，
电脑进入屏保后就登陆不进去了。

@handbook-lesson{C++ 文件结构}

@period-desc[
 #:goals function-object-goals 
 #:datetime ["2023-11-05" "13:00" "14:30"]]

本节课的主要任务是通过逐步迭代的方式来认识 C++ 源码文件的基本结构。
期间涉及语法层面编码规范的简单说明，
包括：缩紧、标点符号两边的空格、@litchar{@string{@#\{}} 左边的空格等。
都是为了提高可读性，让眼睛舒服些。

@tech{函数}的定义和调用是两个过程，
识别函数调用的秘诀是看语法形式。
标点符号比文字重要。

@tech{类}的@tech{实例化}是难点，
需先形成意识，
然后自己在日常生活中寻找可以类比的地方。
比如，学会了一种解题思路之后，
下次再碰到同样、仅参数变了的题目，
套用那个思路在求解，
就是一个(比较抽象)的例子。

孙艺溶今天的状态比上节课好多了。

@handbook-lesson{数学函数}

@period-desc[
 #:goals function-object-goals 
 #:datetime ["2023-11-12" "13:00" "14:30"]]

由于下周要期中考试，
这节课本来没打算学新内容，
就以放松的形式复习、巩固之前学到的概念。
主题是，像个导演一样编排舞台剧，
并用中文或英文把剧本写下来。
目的还是从语言翻译的角度带学生理解@tech{类}和@tech{对象}。
结果她俩都不愿意动笔，
那个男生我知道，他就是抗拒写作。
孙艺溶应该是还没进入状态，
不知道编程和编剧竟然还有联系。

他们五分钟没动静，
我才临时决定做实验的。
孙艺溶动手能力还可以，基本没有误差，
但是又卡在“这个数学模型其实就是函数”上了。
具体地说，她知道这个实验不难，心里是有点不屑的，
我盯了好几次她才老实填表和画关系图像(之前就是随手一画)。
学习新知识(函数)肯定要先从学生已有的知识(正比关系图像)出发。
所以，其实，
本节课涉及的数学@tech{函数}其实就是孙艺溶已经知道的知识换了个名字和语法。

她这个反应在(浮躁的)学霸身上比较常见，
但在这个尚未入门的阶段，
也很难一下子就把他们心态掰过来。

下节课我还是打算从“编舞台剧”的角度逼着他们写作、翻译，
这个坎不克服了，后面会很麻烦。

@handbook-reference[]
