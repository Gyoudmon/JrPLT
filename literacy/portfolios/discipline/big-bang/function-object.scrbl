#lang scribble/base

@require{../../literacy.rkt}

@handbook-portfolio-story{函数与对象}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered

 @item{理解函数和对象。}
 
 @item{理解变量和常量。}
 
 @item{理解类型和值。}
 
 @item{了解函数在数学和编程中的异同。}
 
]

@handbook-scenario{知识点}

@handbook-action{重点知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Variable"]{变量}即可以被改变的量。
  在计算机中它指的是@focus{内存地址}，用来提示“这段内存中的内容可以修改”。

  @tech{变量}可类比成储物柜的抽屉(所在的@tech{位置})。
  但需要注意的是，@tech{变量}即是储物柜抽屉(所在的@tech{位置})本身，
  “变量名”才是贴在抽屉上的“标签”。}

 @item{@handbook-defterm[#:origin "Constant"]{常量}即是不可被改变的量，
  它也可以像@tech{变量}一样拥有名字。}

 @item{@tech{变量}(和@tech{常量})有两个属性：@tech{类型}和@tech{值}。
  @handbook-defterm[#:origin "Value"]{值}指的是变量的@focus{内容}；
  @handbook-defterm[#:origin "Type"]{类型}则用于提示“@focus{如何解读}这个内容”。}
 
 @item{字面上看，@handbook-defterm[#:origin "Function"]{函数}即“功能”，表示计算机要做的事。
  
  计算机做事遵循@handbook-defterm[#:origin "Input-Process-Output" #:abbr "IPO"]{输入-处理-输出}模式。
  具体到每一个@tech{函数}，它负责@emph{处理}一定的@emph{输入}数据，
  完事后也很可能会产生一定的@emph{输出}数据。
  在这个过程中，所有的数据都以@tech{变量}或@tech{常量}的形式存在。}
]

@handbook-action{难点知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Class"]{类}即自定义@tech{类型}，
  它封装了一系列相关联的@tech{变量}和@tech{函数}。
  
  @tech{类}的@tech{变量}称为@handbook-defterm[#:origin "Field"]{字段}，通常代表@tech{类}的属性；
  @tech{类}的@tech{函数}称为@handbook-defterm[#:origin "Method"]{方法}，通常代表@tech{类}的动作或行为。}

 @item{@tech{类}可以比作设计图，根据此设计图创建出的产品
  称为该@tech{类}的@handbook-defterm[#:origin "Object"]{对象}。
  每个@tech{对象}都是自己@tech{类}型的@handbook-defterm[#:origin "Instance"]{实例}。}

 @item{@tech{类}的@handbook-defterm[#:origin "Instantiation"]{实例化}过程本质上
  是调用了该@tech{类}的@handbook-defterm[#:origin "Constructor"]{构造函数}，
  用以完成对@tech{对象}的初始化工作。因此@tech{实例化}的语法与@tech{函数}调用相似。}
 
]

@handbook-scenario{学科交叉}

单看@tech{函数}这个名字，它确实不太好理解。
因此，建议初学者不要过分纠结“它叫什么”，
而把关注点放在具体情境中“它是什么”上。

@tech{函数}在不同场景有不同的理解，
以下几个视角在编程中比较典型。
而且在真实的软件中，
这几个视角往往不会独立存在，
在理解时请注意融会贯通。

@handbook-action{语言学}

@tech{函数}是动宾结构或动补结构。

在给@tech{函数}取名时会直接使用动词(动作的名称)作为函数的名字。
及物动词必须要带一个宾语才不算“语法错误”，
与之对等的@tech{函数}也必须带有一个@tech{变量}用来存放它操作的数据，
但该@tech{变量}的名字往往无法事先确定，
通常会选用一个具有“参考”价值的词，
这样的@tech{变量}也因此被称为@handbook-defterm[#:origin "Parameter"]{参数}。

动词可以没有宾语，可以有双宾语，宾语也可以带补语，一个动宾结构可以写得很长。
与之对等的@tech{函数}的@tech{参数}列表数量可能不唯一，也可能为零。
一般来讲，程序语言对@tech{函数}的多个参数的出现顺序并无特别要求，
但出于方便记忆的考量，
@tech{函数}设计者会根据自己熟悉的语法习惯来编排参数顺序；
@tech{函数}使用者也可以根据作者的习惯来记忆参数顺序。

@handbook-action{数学}

@tech{函数}一词最正统的来源就是数学。
这也是为什么我们明明知道它难以理解，
却还是只能这么叫它的原因。

@handbook-defterm[#:origin "Mathematical Function"]{数学函数}描述的是@tech{变量}之间的变化关系。
即，一些@tech{变量}发生变化之后，另一些@tech{变量}也会根据一定的运算规则跟着变化。
前者称作@handbook-deftech[#:origin "Independent Variable"]{自变量}，
后者称作@handbook-deftech[#:origin "Dependent Variable"]{因变量}。
对于一个具体的@tech{数学函数}而言，
@tech{自变量}的数量不唯一，可以有多个，也可以没有；
@tech{因变量}的数量有且只有一个，
且@focus{当@tech{自变量}的取值确定时，
 @tech{因变量}的值也确定且唯一。}

@tech{数学函数}可以直接翻译成程序@tech{函数}。
此时，函数名通常选用名词(按上述语言学视角的理解就是省略了“计算”俩字)；
而计算得到的@tech{因变量}的值称为@handbook-defterm[#:origin "Return Value"]{返回值}。

程序@tech{函数}和@tech{数学函数}有些细微差别。
比如，@tech{数学函数}更严格，
程序@tech{函数}的相同输入@tech{参数}可以得到不同的@tech{返回值}。
初学阶段可以不用考虑这么多，
仅以此提醒诸位@racketerror{切不可因为在编程中函数写得贼溜就轻视数学课上的函数主题}。
@idea{入门之后，可以考虑系统学习下@emph{函数式编程}，
 这里的这个@emph{函数}就是严格意义上的@tech{数学函数}}。

@handbook-action[#:tag "FO&SE"]{科学与工程}

在知识密集型实践中提到的@tech{函数}通常也是指@tech{数学函数}。
反过来说，@tech{数学函数}实在不接地气，
我们需要一个例子把它变得通熟易懂。

@tech{数学函数}的本质是从@tech{自变量}到@tech{因变量}的@handbook-defterm[#:origin "Map"]{映射}。
在实验设计中，我们经常采用“控制变量法”来探索它们的变化规律。
于是，@tech{自变量}就是被实验人员操作和控制的变量；
@tech{因变量}则是在实验过程中根据不同@tech{自变量}多次尝试观察到的不同现象。

比如，
在想像中，设计师可以通过艺术灵感说“某广场缺少一个多大的圆锥体，一点都不美”。
在实践中，工程师没法凭空制造出这样一个恰好满足要求的圆锥体来，
至少要探索圆锥的底面半径和高这两个@tech{参数}来探究最终成品的效果。
于是，工程师打开了3D建模软件，
通过@emph{分别}调整底面半径和高这两个@tech{自变量}来观察最终作品的效果，
顺便估算制造成本。

这里没有软件工程师什么事吗？
3D建模软件也是一个软件，要由具备相关知识的软件工程师来写。
软件工程师不一定直接解决问题，但他们要考虑更多细节，
以教会计算机做个趁手且专业的工具。
换句话说，学习编程的训练目标之一，
就是@focus{事无巨细地深挖现象背后的本质，
把“你觉得”转化为别人可以照着做的一系列动作(@tech{函数})}。

@handbook-reference[]
