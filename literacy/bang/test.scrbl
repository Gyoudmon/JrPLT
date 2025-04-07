#lang scribble/manual

@require{literacy.rkt}
@require{diagram/spec.rkt}

@(require geofun/vector)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{系统测试与调试}
@handbook-word-count[]

一般来说，
游戏引擎是一类复杂、易错的软件系统，
主要体现在数学含量高，对时间敏感。
因此，为保证软件的质量及降低开发难度，
游戏引擎开发团队往往会开发一整套测试和调试工具自用@$cite{GEA}，
用于教育的本引擎也不例外。

@handbook-scenario{单元测试与行为驱动开发(specmon)}
@handbook-word-count[]

测试在软件开发中的主要作用是发现缺陷、验证功能，以确保软件质量。
针对软件系统的不同层级有不同的测试工作，
单元测试的目标是最小可测试单元，
通常包括函数、类、模块等，
它们通常具有明确定义的规范。

在本引擎中，
最小可测单元主要包括数学对象和物理对象、
通信协议中的数据类型和校验码。
这些测试单元大多都有若干成熟实现，
包括我本人也在其他软件项目中多次重写过，
已经经过真实世界的检验。
因此，虽然本引擎的核心代码使用 C++ 编写，
但对引擎的测试方案并不依赖任何 C++ 测试框架，
而是选用了我自己编写的测试系统 @:sym{specmon}。
作为本引擎的一个外围工具，
它在传统竞赛刷题课中也被用作测评系统。

@tamer-figure!['spec.dia "specmon: 行为驱动开发支持系统" #:sub-sep @hspace[1]]{
 @(let ([s 0.32])
    (list @(hspace 4)
          @(para (geo-scale spec.dia s) "结构模块简图")
          @(hspace 8)
          @(para (geo-scale bdd.dia s) "简易工作流")))
}

@:sym{specmon} 使用 Typed Racket 编写，
设计初衷是为了支持我自己的行为驱动开发(Behavior-Driven Development, BDD)工作流。
对照@fig-ref{spec.dia}，简单介绍几个关键模块。

@handbook-itemlist[
 #:style 'compact

 @item{Behavior: 核心模块，定义了行为规范的基本数据结构和围绕它们的标准操作。
  @handbook-itemlist[
 #:style 'compact
 @item{Expectation：该模块定义了若干测试原语。
    其中包括两个特殊原语分别用于测试日志消息和子进程的标准输出。}
 @item{Issue: 该模块定义了问题类型。比如 错误、异常、待办、不支持等。}
 @item{Formatter: 该模块用于微调错误报告的输出格式。
    比如，以十六进制或二进制格式输出某些数值类型。
    这在报告数据编码相关错误时尤其有用。}
 ]}

 @item{DSL: 该模块定义了书写行为规范的领域专用语言。}
 @item{Prover: 该模块负责运行行为规范并生成测试报告。}
 ]

BDD 系测试框架比较讲究“行为规范即是可执行的测试代码”，
而 @:sym{specmon} 无需配置，且不损失 Racket 原有功能。
因此，行为规范和正常的 Racket 代码可以混合使用，
包括使用外部函数接口(foreign function interface，FFI)或创建子进程。

关于本引擎的开发工作流可参考@Secref{bdd:flow:ex}。

@handbook-scenario{界面测试与可用性测试}
@handbook-word-count[]

界面测试和可用性测试关注的都是以用户为中心的交互设计功能。
前者偏技术，用以确保界面元素正确、布局合理、响应及时、能支持复杂的用户设备，等等；
后者偏体验，用以确保系统易学易用不易错、行为符合用户预期、确实能高效帮助用户完成任务，等等。

@handbook-action{界面测试}
@handbook-word-count[]

具体到本引擎的开发中，
界面测试的主要方法是手动测试，
因为教师既是开发人员也是用户，
所有的项目都必须先自己熟悉才能完成教学任务。

此外，本引擎自带一个类似课程项目软件的测试用引擎项目软件(名为 Tamer)，
里面的每个项目都是对特定界面机能的试运行。
比如，
@fig-ref*{lru.exe} 中用随机生成的正多边形代表游戏中可能出现的场景布局，
当用户需要选中堆叠在一起的某个游戏物体时，
本引擎会依次选择指针输入设备作用范围内的最近最久未被选中的物体。
被选中物体的边界框会被高亮显示，
并自动成为当前被聚焦的物体。

@tamer-figure!['lru.exe "手动测试用于“鼠标单击选择”的最近最久未选中算法(LRU)"]{
 @(let ([s 0.132])
    (list @(para (stone-image "Bang/LayerOrder1.png" #:scale s) "选中了顶层形状")
          @(para (stone-image "Bang/LayerOrder2.png" #:scale s) "选中了中层形状")
          @(para (stone-image "Bang/LayerOrder3.png" #:scale s) "选中了底层形状")))
}

@handbook-action{可用性测试}
@handbook-word-count[]

可用性测试在教学过程中很重要，
它能很大程度上左右教学质量。
因此，其测试方法就是直接观察学生的课堂反应。
比如，学生项目是否难易适中、能否寓教于乐；
演示项目是否吸引人、
能否帮助学生准确聚焦于知识。

@handbook-scenario{调试方案}
@handbook-word-count[]

在测试方案发现缺陷之后，
调试过程开始分析错误原因、定位错误位置，并修复错误。

本引擎在设计时也考虑了对调试的支持。
或者说，本着“开发者应该也是自己软件的用户”这一原则，
很多调试方法本来就应该直接复用软件的相关功能。

@handbook-action{日志和动态追踪}
@handbook-word-count[]

对于时间敏感型缺陷，
用不上基于断点和变量监视的传统调试方法，
最有效的手段反而还是“一串消息打印代码”。
即，先经过推理大致定位到问题代码的位置，
然后插入一系列 @:id{printf} 语句以启发性输出提示消息。

本引擎包括一个动态追踪用的日志系统，
可以按日志等级更细致的将调试信息输出到终端、
本地的 UDP 日志服务器等。

@handbook-action{实时变量监视}
@handbook-word-count[]

如前文所述，调试器无法用于分析时间敏感性缺陷，
但相对于笨拙地添加打印代码，
有时候确实还是能“实时显示变量值”更方便一些。
因此，本引擎内置了“变量值自动更新”机制，
所有实现了 @:id{IValuelet} 接口的图形元素都自带该能力。
@fig-ref{watch.exe} 通过“监视被选中角色的方向”演示了该机制。

@tamer-figure!['watch.exe "实时变量监视案例"]{
 @(let ([s 0.19])
    (list @(para (stone-image "Bang/Watch1.png" #:scale s))
          @(para (stone-image "Bang/Watch2.png" #:scale s))))
}

C++ 实现该机制的原理很简单，
就是在给 @:id{IValuelet} 类型的可视对象设置值的时候直接传递目标变量的引用或地址即可(@code-ref{watch.cpp})。
该类对象会在自己时间轴的每一帧检查目标地址的值，
如果改变了就根据新值重绘自己。

@tamer-cpp['watch.cpp
           "将目标变量 heading 按引用绑定到量纲文本对象 variable"
           "engine/track.cpp"
           #px"Demonstrating Variable in thesis"]

此外，如果对实时性要求不高，
鼠标提示、角色气泡(说话或思考)都可以达到相似目的。

@handbook-action{调试绘图设施}
@handbook-word-count[]

@handbook-action{错误报告}
@handbook-word-count[]

对于诸如“段错误”这样的低级缺陷，
现代操作系统提供的错误报告已经足够帮助定位缺陷位置。
只需通知构建系统“本次构建应该包含调试信息”，
此配置已经包含在与本引擎配套的外围工具中。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
