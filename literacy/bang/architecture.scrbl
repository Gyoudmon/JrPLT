#lang scribble/manual

@require{literacy.rkt}
@require{diagram/architecture.rkt}
@require{diagram/matter.rkt}

@(require geofun/vector)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{系统概要设计}

JrPLT 即是本文所提技术方案的核心系统。
采用 Standard C++ 2017 编写，
兼容 Windows、macOS、Linux 三大操作系统。

@handbook-scenario{引擎内核架构设计}

软件系统往往会按照明显的层次结构构建，
并且约定上层模块建立在下层模块提供的功能之上，
但是不能反过来形成“循环依赖”。
这个设计原则对于诸如游戏引擎这样的大型软件系统尤其重要。

@tamer-figure![
 'arch.dia
 @list{引擎内核架构简图}]{
 @(geo-scale architecture.dia 0.5)
}

@fig-ref{arch.dia}展示的是本引擎内核的架构简图，
它也按照层次结构组织，
但跟现代游戏引擎的层次结构有所不同。
除去最底层的操作系统和最顶层的应用，
我们按照自底向上的顺序简要介绍一下各层职能，
重要、有趣的部分留到后文合适的地方再详细说明。

@handbook-itemlist[
 #:style 'compact

 @item{C++ 运行时和 SDL2 共同构成了本引擎的平台无关层。
  或者反过来说，
  C++ 标准库和SDL2 帮我们隐藏了几乎所有的平台无关细节。}
 
 @item{数据类型包装和底层渲染共同构成了本引擎的基础设施层。
  该层是本项目编写代码的起点，
  当前版本就是补充了 C++ 标准库该有而没有的功能，
  后续版本可向 GPU 或更强大的图形系统借力。
 }
 
 @item{物理系统和虚拟化共同构成了本引擎的核心层。
  该层定义了游戏引擎中常见的数学对象、物理对象和支持系统。
  属于用户无法直接感知但又不可或缺的部分。}

 @item{功能层则连接了引擎内核和应用层。简而言之，
  该层以图形用户接口的名义组织了下层的各模块，
  让用户能够直接感知到引擎正在运行，
  同时定义了应用软件应该遵守的逻辑模型和限制。}
 ]

@handbook-scenario{应用层总体设计}

从前文需求分析部分可知，
项目制课程由一系列主题课构成，
每个项目制课程拥有自己的独立启动程序来管理所有主题课。
它作为本引擎的代表应用类型之一，
非常适合用来讨论“可视元素模型”的设计。

@tamer-figure![
 'visobj.dia
 @list{可视对象模型总体设计类图}]{
 @(geo-scale visobj-mod.dia 0.42)
}

游戏软件和图形用户接口类软件都是对象导向编程的舒适领域，
在本引擎及其应用的实际开发中，接口和抽象类是最核心的概念。
但为保持类图简介，@fig-ref{visobj.dia} 并未标记接口。
从整体上看，
本引擎可视元素模型的设计借鉴了现有游戏引擎的设计，
但在命名上往宇宙尺度扩展了一些。
比如：

@handbook-itemlist[
 #:style 'compact

 @item{Cosmos(宇宙) 对应 Game World，代表最高层级容器。}

 @item{Plane(2D 平面) 对应 Scene, 代表一个逻辑独立的关卡、场景。即，课程项目或演示项目。}

 @item{Matter(物质/物体) 对应 Game Object，代表最基本的可视元素或可交互元素。
  如此命名可避免学生混淆 Matter 和术语 Object(对象)。}
 ]

以 Matter 类为起点的庞大树状(网状)结构构成了本引擎丰富的可视对象。
包括但不限于基本图形、文本框(普通文本、数值文本、量纲文本)、
位图及精灵图、背景图集、数据可视化对象，
以及其他复杂交互对象。

@tamer-figure!['rich "学生项目中丰富的可视元素"]{
 @(let ([s 0.16])
    (list @(para (stone-image "Bang/conway.png" #:scale s) "生命游戏")
          @(para (stone-image "Bang/evolution.png" #:scale s) "演化游戏")))}

@fig-ref{rich} 通过两个终极学生团队项目展示了本引擎丰富的内置可视元素。
同时也展示了课程项目界面的基本构成：标题栏、指令栏、工作区并无本质差别，
皆是对可视元素的合理选择和布局。

左上角的猫是本引擎内置的动作最丰富、最细致的精灵，
出自 Microsoft Office 早期版本的代理助手。
在本引擎中，它继续扮演助手角色，
诸如“返回主界面”、“全屏/窗口切换”、
“报告日志信息”等功能都是通过与它的互动来完成。

@handbook-scenario{网络通信协议}

网络通信能力是本引擎锦上添花的功能，
多用于教师制作的互动项目或高阶学生的小型网络项目。
设计上一切从简，不依赖外部服务器。
所有通信都基于 UDP 协议，
消息的二进制格式头部字段见@tab-ref{slang}，
所有多字节字段均按网络字节序传输。
如果增加额外字段，
需确保消息头部总字节为偶数方可正常校验。

@tamer-table!['slang "消息的二进制结构头部"]{
 @(list @bold{字段名}          @bold{偏移量} @bold{字节长度}  @bold{描述})
 @(list @:sym{magic}          @racket[0]   @racket[2]     @elem{魔数，协议标识符 @:val{0x237E}})
 @(list @:sym{version}        @racket[2]   @racket[1]     @elem{版本号，目前只有 @:val{0x01}})
 @(list @:sym{payload_type}   @racket[3]   @racket[1]     @elem{有效负载类型})
 @(list @:sym{checksum}       @racket[4]   @racket[2]     @elem{校验码，同 IPv4 协议})
 @(list @:sym{transaction_id} @racket[6]   @racket[2]     @elem{版本1额外字段，消息号})
 @(list @:sym{response_port}  @racket[8]   @racket[2]     @elem{版本1额外字段，应答号})}

消息结构头部之后紧跟有效负载(payload)，通常是一个 ASN.1 序列。
相对于 XML、JSON、Protobuf 这几个更为常见的数据格式，
ASN.1 有两个不可替代的优势：

@handbook-itemlist[
 #:style 'compact

 @item{ASN.1 是强类型语义数据编码的成熟方案，在对安全要求较高的工业领域仍不可替代。}
 @item{将来给学生开设数据编码、解码课程时，可以很方便地帮助学生理解高精度整数和浮点数。}]

ASN.1 的代表缺点是其本身过于复杂。
在实际课程中，本引擎提供所有 ASN.1 基本数据类型的编码、解码函数，
学生在大部分情况下都只需要继承代表序列类型的抽象类 IASNSequence，
并复用基本数据类型和序列类型的编码、解码函数来完成对其关键方法的覆盖即可。
简而言之，学生的关注重点是数据实体类字段的顺序和类型。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
