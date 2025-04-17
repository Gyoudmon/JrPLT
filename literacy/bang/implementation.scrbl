#lang scribble/manual

@require{literacy.rkt}
@require{diagram/matter.rkt}

@(require geofun/vector)

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{系统详细设计和实现}

@handbook-scenario{可视对象模型}

@tamer-figure![
 'cosmos.dia
 @list{可视对象模型类图}]{
 @(geo-scale cosmos.dia 0.45)
}

比之上一章关注整体架构的 @fig-ref{visobj.dia}，
本章的 @fig-ref{cosmos.dia} 更关注可视元素的组织和管理。

IDisplay 代表操作系统视角下的物理窗口，
其直接实现类 Universe 借 SDL2 实现了标准游戏引擎都会做的最小子集：
初始化引擎、创建窗体、绘制窗体不同区域、切换窗体显示模式，等等。
最后启动游戏主循环。主循环的主要任务有：

@handbook-itemlist[
 #:style 'compact
 @item{响应、分发所有事件，此后被分发事件会传递给 Plane 实例和 Matter 实例。}
 @item{处理本引擎定义的标准快捷键。包括屏幕截图、发送“文件保存”指令、处理用户的文本输入，等等。}
 @item{按需要重绘窗体。比如，当任意一个可视元素通知自己需要更新，或当用户改变了窗口大小。}
 @item{显示日志信息。默认输出到终端，也可以在子类覆盖为让某个可视精灵“说”出来。}
 ]

IScreen 则代表本引擎视角下的虚拟窗口。
现有实现中，Cosmos 实例通过 LinkedPlaneInfo 实例管理场景列表，
并且把其存储在场景实例内部，
使得场景实例也可以通过它反向获取与自己有关的 IScreen 实例信息。
比如，获取可视区域大小，发出“已更新，请重绘”消息，等等。

同理，Plane 实例也通过 IMatterInfo 实例管理游戏物体；
Matter 实例则通过 IMatterInfo 实例发送“重绘”请求。
特别地，Continent(大陆)类是一种特殊的物体子类，
它把场景实例包装成了单个的物体，
使得我们可以通过组合单个物体来实现对复杂物体的构建。
这也是 IScreen 接口必须存在的原因。

@handbook-action{物体的信息}

为了解耦合场景控制逻辑和物体的外在表现。
本引擎内部使用 IMatterInfo 存储物体在场景中的信息，
Matter 实例只需关心自身的属性和数据，
特别是如何渲染自己、如何响应来自场景的消息。
@tab-ref{MatterInfo}列出了当前 Plane 实现中用到的 IMatterInfo 典型字段。
除了这些，还有很多其他数据。
比如，鼠标选择状态、私有时间轴、滑动进度条等，
不一一列出。 

@tamer-table!['MatterInfo "MatterInfo类的典型字段"]{
 @(list @bold{字段名称}        @bold{字段类型}                   @bold{描述})
 @(list @:id{x}               @:type{float}                    @elem{物体位置横坐标})
 @(list @:id{y}               @:type{float}                    @elem{物体位置纵坐标})
 @(list @:id{motion_actions}  @:type{std::deque<MotionAction>} @elem{动作序列})
 @(list @:id{bubble}          @:type{IMatter*}                 @elem{说话/思考气泡对象})
 @(list @:id{canvas}          @:type{Tracklet*}                @elem{轨迹对象})
 @(list @:sym{prev}           @:type{IMatter*}                 @elem{前一个物体})
 @(list @:sym{next}           @:type{IMatter*}                 @elem{后一个物体})
}

@tab-ref{MatterInfo}隐含表达了一些有趣的设计：

@handbook-itemlist[
 #:style 'compact

 @item{物体的位置信息竟然也保存在了 MatterInfo 实例里。
  这是在对真实世界建模,现实中的人类需要借助 GPS 才能知道自己的准确位置；
  物体的外在形象也跟地理坐标关系不大。
  即，可视对象可以被平移而保持外观不变。}
 
 @item{所有的物体都实现了 IMovable 接口，
  可分别从标量和矢量角度设置其速度、加速度、最终速度等运动学参数。
  有速度就有运动时间，就需要合理管理运动序列，
  这就有了 motion_actions。}
 
 @item{bubble 指向说话或思考的气泡实例。
  比如，@fig-ref{pv-sys}中被数数的鸡在报数。}

 @item{canvas 指向绘制物体运动轨迹的 Tracklet 实例，万物皆海龟。
  比如，@fig-ref*[#:subidx 'b]{motion}中野外露营中毒的阿加特。}

 @item{最后，Plane 实例通过双向链表来组织Matter实例。}
 ]

@tamer-figure!['pv-sys "边数数边报数"]{@(stone-image "Bang/Bubble.png" #:scale 0.216)}

@handbook-action{对象版程序入口}

在教学实施中，学生通过继承 Cosmos 类创建窗体和屏幕，
继承 Plane 类创建场景，并将其添加到 Cosmos 实例中。
Plane 类最常被覆盖的几个方法是：

@handbook-itemlist[
 #:style 'compact
 @item{load: 加载场景中的所有静态物体对象。通常静态物体对象会以成员变量的形式定义在场景中。}
 @item{reflow: 当窗口大小发生变化时重新布局游戏物体。}
 @item{update: 处理时间轴的一帧。
  配合 Plane 类提供的容器标配函数，学生可在此按教学目标控制场景中的物体，
  包括创建/删除诸如子弹这样的临时物体对象、执行碰撞检测等。}
 @item{on_char: 处理键盘事件。通常用于发送预定义指令、控制角色移动等。}
 @item{on_select: 处理鼠标“选择/取消选择”事件，“被鼠标选中或取消选中”是“点击”的默认逻辑。}
 ]

就教学而言，学生很少会直接继承 Matter 类或其子类，内置的可视元素已经足够学习用了。
Plane 类则可以选择额外继承 I8WayMotion 或 IPlatformer
分别实现平面八向移动型游戏（@fig-ref*[#:subidx 'b]{rich}）和平台跳跃型游戏（@fig-ref{splash}）。

@handbook-scenario{时间轴}

时间轴是运动系统和角色动画的基础。
本引擎的时间轴基于 SDL2 的定时器和事件机制，
对时间的度量是基于整数的离散量。
具体来说，每一个 Plane 实例和 Matter 实例通过 update 方法处理自己的帧事件，
该方法返回一个整数值，代表下一次更新距现在的时间间隔的最小值。

Matter 子类以 ISprite 为代表，
它在自己的 update 方法里切换动作以达到播放效果。
在此机制下，用户可以通过设置精灵角色的“理想帧频”来控制动作序列的播放速度，
再配合合适的贴图可在视觉上达到加速、减速的效果。

Plane::update 方法除了能根据“理想帧频”整体控制自己管理的所有的可视元素的帧频上限，
还负载控制角色的移动、角色说话/思考气泡的显示、隐藏和跟随。
负责角色移动的方法主要有四个：

@handbook-itemlist[
 #:style 'compact
 @item{move: 瞬移到指定位置，使用相对位置参数。}
 @item{move_to: 瞬移到指定位置，使用绝对位置参数。}
 @item{glide: 在指定秒内滑行到指定位置，使用相对位置参数。}
 @item{glide_to: 在指定秒内滑行到指定位置，使用绝对位置参数。}
 ]

@$$[#:tag "n(s)"]{ n = \lfloor s \times f \rfloor }

当前版本使用@eqref[#:label "公式"]{n(s)}将滑动秒数 @${s} 映射为帧数 @${n}， @${f} 为帧频。
滑动是匀速移动，之后按等距离点位瞬移物体，
若不能到达目标位置就留到下一帧，简单粗暴但已经能满足要求了。
此处，角色的移动由 Plane 实例控制，角色的贴图动画由角色自己控制。
如此，可以平滑展示以假乱真的跑动效果。
IMatterInfo 类内部维持一个运动队列，
当物体正在运动时，
所有的滑动和瞬移都会追加到该队列尾部。
如此，用户可异步设定角色的运动路线。

@tamer-figure!['motion "运动中的精灵角色"]{
 @(let ([s 0.16])
    (list @(para (stone-image "Bang/Motion1.png" #:scale s) @elem{醉汉漫步主题课})
          @(para (stone-image "Bang/Motion2.png" #:scale s) @elem{自回避游走主题课})))
}

走路是最常用的运动效果之一，
@fig-ref{motion} 选了随机游走相关主题课来展示其表现力，
在吸引学生的同时又不至于干扰他们思考重点。
“醉汉漫步”主题课里还使用了 Tracklet 实例显示运动轨迹。

@handbook-scenario{布局管理}

传统的图形用户接口开发框架往往会提供好些种类的布局管理器帮助开发者管理可视元素在界面的位置和大小。比如：

@handbook-itemlist[
 #:style 'compact
 @item{静态布局，直接基于绝对坐标手动精确控制。
  缺点显而易见， Pygame 编程课都是这个思路。}

 @item{容器布局，事先设定几类独特的容器，比如流式容器、盒式容器、网格等。
  然后这些容器根据自己的特点按部就班地把可视元素或另一个容器安置在自己的槽位中。
  这类布局的大问题是方案很多，但每种方案都只解决部分布局问题。换句话说，
  要么无法精确控制、要么限制太多、要么用起来过于繁琐。}

 @item{弹性布局，这是现代前端设计的首选方案。缺点是概念过于复杂。}
 ]

本引擎提供两类布局思路，一种是网格布局，另一种是锚点位置布局。

@handbook-action{网格布局}

网格布局也有两种方案：

@handbook-itemlist[
 #:style 'compact
 @item{直接使用 Plane 类内置的网格，该网格也不必非得占据整个可视区域。}
 @item{使用 IAtlas 类，它也是一种可视元素，
  对标 2D 游戏引擎中比较常见的 Atlas 和 Tilemap。}]

在使用本引擎制作的项目制课程中，网格布局常被用来构造背景层。
IAtlas 在使用时，还可以无视其贴图的物理尺寸而新建一个逻辑网格，
这在构造伪3D场景时比较有用。
比如@fig-ref{rich}的生命游戏和@fig-ref{motion}的自回避游走用的都是同一种美术资源。

所有的美术资源通过 std::shared_ptr 共享，确保相同资源只会被加载一次。

@handbook-action{锚点位置布局}

锚点位置布局的思路并不新奇，但确实很适合本引擎的工作领域。
具体来说，位置和锚点是本引擎的核心概念之一，
也是瞬移和滑动方法的首要位置参数。

可视元素都有面积。
因此，移动(瞬移和滑动)语义应该被定义为：
将可视元素 A 上的一个锚点移动到目标位置 P 处。
P 可以通过以下任意一种方式指定：

@handbook-itemlist[
 #:style 'compact

 @item{可视区域中的绝对坐标;}
 @item{可视区域中某个可视元素上的锚点;}
 @item{可视区域中某个可视元素锚点的横坐标和另一个可视元素锚点的纵坐标组合成的新坐标。}
 ]

锚点是可视元素自身的某个位置，
其值通常表示为一对百分比，分别相对于可视元素自己的宽度和高度。
每一个可视元素都有9个最常用的锚点，这些锚点应该拥有自己的名字，
详见@tab-ref{anchor}和@fig-ref*[#:subidx 'a]{gallery}。

@tamer-table!['anchor "具名锚点"]{
 @(list @bold{锚点缩写}  @bold{锚点全称} @bold{位置语义} @bold{宽度百分比} @bold{高度百分比})
 @(list @elem{LT}      @elem{Left-Top} @elem{左上角}   @racket[0.0]     @racket[0.0])
 @(list @elem{CT}    @elem{Center-Top} @elem{中上}     @racket[0.5]     @racket[0.0])
 @(list @elem{RT}     @elem{Right-Top} @elem{右上角}   @racket[1.0]     @racket[0.0])
 @(list @elem{LC}   @elem{Left-Center} @elem{左中}     @racket[0.0]     @racket[0.5])
 @(list @elem{CC} @elem{Center-Center} @elem{中心}     @racket[0.5]     @racket[0.5])
 @(list @elem{RC}  @elem{Right-Center} @elem{右中}     @racket[1.0]     @racket[0.5])
 @(list @elem{LB}   @elem{Left-Bottom} @elem{左下角}   @racket[0.0]     @racket[1.0])
 @(list @elem{CB} @elem{Center-Bottom} @elem{中下}     @racket[0.5]     @racket[1.0])
 @(list @elem{RB}  @elem{Right-Bottom} @elem{右下上角}  @racket[1.0]     @racket[1.0])
}

@tamer-figure!['gallery "图形版“Hello, World!”"]{
 @(list @(para (let ([anchor-font (desc-font #:size 9)])
                 (geo-cc-superimpose
                  (geo-blank 1 116)
                  (for/fold ([base (geo-square 96 #:fill 'palegreen #:stroke #false)])
                            ([superimpose (in-list (list geo-lt-superimpose geo-ct-superimpose geo-rt-superimpose
                                                         geo-lc-superimpose geo-cc-superimpose geo-rc-superimpose
                                                         geo-lb-superimpose geo-cb-superimpose geo-rb-superimpose))])
                    (let* ([anchor (string-upcase (cadr (string-split (symbol->string (object-name superimpose)) "-")))]
                           [label (geo-text anchor anchor-font #:color 'crimson)])
                      (superimpose base label)))))
               @elem{具名锚点位置示意图})
        @(para (stone-image "Bang/Shape.png" #:scale 0.16) @elem{图形的组合与分解}))
}

@fig-ref{gallery} 即是应用本引擎的第一个主题课，
学生会先学习数轴、坐标系、基本图形的代数表示，
然后理解复杂图形可以分解成基本图形，
并通过创建和组合处一个有意义的图案来理解基本图形也可以组合成复杂图形。
语言表达类似这样：将墙壁移动到屋顶的中下点，并保持与墙壁的中上点对齐。

基于锚点位置的布局是所有项目都会用到的知识，
直观易懂，且不会像静态布局那样无法适配各种尺寸的显示器。
很适合培养学生的几何素养和代数素养。

当可视元素需要变形时(比如缩放、换字体等)，
也可以通过锁定锚点使它变形之后处在正确的位置上。

@handbook-scenario{物理系统}

本引擎的物理系统包括常见的数学对象和物理对象。
比如，点、向量、矩阵、轴对齐包围盒，等等。
代码层面大量使用模版以减少编写不同类型的数值函数的重复代码、
或预分配大小固定的矩阵对象。

在实际教学中，色彩空间是另一个重要的光学实验课程。
一般科学教科书上的颜色混合相关实验，其效果并不明显，
部分原因是材料廉价、环境干扰大。
因此，从数学角度设计模拟程序就是很重要的补充。
在上课过程中，教师也可以顺便科普色度图和显示器失真问题。
本引擎提供两种配置的色度图，对比效果见@fig-ref{chromaticity}。

@tamer-figure!['chromaticity "色度图演示程序"]{
 @(let ([s 0.16])
    (list @(para @(stone-image "Bang/Chromaticity1.png" #:scale s) @elem{标准色度图})
          @(para @(stone-image "Bang/Chromaticity2.png" #:scale s) @elem{sRGB-D65 色度图})))}

@handbook-scenario{扩展性设计}

除了以上跟游戏引擎直接相关的部分，
本引擎实际上也是一个完整的图形用户接口框架，
教师可以根据自己的需要开发出复杂的演示程序和应用程序。
比如，@fig-ref{IMS}的成绩管理系统。

@tamer-figure!['IMS "成绩管理系统"]{@(stone-image "Bang/IMS.png" #:scale 0.24)}

该系统还演示了两个额外功能：

@handbook-itemlist[
 #:style 'compact
 @item{本引擎支持多选机制。此处同时选中了科目和班级两个可视元素。}
 @item{本引擎虽没有直接提供文本输入框，但在必要的时候，可以激活窗口底部的命令行区域。
  该区域允许用户输入 utf-8 文本，并且支持退格键删除错别字。}]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
