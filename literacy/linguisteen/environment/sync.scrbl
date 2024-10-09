#lang scribble/manual

@(require racket/symbol)
@(require geofun/resize)

@require{../literacy.rkt}
@require{../../share/fstree.rkt}
@require{../../share/diagram/update.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{同步课程源码}

@handbook-scenario{工作目录与项目目录}

所谓工作目录，
简单来说就是指学生用来存放与我的课程有关的所有文档的目录。
这个目录就不要问学生选哪个了，
因为学生(和家长)大概率都还没有相关意识。
直接给建议，
比如在D盘(或其他非系统盘)根目录下创建一个以学生的英文名命名的目录
(@racketerror{目录名一定不能带中文和空白字符})。

工作目录的基本结构见@tamer-figure-ref{wstree}，
除了 @exec{software} 和 @exec{handbook}，
其他直接跟源码有关都是项目目录。
也就是说，项目目录不止一个，
由学生所学课程而定，
一类课程至少对应一个项目。

@tamer-figure!["wstree" "学生工作目录结构(片段)"]{
 @filesystem-tree[
 `(,(string->symbol "D:\\[name]")
   (pbl . "项目制课程源码目录")
   (noi
    (problems . "教师示例源码目录")
    (NAME . "学生[name]的源码目录"))
   (handbook . "讲义和学生手册目录")
   software)]}

一般情况下，对于每一个项目，学生都只需一步克隆：

@itemlist[#:style 'compact
          @commandline{git clone @litchar{stem}@string[#\@]@litchar{plteen.fun}@string[#\:]@litchar{noi}.git D:\@litchar{[name]}\noi}
          @commandline{git clone @litchar{stem}@string[#\@]@litchar{plteen.fun}@string[#\:]@litchar{pbl}.git D:\@litchar{[name]}\pbl}]

@handbook-scenario{更新课程源码}

走到这一步，剩下的就简单了。
但这一步每节课上课时学生都要跟着@tamer-figure-ref{update-ws}走一遍，
这也是真实世界软件项目的基本操作。

@tamer-figure!["update-ws" "更新课程源码流程图" @(geo-scale update.dia 0.30)]

注意，@tamer-figure-ref{update-ws}更新了全部的与学生代码无关的课程软件，
与学生有关的项目代码只演示了 NOI 一个，
如果学生有多个项目，也需要重复这个过程一个一个更新，
或者只更新与当前课程有关的那个。

@handbook-scenario{源码库结构}

做软件可以是一件比较有趣的事，教科书里的标准源码目录比较无趣，我的目录名字跟通用做法不一样。
因此，特别在这里说清楚。

对照@tamer-figure-ref{srctree}，
每个课程的源码按如下目录组织（先把课程假想成软件，再把软件假想成数字生物，这样会更容易理解）。

@itemlist[
 #:style 'compact

 @item{@tamer-deftech[#:key "digivice"]{.}: 源码根目录，相当于常规的 @tt{src}。
  这里存放最直接的源码，包括带 @italic{main} 的程序入口文件、
  你愿意共享给别人使用的模块文件和头文件。

  顺便说一句，用我自己的构建工具编译 C++ 项目，
  可以存在多个 @italic{main}，一般一个 @italic{main} 代表一个程序。}
 
 @item{@tamer-deftech{digitama}: 私有源码目录，表示这个目录里的源码不对外开放。
  @tt{digitama} 是“数码蛋”的意思，意为我们的软件是从这个蛋里孵出来的。}
 
 @item{@tamer-deftech{stone}: 资源目录，相当于常规的 @tt{res} 或 @tt{Assets}。
  这里存放资源和(静态)配置信息，比如图片素材、界面文字的多语言翻译等。
  @tt{stone} 源自“罗赛塔石碑(Rosetta Stone)”，
  石碑上刻着对于任务而言至关重要的信息，这些信息只可读不可写。}

 @item{@tamer-deftech{tamer}: 测试目录，是常规的 @tt{tests} 和 @tt{docs} 的结合。
  这里用于存放测试程序和项目文档。
  我喜欢将文档和测试结合，这样我提供的文档，
  既包含了函数签名，还包含用法举例，以及算法的正确性验证等信息。
  @tt{tamer}是“驯兽师”的意思，寓意明显。}

 @item{@tamer-deftech{literacy}: 出版物源码目录。
  这是我的个人偏好，在软件项目里没有对应的标准目录。
  相对于上面提到的项目文档，这个目录里面的内容要正式一些，
  作品可能是一本书、一篇论文等。
  @tt{literacy} 就是字面意思“文字素养”。}
 
 @item{@tamer-deftech{village}: 协作目录，相当于开源项目的 @tt{contrib}。
  总体上说，这个目录里面的东西比较包罗万象，可能是任何东西。
  @tt{village} 源自“创始村(Primary Village)”，数码宝贝出生的地方。
         
  在本系列课程中，其他类型的课程代码也放在这里了。比如
  @itemlist[#:style 'compact
            @item{@tt{diagram}: 图表源码，用于生成辅助学习用的各类示意图表。}
            @item{@tt{sketch}: 草稿源码，每个文件都自带 main，探索语言的某一个知识点。}
            @item{@tt{problem}: 问题源码，每个文件都自带 main，对应着各类比赛的题库。}]}]

以上条目未必都会出现在同一课程目录里，如果出现，一定符合上述解释。
有时为了方便，也会省略 @tt{village} 目录，而把它里面的文件夹直接提取到项目目录中。

@tamer-figure!["srctree" "源码目录结构"]{
 @filesystem-tree[
 '(|.|
   (info.rkt . "Racket 软件包元信息文件")
   (digitama . "私有源码目录") 
   (stone . "资源目录")
   (literacy . "出版物源码目录")
   ((village . "协作目录")
    (diagram . "图表源码目录")
    (sketch . "草稿源码目录")
    (problem . "题库源码目录"))
   ((compiled . "Racket 编译缓存目录")
    (typesetting . "出版物成品目录")
    (native . "含 C++ 可执行文件")))]}

此外，还有两个特殊文件(夹)，文件名不可更改。

@itemlist[
 #:style 'compact
 @item{@tamer-deftech{info.rkt}: Racket 软件包的元信息文件。用于配置我的 C++ 构建工具。}
 @item{@tamer-deftech{compiled}: Racket 编译缓存目录。
  用于存放所有编译过程中可以自动生成的文件。
  注意，此目录不唯一，各个被编译的文件所在的目录里都有一个。

  以下几个子目录在我们的课程中也可能用到：      
  @itemlist[#:style 'compact
            @item{@tamer-deftech{typesetting}: 存放 @tech{literacy}的输出。}
            @item{@tamer-deftech{native}: 存放 C++ 二进制文件的输出。}]}]

@tt{compiled} 和 Python 源码目录里的 @tt{__pycache__} 功能相同。

此外，Python 目录本身也有一系列约定俗成的规则，
但目前我们的课程不会涉及相关概念，暂且不表。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
