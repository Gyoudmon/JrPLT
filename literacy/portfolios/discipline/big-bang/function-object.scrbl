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

@handbook-action{前置知识}

@itemlist[
 #:style 'ordered

 @item{字面上看，@handbook-defterm[#:origin "Function"]{函数}即“功能”，表示计算机要做的事。

  @tech{函数}可以类比成语言学里的“动宾结构”。
  于是，@tech{函数}的前两个要素是“名称”和“参数列表”。
  “函数名”对应着“动作名称”；
  “参数列表”对应着“宾语(含补语)”，宾语的数量可能不唯一。
  参数列表中的各个参数用逗号(@racketparenfont{,})分隔。

  @tech{函数}的第三个要素是“返回值”(@tech{函数}的执行结果)。
  有返回值的@tech{函数}的最典型例子是@handbook-defterm{数学函数}：
  根据一定的运算规则将一个或多个输入参数@term-name{映射}为一个返回值。}

 @item{@handbook-defterm[#:origin "Variable"]{变量}即可以被改变的量。
  在计算机中它指的是@focus{内存地址}，用来提示“这段内存中的内容可以修改”。

  @tech{变量}可类比成储物柜的抽屉(所在的@tech{位置})。
  但需要注意的是，@tech{变量}即是储物柜抽屉(所在的@tech{位置})本身，
  “变量名”才是贴在抽屉上的“标签”。}

 @item{@handbook-defterm[#:origin "Constant"]{常量}即是不可被改变的量，
  它也可以像@tech{变量}一样拥有名字。}
 
]

@handbook-action{进阶知识}

@itemlist[
 #:style 'ordered

 @item{@tech{变量}(和@tech{常量})有两个属性：@tech{类型}和@tech{值}。
  @handbook-defterm[#:origin "Value"]{值}指的是变量的@focus{内容}；
  @handbook-defterm[#:origin "Type"]{类型}则用于提示“@focus{如何解读}这个内容”。}

]

@handbook-action{最终知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Class"]{类}即自定义@tech{类型}，
  它封装了一系列相关联的@tech{变量}和@tech{函数}。
  
  @tech{类}的@tech{变量}称为@handbook-defterm[#:origin "Field"]{字段}；
  @tech{类}的@tech{函数}称为@handbook-defterm[#:origin "Method"]{方法}。}

 @item{@tech{类}可以比作设计图，根据此设计图创建出的产品
  称为该@tech{类}的@handbook-defterm[#:origin "Object"]{对象}。
  每个@tech{对象}都是自己@tech{类}型的@handbook-defterm[#:origin "Instance"]{实例}。
  
  在语言学视角下，@tech{对象}是@tech{函数}的真·主语。}

 @item{@tech{类}的实例化过程本质上是调用了该@tech{类}的@handbook-defterm[#:origin "Constructor"]{构造函数}，
  用以完成对@tech{对象}的初始化工作。因此实例化的语法与@tech{函数}调用相似。}
 
]

@handbook-reference[]
