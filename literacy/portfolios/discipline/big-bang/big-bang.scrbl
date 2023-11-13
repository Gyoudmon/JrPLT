#lang scribble/base

@require{../../literacy.rkt}
@require{../../../share/stone/shape.rkt}
@require{../../../share/stone/anchor.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@handbook-portfolio-story{宇宙大爆炸}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered

 @item{理解模块化。}

 @item{了解类的继承。}
 
 @item{理解屏幕坐标系。}
 
 @item{掌握基本几何图形的数学表示和@tech{对象}表示。}
 
 @item{掌握基本图形的平移和组合。}]

@handbook-scenario{知识点}

@handbook-action{重点知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Modularity"]{模块化}是一种将复杂问题分解成一个个简单可行的小问题的过程。
  在软件工程中，模块化也是一种分割、组织、打包软件的方法。

  在C++/Python中，
  一个文件对应着一个@handbook-defterm[#:origin "Module"]{模块}，
  相当于一类工具箱；
  C++ 还需要一个@handbook-defterm[#:origin "Header File"]{头文件}，
  同常以@litchar{.hpp}或@litchar{.h}作为后缀名，
  用以说明工具箱里有什么(@tech{函数}、@tech{类}等)。}

 @item{@handbook-defterm[#:origin "Inheritence"]{继承}是一种在已有@tech{类}的基础上定义新@tech{类}的机制，
  用以复用或扩展已有代码。在一个继承关系中，
  已经存在的@tech{类}称为@handbook-defterm[#:origin "Base Class"]{基类}或@handbook-defterm[#:origin "Superclass"]{超类}，
  定义出来的新@tech{类}称为@handbook-defterm[#:origin "Derive Class"]{派生类}或@handbook-defterm[#:origin "Subclass"]{子类}。

  @tech{子类}可以添加新的@tech{字段}和@tech{方法}，
  也可以复用或@handbook-defterm[#:origin "Override"]{覆盖}@tech{基类}的@tech{方法}，
  但一般不删除@tech{基类}的@tech{方法}。}]
 
@handbook-action{难点知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Coordinate System"]{坐标系}是一种描述空间位置的辅助工具。
  最常见的平面@tech{坐标系}由两根相互垂直的数轴构成，通常
  称水平方向的轴为@handbook-deftech[#:origin "X Axis"]{X轴}、
  称竖直方向的轴为@handbook-deftech[#:origin "Y Axis"]{Y轴}。
  在数学教育中，@tech{X轴}方向向@emph{右}、@tech{Y轴}方向向@litchar{上}；
  在计算机图形学中，屏幕坐标系通常会让@tech{Y轴}朝@litchar{下}，
  且只显示第一象限的内容。

 @centered{@make-cartesian-demo[0.75]}}

 @item{在创建基本图形@tech{对象}时，应假设它位于坐标原点，
  之后可根据需要将它们@handbook-defterm[#:origin "Translate"]{平移}到舞台的其他位置。}

 @item{复杂图形可以由若干基本图形@handbook-defterm[#:origin "Compose"]{组合}而成。
  在这个过程中，经常需要@emph{对齐}各个基本图形。
  因此，请牢记以下9个常用锚点。
  @racketerror{锚点不是标准术语，
   此条仅在我的图形系统里有效。
   不过此思路可以套用到其他系统里}。

  @anchor-table}]

@handbook-action{扩展知识}

@itemlist[
 #:style 'ordered

 @item{游戏的本质是数学定义的虚拟世界。}

 @item{探索科学是为了了解和改造我们所处的宇宙，
  学习编程可以创建一个虚拟的宇宙。}]

@handbook-reference[]
