#lang scribble/base

@require{../literacy.rkt}
@require{../../share/stone/string.rkt}

@(define example-font (desc-font #:family 'monospace #:size 16))

@(define (random-natural ch)
   (remainder (char->integer ch) 100))

@(define (random-flonum ch)
   (/ (remainder (char->integer ch) 100) 10.0))

@(define (char->flonum ch)
   (real->double-flonum (- (char->integer ch)
                           (char->integer #\0))))

@(define (array-example id content legend-fmt char->datum)
   @centered{@(let* ([arr content]
                     [size (string-length arr)])
                (tamer-figure! id (format legend-fmt size (sub1 size))
                               (make-chars-array #:font example-font #:char->datum char->datum
                                                 arr)))})

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{一维数组}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered

 @item{理解@tech{数组}的基本概念}

 @item{理解@tech{下标}运算符}

 @item{了解@tech{初始化列表}}
 
 @item{理解@tech{数组}的基本操作}

 ]

@handbook-scenario{知识点}

@handbook-action{基础知识}

@itemlist[
 #:style 'ordered

 @item{在计算机科学和程序语言中，
  @handbook-defterm[#:origin "Array"]{数组}是最简单的复合数据@tech{类型}，
  用于存储固定数量、相同@tech{类型}的一组@tech{值}。

  @itemlist[
 #:style 'ordered
 
 @item{@tech{数组}中的@tech{值}称为@handbook-defterm[#:origin "Element"]{元素},
    每个@tech{元素}都唯一对应一个@handbook-defterm[#:origin "Subscript"]{下标}。}
    
 @item{@tech{数组}中所含@tech{元素}的个数称为@handbook-defterm[#:origin "Array Length"]{数组的长度}。}
 
 @item{@tech{数组}的@tech{元素}在内存中按顺序连续存放在一起，
    因此@tech{数组}的@tech{下标}也是从@racket[0]开始的连续自然数。

    @(array-example 'ex:a:n "array<byte>" "含~a个元素的自然数数组，内容下标最大值是~a" random-natural)
    @(array-example 'ex:a:fl "vectorof flonum" "含~a个元素的浮点数数组，内容下标最大值是~a" random-flonum)}]}

 ]

@handbook-action{进阶知识}

@itemlist[
 #:style 'ordered
 
 @item{跟其他@tech{变量}一样，使用@tech{数组}的第一步是@emph{定义}。
  定义@tech{数组}的基本语法就是在定义@tech{变量}的语法基础上使用中括号@litchar{[]}增加@tech{数组的长度}信息(@tamer-code-ref{defa})。
  @tech{数组的长度}必须是任意@emph{自然数}，包括@racket[0]。
  此时的@tech{数组}从语法上讲已经可以正常使用，
  但它的内容尚不确定，可能是任何稀奇古怪的脏数据。

  @tamer-c++['defa "定义数组" "array.cpp" #px"定义数组"]}

 @item{@handbook-deftech[#:origin "Initializer List"]{初始化列表}在写法上像数学的集合，
  用大括号@litchar["{}"]和逗号@litchar[","]表达数据集。
  常用于给@tech{数组}或其他没有@tech{构造函数}的@tech{类}成员(@tamer-code-ref{ilst})设置合理的初始值。

  @tamer-c++['ilst "初始化列表" "array.cpp" #px"初始化列表"]

  使用@tech{初始化列表}方式定义数组时有个额外好处：
  @tech{初始化列表}本身已经自带了@tech{数组的长度}信息，
  因此声明变量时的@litchar{[]}里可以省略长度。
  如果不省略@tech{数组的长度}，
  @tech{初始化列表}所含@tech{元素}的数量@racketerror{不能比已知的长度多}，
  少了的部分自动补@racket[0]。
  比如：@tamer-code-ref{ilst} 中的单精度浮点数数组
  @:var{singles} 的实际内容是：

  @(array-example 'ex:a:fl+0 (string #\1 #\2 #\3 #\0 #\0 #\0 #\0 #\0)
                  "含~a个元素的单精度浮点数数组，内容下标最大值是~a"
                  char->flonum)}

 @item{@handbook-deftech[#:origin "Subscript Operator"]{下标运算符}用于引用@tech{下标}处的@tech{数组}@tech{元素}。
  @margin-note{下标在数学中的写法是这样的: @${a_1}。}
  写法上跟声明时指定@tech{数组的长度}一样，用中括号@litchar{[]}和自然数表示。
  而且中括号里面的自然数可以是@tech{常量}、@tech{变量}，
  以及任何计算结果是自然数的@tech{表达式}、@tech{函数}调用等。}

 @item{@handbook-deftech[#:origin "Traversal"]{遍历}是指按照某种顺序不重复、不遗漏地访问数据集合中的全部@tech{元素}。
  对于@tech{数组}这样的简单复合@tech{类型}而言，
  @tech{遍历}的过程就是让@tech{数组}的@tech{元素}自己按顺序“报数”：
  第一个@tech{元素}喊@racket[0], 第二个@tech{元素}喊@racket[1], 以此类推。
  写成代码就是一个常规循环(@tamer-code-ref{cp})。
  
  @tamer-c++['cp "遍历" "array.cpp" #px"遍历"]
  
  本例同时演示了@tech{数组}的另一种初始化方法：
  @tamer-code-ref{cp}循环内部的 @tt{flonums[n] = integers[n]}，
  赋值号(@litchar{=})左边的@tt{flonums[n]}指向@tt{flonums}的第@tt{n+1}个元素；
  右边的@tt{integers[n]}指向@tt{integers}的第@tt{n+1}个元素，
  整个表达式的意思是取出@tt{integers}第@tt{n+1}个元素并赋值给@tt{flonums}相同位置的元素；
  整个循环的作用是使用整数数组@tt{integers}里的内容来初始化浮点数数组@tt{flonums}。}
 ]

@handbook-action{扩展知识}

@itemlist[
 #:style 'ordered

 @item{C 和 C++ 自带一个众所周知的安全隐患：
  你在操作@tech{数组}@tech{元素}时可以用任意大小的自然数作为@tech{下标}，
  但只有当@tech{下标}小于@tech{数组的长度}时才不会导致程序出莫名其妙的问题。
  因为，计算机内存中，不仅仅是@tech{数组}的@tech{元素}按顺序存放，
  @tech{数组}也要跟其他数据一起按顺序占据内存空间。
  也就是说，内存中紧挨着某个@tech{数组}的存储空间里可能就存放着别的更重要的数据。
  假如你给的@tech{下标}越界了，可能无事发生，也可能稀里糊涂修改了重要数据，
  严重时会导致程序当场奔溃给你看。}

 @item{@tech{数组}在使用上有诸多不便之处，在真实的软件项目中很少直接使用@tech{数组}，
  但在学习其他更好的用法之前，我们必须先理解@tech{数组}的底层原理。
  比如：到目前为止，你可能会烦躁，@tech{数组的长度}在定义、初始化、@tech{遍历}、输出
  时都要用到，修改其中的一处得连带修改其他所有的地方，太容易遗漏了，
  有没有方法可以直接知道@tech{数组的长度}信息呢？

  可以使用 @:delim{sizeof} 分别获得@tech{数组}占据的全部内存空间
  和@tech{数组}的@tech{类型}所需要的内存空间，
  两相一除得到的就是@tech{数组的长度}(@tamer-code-ref{al})。
 
  @tamer-c++['al "计算数组长度" "array.cpp" #px"sizeof"]}
 ]

@handbook-reference[]

