#lang scribble/base

@require{../literacy.rkt}
@require{../../share/stone/string.rkt}

@(define example-font (desc-font #:family 'monospace #:size 16))

@(define (char->natural ch)
   (remainder (char->integer ch) 100))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{一维数组与字符串}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered

 @item{理解@tech{数组}的基本概念}
 
 @item{理解@tech{字符}、@tech{字符数组}、@tech{字符串}}

 @item{理解@tech{数组}和@tech{字符串}的基本操作}]

@handbook-scenario{知识点}

@handbook-action{基础知识}

@itemlist[
 #:style 'ordered

 @item{在计算机科学和程序语言中，
  @handbook-defterm[#:origin "Array"]{数组}是最简单的复合数据@tech{类型}，
  用于存储固定数量、相同@tech{类型}的一组@tech{值}。
  
  @tech{数组}中的@tech{值}称为@handbook-defterm[#:origin "Element"]{元素},
  每个@tech{元素}都唯一对应一个@handbook-defterm[#:origin "Subscript"]{下标}。

  @tech{数组}中所含@tech{元素}的个数称为@handbook-defterm[#:origin "Array Length"]{数组的长度}。
  
  @tech{数组}的@tech{元素}在内存中按顺序连续存放在一起，
  因此@tech{数组}的@tech{下标}也是从@racket[0]开始的连续自然数。

  @centered{@(let* ([arr "naturals array"]
                    [size (string-length arr)])
               (tamer-figure! 'ex:a:n (format "含~a个元素的自然数数组，内容下标最大值是~a" size (sub1 size))
                              (make-chars-array #:font example-font #:char->datum char->natural
                                                arr)))}}

 @item{@handbook-defterm[#:origin "Character" #:abbr "Char"]{字符}就是单个文字，
  但是也包括空格这种无形的文字。其@tech{常量}要用单引号@litchar{'}引起来，
  并且@emph{一般情况下}单引号里最多只能有一个@tech{字符}。
  
  @handbook-defterm[#:origin "Character Array"]{字符数组}就是由@tech{字符}组成的@tech{数组}。
  换句话说，@tech{字符数组}里的每个@tech{元素}都是@tech{字符}。

  @centered{@(let* ([arr "我是字符数组，野蜂飞舞，乱象环生"]
                    [size (string-length arr)])
               (tamer-figure! 'ex:a:chs (format "含~a个元素的字符数组，内容下标最大值是~a" size (sub1 size))
                              (make-chars-array arr #:font example-font)))}}
 
 @item{@handbook-defterm[#:origin "String"]{字符串}也是一串@tech{字符}，
  其@tech{常量}要用双引号@litchar{"}引起来，
  双引号里@tech{字符}的数量没有特别要求(只要内存够大)。

  @tech{字符串}和@tech{字符数组}最显著的区别有三个：
  
  @itemlist[
 #:style 'ordered

 @item{@tech{字符串}会在自己结尾处偷偷追加一个@tech{终止字符}。

    @handbook-defterm[#:origin "Terminator Character"]{终止字符}也是一种无形的@tech{字符}，
    在@emph{数值}上等于@emph{整数}@racket[0]，
    写作@litchar{'\0'}，注意与真正的@emph{字符}@litchar{'0'}区分。

    @tech{终止字符}会额外占用一字节内存空间，但@tech{字符串}的长度不包含@tech{终止字符}。
  
  @centered{@(let* ([arr "我是字符串，终止字符，护甲在手"]
                    [size (string-length arr)])
               (tamer-figure! 'ex:a:str (format "含~a个字符的字符串，内容下标最大值是~a，占据~a个格子"
                                          size (sub1 size) (add1 size))
                              (make-chars-array #:string? #true #:font example-font
                                                arr)))}}

 @item{@margin-note*{其他@tech{类型}的@tech{数组}也都有各自更好用的@tech{类}。}
    @tech{字符数组}是比较基础的简单概念，在真实世界的代码里很少直接使用。
    @tech{字符串}在现代语言里通常会被设计成@tech{类}，
    帮我们隐藏了直接使用@tech{字符数组}的不少麻烦。
    比如，我们无需自己操心@tech{终止字符}的问题，
    也不需要自己去数有多少个@tech{字符}。

    C++ 的@tech{字符串}类型名是@type{std::string}。
    所以你看，一般的数据@tech{类型}如@type{int}、@type{char}，
    它们的名字里没有奇怪的符号，就是因为那些@tech{类型}都很基础，在哪都一样；
    但是@tech{字符串}可以有很多种，你自己写一个都行，
    初学阶段我们用C++的标准@type{std::string}就好。}

 @item{两个或多个@tech{字符串}可以通过加号(@litchar{+})@tamer-deftech[#:origin "Concatenate"]{连接}成以一个@tech{字符串}。

    @centered{@(tamer-figure! 'ex:s:cat "字符串连接操作"
                              (bitmap-concatenate "计算机" "科学" #:font example-font))}}]
  }]

@handbook-action{进阶知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-deftech[#:origin "Subscript Operator"]{下标运算符}用于获取@tech{下标}处的@tech{数组}@tech{元素}。
  写法上跟声明时指定@tech{数组的长度}一样，用中括号@litchar{[]}，效果上看跟使用普通变量一样。
  @margin-note{下标在数学中的写法是这样的: @${a_1}。}
  比如：@tamer-code-ref{cp}循环内部的 @tt{str[n] = cs[n]}，
  赋值号(@litchar{=})左边的@tt{str[n]}指向@tt{str}的第@tt{n+1}个元素；
  右边的@tt{cs[n]}指向@tt{cs}的第@tt{n+1}个元素，
  整个表达式的意思是取出@tt{cs}第@tt{n+1}个元素并赋值给@tt{str}相同位置的元素。}

 @item{@handbook-deftech[#:origin "Initializer List"]{初始化列表}在写法上像数学的集合，用大括号@litchar["{}"]。
  常用于给@tech{数组}或其他没有@tech{构造函数}的@tech{类}初始化(@tamer-code-ref{ilst})。

  @tamer-c++['ilst "初始化列表" "array.cpp" #px"初始化列表"]
 }
 
 @item{@handbook-deftech[#:origin "Traversal"]{遍历}是指按照某种顺序不重复、不遗漏地访问数据集合中的全部@tech{元素}。
  对于@tech{数组}这样的简单复合@tech{类型}而言，
  @tech{遍历}的过程跟让@tech{数组}的@tech{元素}自己按顺序“报数”很相像：
  第一个@tech{元素}喊@racket[0], 第二个@tech{元素}喊@racket[1], 以此类推。
  写成代码就是一个常规循环(@tamer-code-ref{cp})。

  @tamer-c++['cp "遍历" "array.cpp" #px"遍历"]}
 ]

@handbook-action{扩展知识}

@itemlist[
 #:style 'ordered

 @item{对于 C 和 C++，有个众所周知的安全隐患：你在操作@tech{数组}@tech{元素}时可以用任意大小的自然数作为@tech{下标}，
  但只有当@tech{下标}小于@tech{数组的长度}时才不会导致程序出莫名其妙的问题。
  因为，计算机内存中，不仅仅是@tech{数组}的@tech{元素}按顺序存放，
  @tech{数组}也要跟其他数据一起按顺序占据内存空间。
  也就是说，内存中紧挨着某个@tech{数组}的存储空间里可能就存放着别的更重要的数据。
  假如你给的@tech{下标}越界了，可能无事发生，也可能稀里糊涂修改了重要数据，
  严重时会导致程序当场奔溃给你看。}
 ]

@handbook-reference[]

