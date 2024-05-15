#lang scribble/base

@require{../literacy.rkt}
@require{../../share/stone/string.rkt}

@(define example-font (desc-font #:family 'monospace #:size 16))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{字符串}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered
 
 @item{理解@tech{字符}、@tech{字符数组}}

 @item{了解@tech{字符串}是@tech{类}}

 @item{理解@tech{字符串}的基本操作}

 ]

@handbook-scenario{知识点}

@handbook-action{基础知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Character" #:abbr "Char"]{字符}就是单个文字，
  但是也包括空格这种无形的文字。其@tech{常量}要用单引号@litchar{'}引起来，
  并且@emph{一般情况下}单引号里最多只能有一个@tech{字符}。}
  
 @item{@handbook-defterm[#:origin "Character Array"]{字符数组}就是由@tech{字符}组成的@tech{数组}。
  换句话说，@tech{字符数组}里的每个@tech{元素}都是@tech{字符}。

  @centered{@(let* ([arr "我是字符数组，野蜂飞舞，乱象环生"]
                    [size (string-length arr)])
               (tamer-figure! 'ex:a:chs (format "含~a个元素的字符数组，内容下标最大值是~a" size (sub1 size))
                              (make-chars-array arr #:font example-font)))}}
 
 @item{@handbook-defterm[#:origin "String"]{字符串}也是一串@tech{字符}，
  其@tech{常量}要用双引号@litchar{"}引起来，
  双引号里@tech{字符}的数量没有特别要求(只要内存够大)。}

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
 ]

@handbook-action{进阶知识}

有了前面学习@tech{数组}的经历，
我们对@tech{字符数组}和@tech{字符串}的基本操作都不应该陌生了，
直接就当成是特殊的@tech{数组}即可(@tamer-code-ref{defstr_as_array})。

@tamer-c++['defstr_as_array "字符数组和字符串都是特殊数组" "string.cpp" #px"数组式初始化"]

@handbook-action{扩展知识}

@margin-note*{其他@tech{类型}的@tech{数组}也都有各自更好用的@tech{类}。}
@tech{字符数组}是比较基础的简单概念，在真实世界的代码里很少直接使用。
@tech{字符串}在现代语言里通常会被设计成@tech{类}，
帮我们隐藏了直接使用@tech{字符数组}的不少麻烦。

C++ 的@tech{字符串}类型名是@type{std::string}。
所以你看，一般的数据@tech{类型}如@type{int}、@type{char}，
它们的名字里没有奇怪的符号，就是因为那些@tech{类型}都很基础，在哪都一样；
但是@tech{字符串}可以有很多种，你自己写一个都行，
初学阶段我们用C++的标准@type{std::string}就好。

@itemlist[
 #:style 'ordered

 
 @item{两个或多个@tech{字符串}可以通过加号(@litchar{+})@tamer-deftech[#:origin "Concatenate"]{连接}成以一个@tech{字符串}。
            
  @centered{@(tamer-figure! 'ex:s:cat "字符串连接操作"
                            (bitmap-concatenate "计算机" "科学" #:font example-font))}}
 ]

@handbook-reference[]

