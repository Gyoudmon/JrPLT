#lang scribble/base

@require{../literacy.rkt}
@require{../../share/stone/string.rkt}

@(define example-font (desc-font #:family 'monospace #:size 16))

@(define (array-example id content)
   @centered{@(let* ([arr content]
                     [size (string-length arr)])
                (tamer-figure! id (format "含~a个元素的字符数组，内容下标最大值是~a"
                                    size (sub1 size))
                               (make-chars-array #:font example-font
                                                 arr)))})

@(define (string-example id content)
   @centered{@(let* ([arr content]
                     [size (string-length arr)])
                (tamer-figure! id (format "含~a个字符的字符串，内容下标最大值是~a，占据~a个格子"
                                    size (sub1 size) (add1 size))
                               (make-chars-array #:font example-font #:string? #true
                                                 arr)))})

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-portfolio-story{字符串}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered
 
 @item{理解@tech{字符}、@tech{字符数组}、@tech{字符串}}
 
 @item{了解@tech{字符串}是@tech{类}}

 @item{理解@tech{字符串}的基本操作}

 ]

@handbook-scenario{知识点}

@handbook-action{基础知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Character" #:abbr "Char"]{字符}就是单个字母、文字、标点符号，
  以及诸如空格这样的无形符号。其@tech{常量}要用单引号@litchar{'}引起来，
  并且@emph{一般情况下}单引号里最多只能有一个@tech{字符}。}
  
 @item{@handbook-defterm[#:origin "Character Array"]{字符数组}就是由@tech{字符}组成的@tech{数组}。

  @(array-example 'ex:a:chs "我是字符数组，野蜂飞舞，乱象环生")}
 
 @item{@handbook-defterm[#:origin "String"]{字符串}也是一串@tech{字符}，
  其@tech{常量}要用双引号@litchar{"}引起来。}

 @item{@tech{字符串}会在自己结尾处偷偷追加一个@tech{终止字符}。
  @handbook-defterm[#:origin "Terminator Character"]{终止字符}也是一种无形的@tech{字符}，
  在@emph{数值}上等于@emph{整数}@racket[0]，
  写作@litchar{'\0'}，注意与真正的@emph{字符}@litchar{'0'}区分。
  @tech{终止字符}会额外占用一字节内存空间，但@tech{字符串}的长度不包含@tech{终止字符}。

  @(string-example 'ex:a:str "我是字符串，终止字符，唯我独有")}
 ]

@handbook-action{进阶知识}

有了前面学习@tech{数组}的经历，
我们对@tech{字符数组}和@tech{字符串}的基本操作都不应该陌生了，
直接就当成是特殊的@tech{数组}即可(@tamer-code-ref{defstr_as_array})。

@margin-note{自己动手画画看，字符数组 @variable{chars} 里的内容是什么？}

@tamer-c++['defstr_as_array "数组式初始化" "string.cpp" #px"数组式初始化"]

@itemlist[
 #:style 'ordered
 
 @item{相较于普通@tech{数组}，@tech{字符数组}和@tech{字符串}还有更简便直观的初始化方法(@tamer-code-ref{defstr})。
        
  @tamer-c++['defstr "字符串式初始化" "string.cpp" #px"字符串式初始化"]
  
  @(array-example 'ex:a:str->chs (string #\c #\h #\a #\r #\- #\a #\r #\r #\a #\y #\nul))}

 
 @item{两个或多个@tech{字符串}可以通过加号(@litchar{+})@tamer-deftech[#:origin "Concatenate"]{连接}成一个全新的@tech{字符串}。
            
  @centered{@(tamer-figure! 'ex:s:cat "字符串连接操作"
                            (bitmap-concatenate "计算机" "科学" #:font example-font))}}]

@handbook-action{扩展知识}

C++ 的@tech{字符串}类型名是@type{std::string}。
所以你看，一般的数据@tech{类型}如@type{int}、@type{float}、@type{bool}、@type{void}，
它们的名字里没有奇怪的符号，因为那些@tech{类型}都很基础，一般不用特别对待；
但是@tech{字符串}可以有很多种，跟你自己写的任何@tech{类}并无区别。
将来你很可能会自己写一个更好用的@tech{字符串}@tech{类}(比如做游戏软件时)，
初学阶段用C++的标准@type{std::string}就好。

@tech{字符数组}是比较基础的简单概念，在真实世界的代码里很少直接使用。
@type{std::string} @tech{类}提供了很多操作@tech{字符串}的@tech{方法}，
帮我们隐藏了直接使用@tech{字符数组}的不少麻烦，
包括上面的赋值(@litchar{=})和@tech{连接}(@litchar{+})运算符。

@itemlist[
 #:style 'ordered

 @item{插入（@id{insert}）操作：在给定@tech{字符串}的指定位置插入别的@tech{字符数组}或@tech{字符串}，并且返回自己。
  当插入位置在字符串结尾时，也可以使用@id{append}@tech{方法}或@litchar{+=}运算符省去插入位置。

  @tamer-c++['s.insert "插入操作" "string.cpp" #px"插入操作"]}
                                                      
 @item{删除（@id{erase}）操作：删除给定@tech{字符串}的指定位置处的连续几个@tech{字符}。

  @tamer-c++['s.erase "删除操作" "string.cpp" #px"擦除操作"]}

 @item{更新（@id{replace}）操作：将给定@tech{字符串}的指定位置处的连续几个@tech{字符}替换成别的@tech{字符数组}或@tech{字符串}。
  更新操作可以看成是“先删除再插入”，请结合@tech{方法}签名理解这点。
  如果只需要替换一个字符，别忘了@litchar{[]}和@litchar{=}运算符。

  @tamer-c++['s.replace "更新操作" "string.cpp" #px"替换操作"]}

 @item{查找（@id{find}）操作：在给定@tech{字符串}的指定位置开始查找别的@tech{字符串}，
  如果能找到，返回它@emph{第一次}出现的位置；
  如果不能找到，返回特殊值@type{std::string}::@var{npos}(通常是@racket[-1])。
  
  @tamer-c++['s.find "查找操作" "string.cpp" #px"查找操作"]}

 @item{比较（@id{compare}）操作：比较给定@tech{字符串}与另一个@tech{字符串}的大小。
  返回@racket[0]表示相等；返回负数表示小于；返回正数表示大于。
  如果只关心是否相等，可以像比较数字大小一样直接使用 @litchar{==} 操作符。

  @tech{字符串}的大小规则如下：
  @itemlist[
 #:style 'ordered

 @item{从两个@tech{字符串}的第一个@tech{字符}开始，
    依次比较每一个位置处的@tech{字符}大小，直到遇到不同@tech{字符}。
    字母表中先出现的@tech{字符}较小(小写字母 > 大写字母 > 数字)；}

 @item{如果其中一个@tech{字符串}是另一个@tech{字符串}的前缀，
    则较短的那个较小。}]
  
  @tamer-c++['s.compare "比较操作" "string.cpp" #px"比较操作"]}

 ]

@handbook-reference[]
