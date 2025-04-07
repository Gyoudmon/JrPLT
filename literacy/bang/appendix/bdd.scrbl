#lang scribble/manual

@require{../literacy.rkt}
@require{../diagram/spec.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix-story[#:tag "bdd:flow:ex"]{行为驱动开发工作流举例}
@handbook-word-count[]

@handbook-scenario{Matrix 算术运算规范}

我们以 Matrix 对象的算术运算规范为例来说明本引擎开发过程中的测试工作流。

@handbook-action{编写最小测试单元}

此步骤详见@Secref{matrix}。

@handbook-action{编写测试驱动函数或测试包装器}

这里之所以不直接写测试函数，
是因为这些函数本身不具备测试功能。
它们只负责把测试系统提供的输入转交给待测单元，
完事再把返回值传递回测试系统。

有时，为减少此步骤的工作量，
相似功能可以包装到一起。
比如，在@code-ref{mtx-mul.c}中，
矩阵的加法和减法合并为了 @:sym{matrix_add_subtract}；
矩阵的标量乘法和标量除法合并为了 @:sym{matrix_scale}。

@tamer-cpp['mtx-mul.c
           "矩阵算术运算测试函数"
           "cc/mathematics/matrix/fxmatrix.cpp"
           #px"Matrix Arithematics"]

此步骤有两个注意事项，

@handbook-itemlist[
 #:style 'compact

 @item{此步骤是为了让 Racket 能够通过 FFI 顺利测试 C++ 代码，
  而 FFI 最擅长识别 C 语言标准的函数接口。
  因此测试驱动函数或测试包装器都必须导出为 C 接口才能正常对接。}

 @item{使用 Windows MSVC 编译动态链接库默认不导出任何符号，
  必须明确声明需要导出的符号。
  因此，我的构建工具会自动定义名为 @:sym{__ffi__} 的宏，
  确保符号一定会被导出。}]

@handbook-action{编写测试驱动函数的 FFI 绑定}

@(tamer-rkt "cc/mathematics/matrix/fxmatrix.rkt" #px"fxmatrix_add_subtract")

外部函数接口描述不必非得手写，
可以用其他工具自动解析 C++ 文件现场生成。

@handbook-action{编写行为规范}

@(tamer-rkt "cc/mathematics/matrix/fxspec.rkt" #px"Arithmetic Operators")

现阶段的行为规范 DSL 保留了 s-expression 的特征，
并不强调符合正常人的阅读习惯。
而行为规范本身也就是一个再正常不过的 Racket 模块文件或脚本文件，
直接运行它就可以得到测试报告了。

从本规范也能看出我们测试方案的灵活性，
需要编写的 C++ 测试驱动代码和 FFI 绑定代码都很少，
实际的测试代码既可以借助 Racket 已有的软件生态(比如
@:id{matrix+} 和 @:id{matrix*} 都来自 Racket 自己的数学库)，
也能够充分发挥 Racket 函数式编程的便利。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
