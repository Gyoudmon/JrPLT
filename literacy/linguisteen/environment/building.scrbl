#lang scribble/manual

@require{../literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{构建 C++ 项目}

在开发软件的过程中，打开 PowerShell 并且 @exec{cd} 到当前项目目录是个好习惯。
在本系列课程中，一般就是指 @tech{info.rkt} 文件所在的目录。
如果涉及多个项目，每个项目单独开一个 PowerShell 比较合理，不用来回切换了。
如果使用 Visual Studio Code，本课程源码自带运行和调试配置，无需额外折腾。

对于单个带 @tt{main} 函数的代码，
一般不需要特别复杂的编译参数，可以直接编译

@itemlist[#:style 'compact
          @commandline{raco wisemon -d cc [入口文件.cpp]}]

其中，@exec{raco} 是 Racket 提供的命令行；
@exec{wisemon} 是我的构建工具；
@Flag{d} 表示输出编译过程，但不要太啰嗦；
@exec{cc}是 C/C++ 编译器（C/C++ Compiler）的首字母缩写。

我的构建工具中还有一个 @exec{wizarmon}，
它在 @exec{wisemon} 的基础之上先将修改后的代码重新编译成程序，
然后再根据项目配置决定是运行程序还是测试程序：

@itemlist[#:style 'compact
          @commandline{raco wizarmon -v [入口文件.cpp]}]

对于 @tt{pbl} 这样的复杂项目，编译参数通常都会很复杂。
因此我会在 @filepath{info.rkt} 里配置好，然后直接编译

@itemlist[#:style 'compact
          @commandline{raco wisemon -d cc}]

这次连入口文件都不用指定，
它会自己解析 @tech{info.rkt} 文件找到要编译的文件，
并设置好相应的参数。

无论用哪种方式，编译完了之后的可执行文件，
都在与入口文件相同目录的 @filepath{compiled/native} 子目录里。
比如， @tech{info.rkt} 中已经配置了 @filepath{cpp/FontBrowser.cpp}，
它在 Windows 下对应的可执行文件名是@filepath{cpp/compiled/native/FontBrowser.exe}。

终于写完了，青少年学C++第零难：搞定学习环境。
这个世界上开发软件众多，但竟然没有一个对青少年友好，
即使它们宣称自己的目标受众是学生。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
