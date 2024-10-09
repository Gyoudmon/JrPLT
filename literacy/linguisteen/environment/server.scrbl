#lang scribble/manual

@(require geofun/resize)

@require{../literacy.rkt}
@require{../../share/diagram/setup.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-root-story{教师机配置}

@handbook-scenario{课程源码的git裸库}

教师需要在自己的电脑上(或购买云服务器、或配个树莓派)搭建一个简单的 git 服务器。
新建一个“允许通过 SSH 远程登陆”的系统用户，
并在该用户的HOME目录下创建一系列用于共享给学生的 git 裸库。
学生机器上只需安装 git 即可。

我的服务器域名地址是 @litchar{plteen.fun},
分配给学生的登录名是 @litchar{stem}。
已经包含以下裸库:

@itemlist[
 #:style 'compact
 
 @item{@tt{digimon.git}: 我的 C++ 构建工具}
 @item{@tt{graphics.git}: 我的函数式图形库(含流程图绘制工具)}
 @item{@tt{pbl.git}: C++ 版项目制课程源码(不含游戏引擎源码)}
 @item{@tt{JrPLT.git}: 项目制课程软件的运行时库(含游戏引擎的动态链接库及其依赖的第三方库、资源文件、数据文件等)}
 @item{@tt{noi.git}: 信息学奥赛、其他C++比赛类课程源码库}
 @item{@tt{python.git}: Python 课程源码大杂烩(同时包含比赛、项目制课程源码，以及项目制课程依赖的游戏引擎源码)}
 ]

现以 @tt{digimon.git} 为例说明管理裸库的一般步骤。

远程登陆@litchar{stem}账号，
此时的“当前目录”就是该账号的HOME目录。
简单起见不要切换到其他目录，
然后执行：

@itemlist[#:style 'compact
          @commandline{git init --bare @litchar{digimon}.git}]

创建好所有裸库之后，就可以@emph{断开远程链接、不用再管服务器账号}了。
对于新创建的裸库，还需在对应的@emph{开发库}里添加一个名为
@litchar{student}的@tt{remote}地址：

@itemlist[#:style 'compact
          @commandline{git remote add student @litchar{stem}@string[#\@]@litchar{plteen.fun}@string[#\:]@litchar{digimon}.git}]

@tt{remote}地址可以写成任何标准网络地址格式。
而 git 搭了 SSH 的顺风车，
因此这个地址也可以写成 SSH 的地址格式：
@racketresultfont{用户名@string[#\@]主机地址:相对于用户首目录的路径}。
以后，如源码有所变动，除了完成你自己惯例要做的事外，
还需要额外将变动推送给学生裸库(不要加@Flag{u}选项，
否则学生用的裸库就会变成你的默认上游)：

@itemlist[#:style 'compact
          @commandline{git push student}]

学生克隆源码库也用上述 SSH 格式的地址。
比如：

@itemlist[#:style 'compact
          @commandline{git clone @litchar{stem}@string[#\@]@litchar{plteen.fun}@string[#\:]@litchar{digimon}.git C:\opt\digimon}]

对于每一个具体课程的源码库，教师需要精细控制源码库的内容，
包括但不限于：

@itemlist[#:style 'compact
          @para{去掉与本课程无关的代码，但保持目录结构不变。游戏引擎已经包含在@tt{JrPLT}中无需额外配置。}
          @para{修改学生程序的配置选项，使得构建程序和项目作品知道去@filepath{C:\opt}找资源。}]

对于常规学习，教师可以只准备一个包罗万象的大库，也可以按班级单独准备。
逼真一些地模拟软件开发过程应该是学生每次上课都先同步代码。

对于团队项目，教师需要在服务器端专门为每个团队每个项目新建一个裸库，
方便团队成员之间共享各自负责的模块。

@handbook-scenario{其他共享资源}

除了源码裸库，
服务器还可以用来共享其他对学生有价值的文件，
用以备份打印的资料(这些资料很可能不会被妥善保管和有效利用)。

目前我的服务器已经包含的共享文件有:

@itemlist[
 #:style 'compact
 
 @item{@tt{software}: 课程所学的开发软件}
 @item{@tt{handbook}: 讲义和学生手册}
 @item{@tt{3d}: 3D 软件}
 ]

学生复制共享文件也用上述 SSH 格式的地址，
配合 @exec{scp} 命令。
比如：

@itemlist[#:style 'compact
          @commandline{scp -r @litchar{stem}@string[#\@]@litchar{plteen.fun}@string[#\:]@litchar{software} @litchar{D:\}}]

@handbook-scenario{新生来了}

@tamer-figure-ref{welcome}先给出新生入学的一般流程，详细内容在后续章节中慢慢说道。

@tamer-figure!["welcome" "新生入学流程" @(geo-scale setup.dia 0.45)]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
