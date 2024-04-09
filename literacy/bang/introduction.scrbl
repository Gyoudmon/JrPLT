#lang scribble/manual

@require{literacy.rkt}

@(define-url-bib tiobe
   "TIOBE Index - TIOBE"
   "https://www.tiobe.com/tiobe-index/"
   #:date "2024")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{绪论}

@handbook-scenario{背景和意义}

我们现在正处在信息时代向智能时代的过渡阶段。
这个阶段的典型特点是，
我们理解日常生活和社会现象所需要的知识门槛越来越高。
上个世纪，人们只要能识字、会看报即可;
而现在，高等数学、计算机科学和编程技能成了基础必修课。
一方面是高端职业本就建立在信息和算法之上，
一方面算法经程序语言落地成为的数字系统也在不可逆转地刻画着普通人的生活习惯、
职业状态，乃至思维方式@$cite{HCPiCW}。

未来的就业市场会更偏好“喜欢数学”的人，
而现实情况却要严峻很多：
在科学、技术、工程、数学(STEM)领域，
招聘到靠谱从业者所需时间至少是其他职业的两倍@$cite{EnMSMtd}。

虽然可能未必有人能说清楚将来的社会具体会对现代人类提出何种要求，
但我们总可以帮助学生在学习的早期阶段就开始储备必要的STEM和计算机科学知识。
因此，STEM相关学科教师和编程教师面临的挑战不仅仅是激活学生对自己所教课程的兴趣，
还要确保学生真的能够学有所成、学以致用。

@handbook-scenario{国内外研究现状}

自2013年(或更早的时候)起，
世界各国陆续将信息技术@handbook-footnote{严格来说，计算机科学、信息技术、
 编程是三个相关但完全不同的概念，但在讨论中小学相关课程的时候不做特别区分。}纳入中小学必修课。
芬兰的孩子从一年级开始学；
英国的更早，提前到了5岁。
其中也包括起步稍晚的中国、日本等亚洲国家@$cite{DTCuYRT}。
特别值得一提的是，各国的编程教育目标都包括了这条：
“计算思维”这个脱胎于计算机科学的方法和技能应当渗透到所有类型的教育中@$cite{lq:white}。

但就整体现状而言，整个行业都比较混乱，
大家都在缓慢探索比较靠谱的实践途径，
也必须面对一些棘手的现实问题。
一方面是行业以外的人，包括传统科目教师、家长和学生，普遍对此趋势不敏感；
一方面是从业人员缺少靠谱的教学资源，
对“教什么”、“怎么教”，以及“为什么难学”这三大开放性教育话题的研究也不够充分。
青少年不同于大学生和成年人，不能照搬他们的解决方案，
就如同医生、心理咨询师针对有相同问题的青少年需要特别的干预方案一样，
教育教学也是一类专业技能，关乎心智活动和有效指令设计@$cite{EnMSMtd}。

具体到青少年编程教育，
其看似丰富多彩、花样繁多，
归根到底其实就是两大类：
C++ 凭借其与信息学奥赛的强关联成为了学校、家长和学生认知中的竞技专用语言；
Scratch 及其衍生产品凭借其“低门槛、高天花板、多实践路经”的鲜明特点
成为了学校教育、少儿编程比赛、科创比赛的主语言，
K-12教育教学研究也广泛使用Scratch作为技术平台，
以至于它在 TIOBE 编程语言指数排行榜上竟然闯进了前20名@$cite[tiobe]。
Python 介于两者之间，既可以用来参加难度较低的语言和算法比赛，
也可以替代Scratch参加软硬件结合的科创类比赛。

@handbook-scenario{本文主要工作}

本文从程序语言理论视角，结合国内外跨学科教育研究现状，
及国内青少年学业压力这个现实问题出发，
设计和研发了一套比较另类的 C++/Python 语言基础课，
让学生在竞赛类课程之外还有其他入门专业编程的选项，
理想情况下能够帮助学生将编程技能转化为连接基础学科、
辅助校内学习的生产力工具。

确切地说，本文的工作并不排斥竞赛类课程，
而是不止步于竞赛课程(及其传统授课方式)。
当代的校园生活过于漫长，
学生往往不知道自己为什么要学习、比赛，
在多数家长真正意识到自己孩子在中小学阶段的学习困境之前没有特别好的解决方案@$cite{HaP}。
好在，现在我们知道了，
通过改变授课方式(比如：项目式学习、游戏式学习等)在小班制教学中可以一定程度上缓解此问题@$cite{DaiPBLvPGE}。

本文主要关注与这套课程配套的软件技术部分。

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-reference[]
