#lang scribble/base

@require{../literacy.rkt}

@handbook-portfolio-story{输入输出流}

@handbook-scenario{课程目标}

@itemlist[
 #:style 'ordered

 @item{了解赛题的基本@tech{输入-处理-输出}结构}
 
 @item{了解赛题相关的@tech{流}：文件流、标准流}
 
 @item{理解流对象操作的输入运算符(@racketparenfont{>>})和输出运算符(@racketparenfont{<<})}
 
 @item{知道管道是一种基于@tech{文件}的进程间通信机制}]

@handbook-scenario{知识点}

@handbook-action{基础知识}

@itemlist[
 #:style 'ordered

 @item{计算机外部存储设备中用来长期保存数据的基本概念是@handbook-defterm[#:origin "File"]{文件}。}

 @item{读取@tech{文件}需要打开一个@handbook-defterm[#:origin "file input stream"]{文件输入流}。}

 @item{写入@tech{文件}需要打开一个@handbook-defterm[#:origin "file output stream"]{文件输出流}。}
 
 @item{所有程序都自带一个@handbook-defterm[#:origin "standard input" #:abbr "stdin"]{标准输入流}、
  一个@handbook-defterm[#:origin "standard output" #:abbr "stdout"]{标准输出流}和一个
  @handbook-defterm[#:origin "standard error" #:abbr "stderr"]{标准错误输出流}。}]

@handbook-action{扩展知识}

@itemlist[
 #:style 'ordered

 @item{@handbook-defterm[#:origin "Pipeline"]{管道}是一种基于@tech{文件}的进程间通信机制。
  比如，通过连接两个程序的@tech{标准输入流}和@tech{标准输出流}就可以
  把两个程序看成一个整体共同完成一个更复杂的任务。}]

@handbook-reference[]
