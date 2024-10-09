#lang typed/racket/base

(provide (all-defined-out))

(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! setup.dia #:at 2 [#:start-name "Setup" #:background 'White] #:-
  (jump-to 5 '/plteen.fun)
  (jump-back)
  
  (move-down 1 (string->symbol "自备电脑\nGot a Personal Computer\nfor Yourself!"))
  (move-down 1 ':|Run PowerShell|)
  (move-down 1.2 (string->keyword ">>:选择工作目录\nSelect a Folder as Your Workspace"))
  (move-down 1.2 (string->keyword ">>:下载开发软件\nDownload Development Software") "cd D:\\[name]")
  (move-down 1.2 (string->symbol ":安装开发软件\nInstall Development Software"))
  (move-down 1.2 (string->symbol ":编辑PATH环境变量\nEdit the PATH Environment Variable") "sysdm.cpl")
  (move-down 1.2 (string->keyword ">>:克隆我开发的课程软件\nClone Course-related Software"))
  (move-down 1.2 (string->symbol ">>:安装我的课程软件\nInstall Course-related Software")
             (string-append "raco pkg install --auto --link C:\\digimon " "\n"
                            "raco pkg install --auto --link C:\\graphics"))
  (move-down 1.2 (string->keyword ">>:克隆课程源码库\nClone Source Repositories"))
  (move-down 1 'End$)

  (jump-back)
  (jump-left 2 '/doc/~\\cpp/)
  (jump-to '/plteen.fun)
  (L-step '/doc/~\\cpp/ #false (cons (string-append "git clone stem@plteen.fun:pbl.git D:\\[name]\\pbl" "\n"
                                                    "git clone stem@plteen.fun:noi.git D:\\[name]\\noi") #false))
  
  (jump-back)
  (jump-left 2 '/doc/C:\\opt/)
  (jump-to '/plteen.fun)
  (L-step '/doc/C:\\opt/ #false (cons (string-append "git clone stem@plteen.fun:digimon.git  C:\\opt\\digimon" " \n"
                                                     "git clone stem@plteen.fun:graphics.git C:\\opt\\graphics" "\n"
                                                     "git clone stem@plteen.fun:JrPLT.git    C:\\opt\\JrPLT   ") #false))

  (jump-back)
  (jump-left 2 '/doc/~\\software/)
  (jump-to '/plteen.fun)
  (L-step '/doc/~\\software/ #false (cons "scp -r stem@plteen.fun:software D:\\[name]\\software" #false))
  
  (jump-back)
  (move-left 2 '/doc/D:\\name/ "mkdir D:\\[your-name]"))
