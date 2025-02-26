#lang typed/racket/base

(provide (all-defined-out))

(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! update.dia #:at 3 [#:start-name "Update\nCourse Software" #:background 'White] #:-
  (move-down 1 ':|Run PowerShell|)
  (move-down 1 '-=)
  (L-step -3+i)
  (move-down 1.0 '>>:|Enter digimon| "cd C:\\opt\\digimon")
  (move-down 1.2 '>>:|Update digimon| "git pull")
  (move-down 1.2 '>>:|Compile digimon| "raco wisemon -d")
  (move-down)
  (move-right '#:home '.=-)
  (move-down 0.5 '=-)
  (move-down 1 'Exit$)

  (jump-back '-=)
  (L-step -1+i)
  (move-down 1.0 '>>:|Enter graphics| "cd C:\\opt\\graphics")
  (move-down 1.2 '>>:|Update graphics| "git pull")
  (move-down 1.2 '>>:|Compile graphics| "raco wisemon -d")
  (move-down '.=-)
  
  (jump-back '-=)
  (L-step +1+i)
  (move-down 1.0 '>>:|Enter JrPLT| "cd C:\\opt\\JrPLT")
  (move-down 1.2 '>>:|Update JrPLT| "git pull")
  (move-down '.=-)

  (jump-back '-=)
  (L-step +3+i)
  (move-down 1.0 '>>:|Enter NOI| "cd D:\\[name]\\NOI")
  (move-down 1.2 '>>:|Update NOI| "git pull")
  (move-down '.=-)
  (move-left '#:home)

  (jump-to 7.5 '/plteen.fun)
  (move-down '>>:|Update digimon|)
  (move-left 1 #false "git pull")
  (move-left 6)
  
  (jump-up '-=)
  (jump-left '>>:|Update NOI| (string->keyword "/doc/D:\\[name]\\noi/"))
  (jump-left '>>:|Update JrPLT| (string->keyword "/doc/C:\\opt\\JrPLT/"))
  (jump-left '>>:|Update graphics| (string->keyword "/doc/C:\\opt\\graphics/"))
  (jump-left '>>:|Update digimon| (string->symbol "/doc/C:\\opt\\digimon/"))
  (move-down 1 #false "cd")

  (jump-back)
  (move-down 1 #false "cd")
  
  (jump-back)
  (move-down 1 #false "cd")

  (jump-back)
  (move-down 1 #false "cd"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  update.dia)
