#lang typed/racket/base

(provide (all-defined-out))

(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-flowchart! hsenc.dia
  #:parameterize ([default-diaflow-arrow-label-inline? #false]
                  [default-diaflow-arrow-label-rotate? #true])
  [#:start-name "同音替换加密" #:background 'White
   #:node-desc #hasheq((initial-cipher-table! . "初始化密文表")
                       (>>define-plain-text . "写下明文\nPlaintext")
                       (encrypting? . "存在字符 p?")
                       (random . "随机确定密文索引 n")
                       (encrypt . "计算当前字符密文 c")
                       (<<cipher . "写下密文 c"))] #:-
  (move-down 1 'initial-cipher-table!)
  (move-down 1 '>>define-plain-text)
  (move-down 1 'encrypting?)
  (move-down 1 'random "Do")
  (move-down 1 'encrypt)
  (move-down 1 '<<cipher)
  (move-down 0.75)
  (move-left 1.2)
  (L-step 'encrypting? "下一个字符" "for each")

  (move-right 1.2)
  (move-down '<<cipher)
  (move-down 1.0)
  (move-left '#:home)
  (move-down 0.75 'Done$))

(define-flowchart! hsdec.dia
  #:parameterize ([default-diaflow-arrow-label-inline? #false]
                  [default-diaflow-arrow-label-rotate? #true])
  [#:start-name "同音替换解密" #:background 'White
   #:node-desc #hasheq((initial-cipher-table! . "初始化明文符号表\nPlaintext")
                       (>>cipher . "读取密文\ncipher")
                       (okay? . "读取成功?")
                       (wrap . "环绕到最小密文 c")
                       (<<plain . "输出 Plaintext 中\nc 前一个位置处的字符"))] #:-
  (move-down 1 'initial-cipher-table!)
  (move-down 1 '>>cipher)
  (move-down 1 'okay?)
  (move-down 1 'wrap "Yes")
  (move-down 1 '<<plain)
  (move-down 0.75)
  (move-left 1.2)
  (L-step '>>cipher "下一个密文")

  (jump-to 'okay?)
  (move-right 1.2 #false "No")
  (move-down '<<plain)
  (move-down 1.0)
  (move-left '#:home)
  (move-down 0.75 'Done$))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  hsenc.dia
  hsdec.dia)
