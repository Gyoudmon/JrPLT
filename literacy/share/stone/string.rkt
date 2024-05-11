#lang typed/racket

(provide (all-defined-out) desc-font)

(require bitmap)
(require bitmap/constants)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-font : Font (desc-font #:family 'decorative #:size 24))
(define subfont-rate : Positive-Flonum 0.36)

(define char->string : (-> Char String)
  (lambda [ch]
    (cond [(eq? ch #\nul) "\\0"]
          [else (string ch)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-chars-array : (->* (String)
                                (Positive-Byte #:font Font #:gapsize Nonnegative-Real #:index (Option Byte) #:index-color Color #:fill Fill-Paint #:string? Boolean
                                               #:char->datum (-> Char Any))
                                Bitmap)
  (lambda [#:font [font default-font]  #:string? [string? #false] #:char->datum [->datum char->string]
           #:gapsize [gap -1.0] #:fill [fill-color #false] #:index [index 0] #:index-color [idx-color 'royalblue]
           src0 [max-length 1]]
    (define subfont : Font (desc-font #:family 'monospace #:size (* (font-size font) subfont-rate)))
    (define src : (Listof Char) (append (string->list src0) (if (not string?) null (list #\nul))))
    (define midx : Index (max max-length (length src)))
    (define gridsize : Flonum (* (real->double-flonum (font-size font)) 1.618))
    (define subsize : Flonum (real->double-flonum (font-size subfont)))
    (define grid : Bitmap (bitmap-square gridsize #:fill fill-color))
    (define subgrid : Bitmap (bitmap-square subsize #:border 'transparent))

    (bitmap-hc-append* #:gapsize gap
                       (let char->bitmap : (Listof Bitmap) ([cs : (Listof Char) src]
                                                            [idx : Nonnegative-Fixnum 0]
                                                            [spmb : (Listof Bitmap) null])
                         (cond [(>= idx midx) (reverse spmb)]
                               [(pair? cs)
                                (char->bitmap (cdr cs) (+ idx 1)
                                              (let* ([ch (car cs)]
                                                     [self (bitmap-cc-superimpose grid (bitmap-text (->datum ch) font))])
                                                (cons (cond [(or (not index) (eq? ch #\nul)) self]
                                                            [else (let ([idx.bmp (bitmap-text #:color idx-color (+ index idx) subfont)])
                                                                    (bitmap-cb-superimpose (bitmap-cc-superimpose subgrid idx.bmp)
                                                                                           self))])
                                                      spmb)))]
                               [else (char->bitmap null (+ idx 1) (cons grid spmb))])))))

(define bitmap-concatenate : (->* () (#:font Font) #:rest String Bitmap)
  (lambda [#:font [font default-font] . strs]
    (bitmap-hc-append* (append (add-between (for/list : (Listof Bitmap) ([s (in-list strs)])
                                              (make-chars-array s #:string? #true #:font font))
                                            (bitmap-text " + " font))
                               (list (bitmap-text " → " font)
                                     (make-chars-array #:string? #true #:font font
                                                       (apply string-append strs)))))))

(define bitmap-char-at : (->* (String Positive-Index) (#:string? Boolean #:font Font) Bitmap)
  (lambda [txt idx #:string? [string? #false] #:font [font default-font]]
    (define ghost (bitmap-square (* (font-size font) subfont-rate) #:border 'transparent))
    
    (bitmap-hc-append (make-chars-array txt #:string? string?)
                      (bitmap-text (format "[~a] → " idx) font)
                      (make-chars-array (string (string-ref txt idx)) #:index #false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ex:chars (make-chars-array "我是一个字符数组"))
(define ex:string (make-chars-array "我是一个字符串" #:string? #true))

(define cat (bitmap-concatenate "古典" "书屋"))
(define ref:chars (bitmap-char-at "青少计算机科学" 4))
(define ref:string (bitmap-char-at "青少计算思维" 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  ex:chars
  ex:string
  ref:chars
  ref:string)
