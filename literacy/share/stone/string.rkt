#lang typed/racket

(provide (all-defined-out) desc-font)

(require geofun/vector)

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
                                Geo)
  (lambda [#:font [font default-font]  #:string? [string? #false] #:char->datum [->datum char->string]
           #:index [index 0] #:gapsize [gap -1.0] #:fill [fill-color #false] #:index-color [idx-color 'royalblue]
           src0 [max-length 1]]
    (define subfont : Font (desc-font #:family 'monospace #:size (* (font-size font) subfont-rate)))
    (define src : (Listof Char) (append (string->list src0) (if (not string?) null (list #\nul))))
    (define midx : Index (max max-length (length src)))
    (define gridsize : Flonum (* (real->double-flonum (font-size font)) 1.618))
    (define subsize : Flonum (real->double-flonum (font-size subfont)))
    (define grid : Geo (geo-square gridsize #:fill fill-color))
    (define subgrid : Geo (geo-square subsize #:stroke 'transparent))

    (geo-hc-append* #:gapsize gap
                       (let char->geo : (Listof Geo) ([cs : (Listof Char) src]
                                                            [idx : Nonnegative-Fixnum 0]
                                                            [spmb : (Listof Geo) null])
                         (cond [(>= idx midx) (reverse spmb)]
                               [(pair? cs)
                                (char->geo (cdr cs) (+ idx 1)
                                              (let* ([ch (car cs)]
                                                     [self (geo-cc-superimpose grid (geo-text (->datum ch) font))])
                                                (cons (cond [(or (not index) (>= idx (string-length src0))) self]
                                                            [else (let ([idx.bmp (geo-text #:color idx-color (+ index idx) subfont)])
                                                                    (geo-cb-superimpose (geo-cc-superimpose subgrid idx.bmp)
                                                                                           self))])
                                                      spmb)))]
                               [else (char->geo null (+ idx 1) (cons grid spmb))])))))

(define geo-concatenate : (->* () (#:font Font) #:rest String Geo)
  (lambda [#:font [font default-font] . strs]
    (geo-hc-append* (append (add-between (for/list : (Listof Geo) ([s (in-list strs)])
                                              (make-chars-array s #:string? #true #:font font))
                                            (geo-text " + " font))
                               (list (geo-text " → " font)
                                     (make-chars-array #:string? #true #:font font
                                                       (apply string-append strs)))))))

(define geo-char-at : (->* (String Positive-Index) (#:string? Boolean #:font Font) Geo)
  (lambda [txt idx #:string? [string? #false] #:font [font default-font]]
    (define ghost (geo-square (* (font-size font) subfont-rate) #:stroke 'transparent))
    
    (geo-hc-append (make-chars-array txt #:string? string?)
                   (geo-text (format "[~a] → " idx) font)
                   (make-chars-array (string (string-ref txt idx)) #:index #false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ex-chars (make-chars-array "我是一个字符数组"))
(define ex-string (make-chars-array "我是一个字符串" #:string? #true))

(define cat (geo-concatenate "古典" "书屋"))
(define ref-chars (geo-char-at "青少计算机科学" 4))
(define ref-string (geo-char-at "青少计算思维" 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  ex-chars
  ex-string
  cat
  ref-chars
  ref-string)
