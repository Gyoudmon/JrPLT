#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define artfont (desc-font (default-art-font) #:family 'fantasy))

(define aoc-margin-figure-separator (geo-hline 240.0 8.0 #:stroke 'LightGrey))

(define aoc-assignment-desc : (-> Symbol (-> Any String))
  (lambda [a]
    (λ [[v : Any]] : String
      (format "~s" v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-art-text : (-> Any Geo)
  (lambda [v]
    (geo-scale (geo-art-text v artfont #:stroke 'DimGray) 2.5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-extract-flow-connect-from-info : (-> Any (Values (Option String) Symbol))
  (lambda [v]
    (if (pair? v)
        (values (format "~a" (car v)) (string->symbol (format "&~a" (cdr v))))
        (values #false (string->symbol (format "&~a" v))))))

(define aoc-extract-flow-connect-to-info : (-> Any (Values (Option String) Symbol))
  (lambda [v]
    (if (pair? v)
        (values (format "~a" (car v)) (string->symbol (format "&~a." (cdr v))))
        (values #false (string->symbol (format "&~a." v))))))

