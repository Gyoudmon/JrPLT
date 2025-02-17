#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define artfont (desc-font (default-art-font) #:family 'fantasy))
(define monofont (desc-font (default-art-font) #:family 'monospace))

(define aoc-margin-figure-separator (geo-hline 240.0 8.0 #:stroke 'LightGrey))

(define aoc-assignment-desc : (-> Symbol (-> Any String))
  (lambda [a]
    (λ [[v : Any]] : String
      (format "~s" v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-text : (-> Any Geo)
  (lambda [v]
    (geo-scale (geo-text (aoc-text-datum v) monofont #:color 'DimGray #:alignment 'center) 2.5)))

(define aoc-art-text : (-> Any Geo)
  (lambda [v]
    (geo-scale (geo-art-text (aoc-text-datum v) artfont #:stroke 'DimGray #:alignment 'center) 2.5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-text-datum : (-> Any String)
  (lambda [v]
    (cond [(symbol? v) (symbol->string v)]
          [(procedure? v) (format "~a" (object-name v))]
          [else (format "~s" v)])))

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

