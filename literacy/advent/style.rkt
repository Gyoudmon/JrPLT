#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)

(require diafun/digitama/avatar/procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define artfont (desc-font (default-art-font) #:family 'fantasy))
(define monofont (desc-font (default-art-font) #:family 'monospace))

(define aoc-inner-body-fill : Color (rgb* 'LightBlue))
(define aoc-outer-body-fill : Color (rgb* 'Lavender))
(define aoc-inner-iofill : Dia-Procedure-IO-Fill (λ [v t] 'AliceBlue))

(define aoc-margin-figure-separator (geo-hline 240.0 8.0 #:stroke 'LightGrey))

(define aoc-assignment-desc : (-> Symbol (-> Any String))
  (lambda [a]
    (λ [[v : Any]] : String
      (format "~s" v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-text : (-> Any Geo)
  (lambda [v]
    (geo-scale (geo-text #:id (aoc-text-id v) #:color 'Black #:alignment 'center
                         (aoc-text-datum v) monofont)
               2.5)))

(define aoc-art-text : (-> Any Geo)
  (lambda [v]
    (geo-scale (geo-art-text #:id (aoc-text-id v) #:stroke 'Black #:alignment 'center
                             (aoc-text-datum v) artfont)
               2.5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-text-id : (-> Any Symbol)
  (lambda [v]
    (cond [(symbol? v) v]
          [(string? v) (string->symbol v)]
          [(procedure? v) (assert (object-name v) symbol?)]
          [else (string->symbol (format "~a" v))])))

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

