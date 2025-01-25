#lang typed/racket/base

(provide (all-defined-out))

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

