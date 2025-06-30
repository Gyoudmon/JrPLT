#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require geofun/vector)
(require diafun/digitama/avatar/procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define string-square.dia
  (dia-procedure '|>_| #(length char) #(square) #(4 #\+)
                 (geo-vc-append* #:gapsize 2.0
                                 (build-list 4 (λ [[n : Index]]
                                                 (geo-trim (geo-text (make-string 4 #\+)
                                                                     (default-procedure-datum-font))))))))

(define string-triangle.dia
  (dia-procedure '|>_| #(height char) #(triangle) #(4 #\^)
                 (geo-vc-append* (build-list 4 (λ [[n : Index]]
                                                 (geo-text (make-string (+ (* 2 n) 1) #\^)
                                                           (default-procedure-datum-font)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  string-square.dia
  string-triangle.dia)
