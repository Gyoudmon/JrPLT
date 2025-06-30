#lang typed/racket/base

(require racket/list)
(require digimon/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define find-missing-numbers : (-> Index (Listof Integer) (Option Integer))
  (lambda [N numbers]
    (define size (quotient (+ N 7) 8))
    (define bitmap : Bytes (make-bytes size 0))

    (for ([n (in-list numbers)])
      (define-values (pos tail) (quotient/remainder (sub1 n) 8))
      (define idx (- size pos 1))
      (bytes-set! bitmap idx
                  (bitwise-ior (bytes-ref bitmap idx)
                               (arithmetic-shift 1 tail))))

    (when (< N 100)
      (displayln (~binstring bitmap)))
    
    (let search ([idx : Integer (sub1 size)])
      (and (>= idx 0)
           (let ([b (bytes-ref bitmap idx)])
             (cond [(eq? b #xFF) (search (sub1 idx))]
                   [else (for/or : (Option Integer) ([jdx (in-range 8)])
                           (and (zero? (bitwise-and b (arithmetic-shift 1 jdx)))
                                (+ (* (- size idx 1) 8) jdx 1)))]))))))

(define find-missing-numbers* : (-> Index (Option Integer))
  (lambda [n]
    (define missing (add1 (random n)))
    (define numbers (shuffle (build-list n add1)))

    (when (< n 100)
      (printf "~a: [~a] + ~a\n" n (car numbers) (cdr numbers)))
    
    (when (not (eq? (car numbers)
                    (find-missing-numbers n (cdr numbers))))
      (raise-user-error "wrong"))
    
    (car numbers)))

(find-missing-numbers 3 (list 1 3))
(find-missing-numbers* 29)
(find-missing-numbers* 32)
(find-missing-numbers* 1000)
(find-missing-numbers* 100000)
(find-missing-numbers* 10000000)
