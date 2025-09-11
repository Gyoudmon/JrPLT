#lang typed/racket/base

(require digimon/format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define size 100)
(define n (random 0 #xFFFFFF))

(define (x^2x^3x [x : Integer]) : Integer
  (bitwise-xor x (* 2 x) (* 3 x)))

(define (reason [x : Integer] [width : Integer]) : Void
  (define x^2x (bitwise-xor x (* 2 x)))
  (define 3x (* 3 x))
  
  (printf "// ~a ^ ~a ^ ~a = ~a ^ ~a = ~a~n"
          (~binstring x width)
          (~binstring (* 2 x) width)
          (~binstring 3x width)
          (~binstring x^2x width)
          (~binstring 3x width)
          (bitwise-xor x^2x 3x)))

(define (collect [n : Integer] [size : Integer]) : (Listof String)
  (let xor : (Listof String) ([x : Integer 0]
                              [i : Integer 0]
                              [rs : (Listof String) null])
    (if (and (<= x n) (< i size))
        (if (zero? (x^2x^3x x))
            (xor (+ x 1) (+ i 1) (cons (~binstring x) rs))
            (xor (+ x 1) i rs))
        (reverse rs))))

(define (count [n : Integer]) : Integer
  (let xor : Integer ([x : Integer 0]
                      [i : Integer 0])
    (if (<= x n)
        (if (zero? (x^2x^3x x))
            (xor (+ x 1) (+ i 1))
            (xor (+ x 1) i))
        i)))
n
(collect n size)

(reason #b00 4)
(reason #b01 4)
(reason #b10 4)
(reason #b11 4)
(reason #b100 4)

(cons (~binstring 80) (count 80))
(cons (~binstring 648276683) (count 648276683))
(cons (~binstring 390675405) (count 390675405))

(for ([n (in-range 32)])
  (printf "~a: ~a\n" (+ n 1) (count (arithmetic-shift 1 n))))
