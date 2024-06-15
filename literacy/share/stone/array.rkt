#lang typed/racket/base

(require racket/list)

(require bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type ArraySection (List (-> Integer Integer Integer) Integer Color))

(define default-font : Font (desc-font #:family 'decorative #:size 48))
(define default-border : Stroke (desc-stroke #:width 2.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-cells : (-> (Listof Integer) Font Real Fill-Paint (Listof (Option Bitmap)))
  (lambda [ns font size color]
    (cons #false (for/list : (Listof Bitmap) ([n (in-list ns)])
                   (bitmap-cc-superimpose (bitmap-text n font #:color color)
                                          (bitmap-square size #:border default-border))))))

(define bitmap-array-section : (-> ArraySection Real Font Color Bitmap)
  (lambda [section size font text-color]
    (define-values (infix operand color) (values (car section) (cadr section) (caddr section)))
    (define border (desc-stroke default-border #:color 'whitesmoke))
    
    (bitmap-pin* 0.5 0.25 0.5 0.5
                 (bitmap-sandglass size #:neck-width -0.32 #:neck-height -0.0 #:fill color #:border border)
                 (bitmap-text (format "(~a~a)" (object-name infix) operand) font #:color text-color))))

(define do-array-section : (-> (Listof Integer) ArraySection Font Font Real (Values (Listof Integer) (Listof (Listof (Option Bitmap)))))
  (lambda [ns section font subfont size]
    (define-values (infix operand color) (values (car section) (cadr section) (caddr section)))
    (define arrow (bitmap-arrow 8.0 (* size 1.618) pi/2 #:fill color))
    (define ns++ (map (λ [[n : Integer]] (infix n operand)) ns))
    
    (values ns++
            (list (cons (bitmap-array-section section size subfont 'ghostwhite)
                        (make-list (length ns) arrow))
                  (bitmap-cells ns++ font size color)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-array-map-flow : (->* ((Listof Integer))
                                   ((Listof ArraySection) Font #:row-gapsize Nonnegative-Real)
                                   Bitmap)
  (lambda [ns [sections null] [font default-font] #:row-gapsize [gapsize 8.0]]
    (define subfont : Font (desc-font font #:size -0.618))
    (define cell-size (* (font-metrics-ref font 'em) 1.618))
    (define-values (cells _)
      (for/fold ([cells : (Listof (Listof (Option Bitmap))) (list (bitmap-cells ns font cell-size 'black))]
                 [data : (Listof Integer) ns])
                ([sec (in-list sections)])
       (define-values (ns++ self) (do-array-section data sec font subfont cell-size))
       (values (append cells self)
               ns++)))
    
    (bitmap-table* cells 'cc 'cc (list gapsize (- (stroke-width default-border))) gapsize)))

(define make-array-sections-merge-flow : (->* ((Listof ArraySection)) (Font #:gapsize Nonnegative-Real) Bitmap)
  (lambda [ss [font default-font] #:gapsize [gapsize 8.0]]
    (define em : Flonum (font-metrics-ref font 'em))
    (define size : Flonum (* em 1.618 1.618))
    (define final-size : Flonum (* size 1.618))
    (define operator : Bitmap (bitmap-circle 8.0 #:border default-border))
    (define arrow : Bitmap (bitmap-arrow 12.0 em))

    (define-values (sections operand r g b)
      (for/fold ([snoitces : (Listof Bitmap) null]
                 [operand : Integer 0]
                 [r : Flonum 0.0] [g : Flonum 0.0] [b : Flonum 0.0])
                ([s (in-list ss)])
        (define c (rgb* (caddr s)))
        (values (cons (bitmap-array-section s size font 'ghostwhite) snoitces)
                ((car s) operand (cadr s))
                (+ r (rgba-red c))
                (+ g (rgba-green c))
                (+ b (rgba-blue c)))))

    (define final-section : ArraySection
      (if (< operand 0)
          (list - (abs operand) (rgb* r g b))
          (list + operand (rgb* r g b))))
    
    (bitmap-hc-append* #:gapsize gapsize
                       (append (add-between sections operator)
                               (list arrow (bitmap-scale (bitmap-vc-append* (reverse sections)) 0.618)
                                     arrow (bitmap-array-section final-section final-size font 'crimson))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define array-sections : (Listof ArraySection)
  (list (list + 1 'royalblue)
        (list - 4 'orange)
        (list + 2 'green)))

(define array-map (make-array-map-flow (list 1 2 1 4 5 10 5 16 8) array-sections))
(define array-section (make-array-sections-merge-flow array-sections))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  array-map
  array-section)
