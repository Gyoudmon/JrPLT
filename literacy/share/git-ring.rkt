#lang typed/racket/base

(provide git-size-ring-chart)

(require digimon/format)
(require digimon/git)

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define git-size-ring-chart : (-> Real (HashTable Any (Git-Language-With Natural)) (Listof (Pairof String Color))
                                  [#:radian0 Real] [#:bytes-fx Real] [#:bytes-fy Real]
                                  [#:legend-font Font] [#:legend-border Color] [#:legend-brush Color]
                                  [#:label-color Color] [#:%-color Color] [#:total-color Color]
                                  Geo)
  (lambda [#:radian0 [radian0 0.0] #:bytes-fx [fx 0.5] #:bytes-fy [fy 0.618]
           #:legend-font [legend-font (desc-font #:family 'modern #:weight 'bold #:size 8)]
           #:legend-border [legend-pen 'GhostWhite] #:legend-brush [legend-brush (rgb* #xFFFFFF 0.618)]
           #:label-color [label-color 'black] #:%-color [%-color #x6a728f] #:total-color [total-color (rgb* 0 0.2)]
           flradius0 langsizes altcolors]
    (define 1em (font-metrics-ref legend-font 'em))
    (define radius (* flradius0 0.80)) ; we has trouble in generating proper pdf for latex renderer
    (define ring-thickness (* 1em 1.618))
    (define gapsize : Nonnegative-Flonum 2.0)
    
    (define sorted-langsizes ((inst sort (Git-Language-With Natural) Natural) #:key git-language-content (hash-values langsizes) >=))
    (define total (for/sum : Natural ([ds (in-list sorted-langsizes)]) (git-language-content ds)))
    
    (define sorted-infos : (Listof (List Color String Real))
      (for/list ([ds (in-list sorted-langsizes)])
        (list (language-rgba ds altcolors)
              (language-name (git-language-name ds))
              (/ (git-language-content ds) total))))
    
    (define lang-legends : (Listof (Listof Geo))
      (for/list ([info (in-list sorted-infos)])
        (list (geo-circle (* 1em 0.618 0.5) #:fill (car info) #:stroke #false)
              (geo-text (cadr info) legend-font #:color label-color)
              (geo-text (~% (caddr info) #:precision '(= 2)) legend-font #:color %-color))))

    (define lang-rings : Geo
      (let make-ring ([rings : (Listof Geo) null]
                      [infos : (Listof (List Color String Real)) sorted-infos]
                      [radian : Real radian0])
        (cond [(pair? infos)
               (let*-values ([(self rest) (values (car infos) (cdr infos))]
                             [(%) (caddr self)]
                             [(radian++) (+ radian (* 2pi %))])
                 (make-ring (cons (geo-arc #:stroke (desc-stroke #:width ring-thickness #:color (car self))
                                              radius radian radian++) rings)
                            rest radian++))]
              [(pair? rings) (geo-cc-superimpose* rings)]
              [else (geo-square (* radius 2.0) #:stroke #false #:fill #false)])))
    
    (define legend : Geo
      (geo-frame #:border legend-pen #:background legend-brush #:padding gapsize
                 (geo-table* lang-legends '(cc lc rc) '(cc) (list gapsize) (list gapsize))))
    
    (geo-pin-over (geo-pin* fx fy 0.5 0.5 lang-rings
                            (geo-text (~size total) legend-font #:color total-color))
                  gapsize gapsize
                  legend)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define language-rgba : (-> (Git-Language-With Natural) (Listof (Pairof String Color)) Color)
  (lambda [lang altcolors]
    (define maybe-color
      (or (assoc (language-name (git-language-name lang)) altcolors)
          (git-language-color lang)))
    
    (cond [(pair? maybe-color) (cdr maybe-color)]
          [(keyword? maybe-color) maybe-color]
          [else 'black])))

(define language-name : (-> String String)
  (lambda [lang]
    (define maybe-name-parts (regexp-match #px"(\\w+)\\s*[(]\\w+[)]" lang))
    (cond [(not maybe-name-parts) lang]
          [else (assert (cadr maybe-name-parts))])))
