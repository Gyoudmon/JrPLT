#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require geofun/vector)
(require diafun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING: this affects all diagrams of flowchart
(default-diaflow-storage-block-width  64.0)
(default-diaflow-storage-block-height 50.0)
(default-diaflow-storage-font (desc-font #:family 'math #:size 'xx-large))
(default-diaflow-storage-arrow-font (desc-font #:family 'math #:size 'large))

(define aoc-blank : Geo (geo-blank))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-dataflow-node-construct : DiaFlow-Id->Node-Shape
  (lambda [master id label style width height direction hint]
    (case/eq (object-name style)
             [(diaflow-process-style) (aoc-block-process id label style width height direction hint)]
             [(diaflow-storage-style) (when (not hint) (aoc-block-dataIO  id label style width height direction hint))])))

(define aoc-dataflow-arrow-identify : DiaFlow-Arrow-Identifier
  (lambda [source target labels]
    (dia-edge-style-construct source target labels (default-diaflow-storage-arrow-style-make) make-diaflow-storage-arrow-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-block-process : DiaFlow-Block-Create
  (lambda [id label style width height direction hint]
    (define fbox : Geo
      (geo-sandglass #:fill (dia-node-select-fill-paint style)
                     #:stroke (dia-node-select-stroke-paint style)
                     #:neck-width (* width 0.16) #:neck-height (* width 0.05)
                     (* width 0.25)))
    
    (if (or (not direction) (not (zero? direction)))
        (create-dia-node #:id id #:fit-ratio 0.95 0.4 #:position 0.5 0.25 fbox label)
        (create-dia-node #:id id (geo-rotate fbox (- direction (* pi 0.5))) label))))

(define aoc-block-dataIO : DiaFlow-Block-Create
  (lambda [id label style width height direction hint]
    (create-dia-node #:id id #:type 'Storage #false
                     #:fit-ratio 1.05 1.05 #:position 0.5 0.5
                     aoc-blank #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-extract-flow-connector-info : (-> Any (Values (Option String) Symbol))
  (lambda [v]
    (if (pair? v)
        (values (format "~a" (car v)) (string->symbol (format "&~a." (cdr v))))
        (values #false (string->symbol (format "&~a." v))))))
