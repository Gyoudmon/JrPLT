#lang typed/racket/base

(provide (all-defined-out))

(require diafun/class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define class-type : DiaCls-Association-Identifier
  (lambda [src tgt]
    (case src
      [(Cosmos)
       (cond [(memq tgt '(IScreen LinkedPlaneInfo)) 'composition]
             [(memq tgt '(Plane)) 'aggregation])]
      [(Plane)  (and (memq tgt '(Matter MatterInfo IPlaneInfo)) 'composition)]
      [(Matter) (and (memq tgt '(IMatterInfo)) 'composition)]
      [(Group)  (cond [(memq tgt '(IScreen)) 'composition]
                      [(memq tgt '(Plane)) 'aggregation])]
      [(IMatterInfo IPlaneInfo) 'aggregation]
      [(Continent) (cond [(memq tgt '(IScreen IPlaneInfo)) 'composition]
                         [(memq tgt '(Plane)) 'aggregation])]
      [(MatterInfo) (and (memq tgt '(Plane)) 'aggregation)])))

(define class-color : (Dia-Path-Node-Style-Make DiaCls-Class-Style)
  (lambda [anchor hint]
    (cond [(memq anchor '(Sprite Atlas Tracklet Continent Dimensionlet))
           (make-diacls-class-style #:fill-paint 'RoyalBlue)]
          [(memq anchor '(IPlotlet IShapelet ICanvaslet ITextlet IValuelet))
           (make-diacls-class-style #:fill-paint 'DarkKhaki)]
          [else (make-diacls-class-style)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-simple-class! visobj-mod.dia
  #:parameterize ([default-diacls-class-style-make class-color])
  #:start 'Cosmos [#:background 'White #:relationship class-type] #:-
  (move-down 1.5 'Plane (cons "1" "0..n"))
  (move-down 1.5 'Matter (cons "1" "0..n"))
  (radial-back 2.5 150.0 'IGraphlet)
  (radial-back 2.0 90.0 'Sprite)
  (radial-back 2.5 30.0 'Atlas)

  (jump-to 'IGraphlet)
  (radial-back 2.0 90.0 'ITextlet)
  (jump-left-down  1.5 2.0 'IValuelet)  (move-to 'IGraphlet)
  (jump-right-down 1.5 2.0 'ICanvaslet) (move-to 'IGraphlet)
  (jump-left-down 0.75 4.0 'Dimensionlet)
  (move-to 'IValuelet) (jump-to 'Dimensionlet)
  (move-to 'ITextlet)

  (jump-to 'ICanvaslet)
  (radial-back 2.0 90.0 'Tracklet)
  (radial-back 2.0 45.0 'IPlotlet)
  (radial-back 2.0 00.0 'IShapelet))

(define-simple-class! cosmos.dia
  #:parameterize ([default-diacls-class-style-make class-color])
  #:start 'Cosmos [#:background 'White #:relationship class-type] #:-
  (radial-move 3.5 180.0 '#:IScreen (cons "1" "1"))
  (radial-move 2 155.0 'LinkedPlaneInfo (cons "1" "1"))
  (move-up 2.0 'Universe)
  (move-left 2.5 '#:IDisplay)

  (jump-to 'LinkedPlaneInfo)
  (move-down 1.75 '#:IPlaneInfo)
  (move-to '#:IScreen #false (cons "1" "1"))

  (jump-to 'Cosmos)
  (move-down '#:IPlaneInfo 'Plane (cons "1" "0..n"))
  (move-to '#:IPlaneInfo #false "uses")

  (jump-to 'Plane)
  (move-down 2.5 'Matter (cons "1" "0..n"))
  (jump-left '#:IPlaneInfo 'Continent)
  (T-step '#:IScreen (cons "1" #false) (cons #false "1"))
  (jump-to 'Continent) (move-to '#:IPlaneInfo #false (cons "1" "1"))
  (jump-to 'Continent) (move-to 'Plane #false (cons "1" "1"))
  (jump-to 'Continent) (move-to 'Matter)
  (move-right 2.5 '#:IMovable)

  (jump-to 'Plane)
  (move-right 2.5 'MatterInfo (cons "1" "1"))
  (move-down 1.5 '#:IMatterInfo)
  (move-to 'Plane #false (cons "1" "1"))
  (jump-to 'Matter)
  (move-to '#:IMatterInfo #false "uses"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  visobj-mod.dia
  cosmos.dia)
