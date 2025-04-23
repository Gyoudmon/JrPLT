#lang typed/racket/base

(provide (all-defined-out))

(require diafun/class)
(require geofun/markup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  #:start 'Cosmos [#:background 'White] #:-
  (move-down 1.5 'Plane (gpmult 1 '0..n 'aggregation))
  (move-down 1.5 'Matter (gpmult 1 '0..n 'composition))
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
  #:start 'Cosmos [#:background 'White] #:-
  (move-up 2.0 'Universe)
  (move-left 2.5 '#:IDisplay)

  (jump-to 'Cosmos)
  (move-left-up   2 0.75 'OnionSkin (gpmult 1 1 'composition))
  (move-down-left 2 0.75 '#:IScreen)
  (jump-to 'Cosmos)
  (move-left-down 2 0.75 'LinkedPlaneInfo (gpmult 1 1 'composition))
  (move-down 1.75 '#:IPlaneInfo)
  (move-to '#:IScreen #false (gpmult 1 1 'aggregation))

  (jump-to 'Cosmos)
  (move-down '#:IPlaneInfo 'Plane (gpmult 1 '0..n 'aggregation))
  (move-to '#:IPlaneInfo #false "<<use>>")

  (jump-to 'Plane)
  (move-down 2.5 'Matter (gpmult 1 '0..n 'composition))
  (jump-left '#:IPlaneInfo 'Continent)
  (move-left '#:IScreen #false (gpmult 1 #false))
  (move-up '#:IPlaneInfo 'Pasteboard (gpmult #false 1 'composition))
  (move-up '#:IScreen)
  (jump-to 'Continent) (move-to '#:IPlaneInfo #false (gpmult 1 1 'composition))
  (jump-to 'Continent) (move-to 'Plane #false (gpmult 1 1 'aggregation))
  (jump-to 'Continent) (move-to 'Matter)
  (move-right 2.5 '#:IMovable)

  (jump-to 'Plane)
  (move-right 2.5 'MatterInfo (gpmult 1 1 'composition))
  (move-down 1.5 '#:IMatterInfo)
  (move-to 'Plane #false (gpmult 1 1 'aggregation))
  (jump-to 'Matter)
  (move-to '#:IMatterInfo #false "<<use>>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  visobj-mod.dia
  cosmos.dia)
