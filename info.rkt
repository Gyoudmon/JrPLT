#lang info

(define collection "Jr.PLT")
(define pkg-desc "Programming Language Theory for Juniors, A Contrastive Approach")

(define deps '("digimon" "pict"))
(define build-deps '("digimon" "scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '("WarGrey Gyoudmon Ju"))
(define test-omit-paths 'all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sdl2-config
  '((config SDL2)
    (include [windows "C:\\opt\\vcso\\include"])
    (libpath [windows "C:\\opt\\vcso\\lib"])))

(define native-compiled-subpath '())

(define native-launcher-names
  `(["digitama/big-bang-cpp/gydm.stem.cpp" so ,sdl2-config]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define typesettings
  '(["TheBigBang.scrbl" xelatex]
    ["literacy/Metrics.scrbl" xelatex #:always-make]))
