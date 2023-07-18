#lang info

(define collection "Jr.PLT")
(define pkg-desc "Programming Language Theory for Juniors, A Contrastive Approach")

(define deps '("digimon" "pict"))
(define build-deps '("digimon" "scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '("WarGrey Gyoudmon Ju"))
(define test-omit-paths 'all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING
; Package binaries and resources inside one repository might not be a good idea
;   as the SDL2 header files causes "typedef redefinition with different types
;   about the `size_t`" in macOS, in which we don't have to manage SDL2 header
;   files manually.
; Here we choose to link to the base shared object directly for generating
;   additional shared objects, in which case the mentioned problem above
;   is avoided.

(define sdl2-config
  '((config SDL2)
    (include [windows "C:\\opt\\vcso\\include"])
    (libpath [windows "C:\\opt\\vcso\\lib"])))

(define sdl2+config
  (append sdl2-config
          '((lib gydm.stem)
            (include "digitama/big-bang-cpp/compiled/native")
            (libpath "digitama/big-bang-cpp/compiled/native"))))

(define native-compiled-subpath '())

(define native-launcher-names
  `(["digitama/big-bang-cpp/gydm.stem.cpp" so ,sdl2-config]
    ["digitama/pltmos/pltmos.cpp" so ,sdl2+config]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define typesettings
  '(["TheBigBang.scrbl" xelatex]
    ["literacy/Metrics.scrbl" xelatex #:always-make]))
