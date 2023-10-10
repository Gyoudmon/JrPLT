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
; Packaging binaries and resources inside one repository might not be a good
;   idea as the SDL2 header files causes "typedef redefinition with different
;   types about the `size_t`" in macOS, in which we don't have to manage SDL2
;   header files manually.
; Here we choose to link to the base shared object directly for generating
;   additional shared objects, in which case the mentioned problem above
;   is avoided.

(define sdl2-config
  '((config SDL2)
    (include [windows "C:\\opt\\GYDMstem\\include"])
    (libpath [windows "C:\\opt\\GYDMstem\\lib"])))

(define sdl2+config
  '((config SDL2)
    (lib gydm.stem)

    [#:release
     (include "digitama/gydm_stem/compiled/native")
     (libpath "digitama/gydm_stem/compiled/native")]
    [#:debug
     (include "digitama/gydm_stem/compiled/native/debug")
     (libpath "digitama/gydm_stem/compiled/native/debug")]

    (include [windows "C:\\opt\\GYDMstem\\include"])
    (libpath [windows "C:\\opt\\GYDMstem\\lib"])))

(define native-compiled-subpath '())

(define native-launcher-names
  `(["digitama/gydm_stem/gydm.stem.cpp" so ,@sdl2-config]
    ["village/pltmos/pltmos.cpp" so ,@sdl2+config]
    ["village/scsmos/scsmos.cpp" so ,@sdl2+config]
    
    ["BigBang.cpp" console ,@sdl2+config]
    
    ["tamer/IME.cpp" console ,@sdl2-config]
    ["tamer/Tamer.cpp" console ,@sdl2-config]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define typesettings
  '(["literacy/TheBigBang.scrbl" xelatex]
    ["literacy/Metrics.scrbl" xelatex #:explicitly-make]

    ["literacy/Portfolios.scrbl" xelatex #px"Portfolios/*.(png|gv)"]
    ["literacy/portfolios/dingjiaqi.scrbl" "丁嘉琪" xelatex]
    ["literacy/portfolios/huajiaze.scrbl" "华佳泽" xelatex]
    ["literacy/portfolios/sungongcheng.scrbl" "孙功铖" xelatex]
    ["literacy/portfolios/wuchang.scrbl" "吴玚" xelatex]))
