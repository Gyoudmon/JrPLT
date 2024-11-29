#lang info

(define collection "JrPLT")
(define pkg-desc "Computational Thinking Beyond C++, An Interdisciplinary Approach")

(define deps '("digimon" "pict"))
(define build-deps '("digimon" "scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '("WarGrey G. Ju"))
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

(define sdl2-config '((config SDL2)))

(define sdl2+config
  '((config SDL2)
    (lib plteen)
    
    (macro _USE_EXTERNAL_INCLUDING)
    
    [#:debug
     (libpath "digitama/plteen/compiled/native/debug/lib")]))

(define native-compiled-subpath '())
(define native-compiled-bindir '())
(define native-compiled-libdir '())
(define native-compiled-release '("release"))
(define native-compiled-debug '("debug"))

(define native-destination-drive "C:")
(define native-destination-subroot '("opt" "JrPLT"))
(define native-destination-release '())
(define native-destination-debug '("debug"))
(define native-destination-incdir '("include"))
(define native-destination-libdir '("lib"))

(define native-launcher-names
  `(["digitama/plteen/plteen.cpp" so ,@sdl2-config]
    ["village/pltmos/pltmos.cpp" so ,@sdl2+config]
    ["village/stemos/stemos.cpp" so ,@sdl2+config]

    ["BigBang.cpp" console ,@sdl2+config]
    ["JrPLT-CR.cpp" console ,@sdl2+config]

    ["tamer/IME.cpp" console ,@sdl2-config]
    ["tamer/Tamer.cpp" console ,@sdl2-config]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define typesettings
  '(["literacy/Linguisteen.scrbl" xelatex]
    ["literacy/HighOI.scrbl" xelatex]
    ["literacy/TheBigBang.scrbl" xelatex]
    ["literacy/HowToDesignPrograms.scrbl" xelatex]
    ["literacy/Introduction.scrbl" xelatex]
    ["literacy/AdventOfLambda.scrbl" xelatex "advent-of-lambda"]

    ["literacy/Metrics.scrbl" xelatex #:explicitly-make]))
