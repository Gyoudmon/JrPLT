#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define swap-snapshots : Dia-Memory-Snapshots
  (dia-memory-snapshots "/Users/wargrey/Laboratory/JrPLT/tamer/ffi/memory.c" #:entry 'main
                        (λ [[snapshots : Dia-Reversed-Variables] [segment : Symbol] [state : String]]
                          (dia-memory-snapshot snapshots segment state #:no-binary-datum? #true #:integer-base 16 #:no-padding? #false))))

(define swap-memory : Geo (dia-memory-snapshots->table swap-snapshots #:gapsize 8.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  swap-memory)
