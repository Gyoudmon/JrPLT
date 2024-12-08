#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define swap-snapshots : Dia-Memory-Snapshots
  (parameterize ([default-memory-fixnum-radix 10]
                 [default-memory-padding-limit 16]
                 [default-memory-address-mask #xFFFFFFFF]
                 [default-memory-combine-datum? #true])
    (dia-memory-snapshots #:lookbehind-size 3 #:lookahead-size 3 #:optimize? #false
                          "swap.c")))

(define swap-memory : Geo (dia-memory-snapshots->table swap-snapshots #:gapsize 16.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  swap-memory)
