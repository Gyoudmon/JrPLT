#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pointer-snapshots : Dia-Memory-Snapshots
  (parameterize ([default-memory-fixnum-radix 10]
                 [default-memory-padding-limit 16]
                 [default-memory-combine-datum? #true])
    (dia-memory-snapshots #:optimize? #false "pointer.c")))

(define pointer-memory : Geo (dia-memory-snapshots->table pointer-snapshots #:gapsize 16.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  pointer-memory)
