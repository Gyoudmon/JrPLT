#lang racket/gui

(define argn (vector-length (current-command-line-arguments)))

(define html-client (new clipboard-client%))
(send html-client add-type "HTML")
(send the-clipboard set-clipboard-client html-client 0)

(let watch-clipboard ()
  (define datum (send the-clipboard get-clipboard-data "HTML" 0))

  (displayln datum)
  (flush-output (current-output-port))
  (send the-clipboard set-clipboard-string "" 0)
  (sleep 1)
  (watch-clipboard))