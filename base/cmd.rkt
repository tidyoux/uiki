#lang racket

(include "config.rkt")

(provide $)

; Shell interaction:
(define ($ command)
    ; run a shell command, then
    ; print out stdout, stderr as needed:
    (match (process command)
        [`(,stdout ,stdin ,exit ,stderr ,proc)
        (define cmd-out (port->string stdout))
        (define cmd-err (port->string stderr))

        (when (not (equal? cmd-err ""))
            (printf "~nerror:~n~a~n" cmd-err))

        (when debug
            (printf "$ command: ~a~n~a~n" command cmd-out))

        (close-input-port stdout)
        (close-output-port stdin)
        (close-input-port stderr)

        cmd-out]))
