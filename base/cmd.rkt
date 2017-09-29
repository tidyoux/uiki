#lang racket

(include "config.rkt")

(provide $)

; Shell interaction:
(define ($ . commands)
    ; run a shell command, then
    ; print out stdout, stderr as needed:
    (define command (string-join commands))
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
