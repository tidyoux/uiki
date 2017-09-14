#lang racket

(provide $)

; Shell interaction:
(define ($ command)
  ; run a shell command, then
  ; print out stdout, stderr as needed:
  (match (process command)
    [`(,stdout ,stdin ,exit ,stderr ,proc)
     (define cmd-out (port->string stdout))
     (define cmd-err (port->string stderr))
     
     (printf "$ command~n~a~n" cmd-out)
     
     (when (not (equal? cmd-err ""))
       (printf "~nerror:~n~a~n" cmd-err))]))