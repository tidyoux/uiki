#lang racket

(require web-server/http/xexpr)

(include "config.rkt")

(provide
    response/xexpr/head
    response/xexpr/404)

; head
(define (response/xexpr/head
         #:title [title "no title"])
  `(head 
    (title ,title)
    (link
        ((rel "stylesheet") (href "http://localhost:8080/css/bootstrap.min.css")))
    (link
        ((rel "stylesheet") (href "http://localhost:8080/css/bootstrap-theme.min.css")))
    (link
        ((rel "stylesheet") (href "http://localhost:8080/css/main.css")))
    ))


; 404
(define (response/xexpr/404 req)
    (response/xexpr
        #:preamble #"<!DOCTYPE html>"
        #:code     404
        #:message  #"Not found"
        `(html
            ,(response/xexpr/head
                #:title "Not found")
            (body
                (p "Not found")))))
