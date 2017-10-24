#lang racket

(require web-server/http/xexpr)

(include "config.rkt")

(provide
    response/xexpr/head
    response/xexpr/edit/head
    response/xexpr/404)

; head
(define (response/xexpr/head
         #:title [title "no title"])
  `(head 
    (title ,title)
    (link
        ((rel "stylesheet") (href "../css/mdui.min.css")))
    (link
        ((rel "stylesheet") (href "../css/main.css")))
    (script
        ((src "../js/mdui.min.js")))
    (script
        ((src "../js/main.js")))
    ))

(define (response/xexpr/edit/head
         #:title [title "no title"])
  `(head 
    (title ,title)
    (link
        ((rel "stylesheet") (href "../../css/mdui.min.css")))
    (link
        ((rel "stylesheet") (href "../../css/main.css")))
    (script
        ((src "../../js/mdui.min.js")))
    (script
        ((src "../../js/main.js")))
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
