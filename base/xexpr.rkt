#lang racket

(require web-server/http/xexpr)

(include "config.rkt")

(provide
    response/xexpr/head
    response/xexpr/404)

; head
(define (response/xexpr/head
         #:title [title "no title"]
         #:style [style ""])
  `(head 
    (title ,title)
    (style 
     ,(string-append default-style "\n" style))))

; 404
(define (response/xexpr/404)
    (response/xexpr
        #:preamble #"<!DOCTYPE html>"
        #:code     404
        #:message  #"Not found"
        `(html
            ,(response/xexpr/head
                #:title "Not found")
            (body
                (p "Not found")))))
