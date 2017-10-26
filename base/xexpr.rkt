#lang racket

(require web-server/http/xexpr)

(include "config.rkt")

(provide
    response/xexpr/html
    response/xexpr/head
    response/xexpr/edit/head

    response/xexpr/list/body
    response/xexpr/view-not-exist/body
    response/xexpr/edit/body

    response/xexpr/jump2list
    response/xexpr/404)


; common response xexpr
(define (response/xexpr/html head body)
    (response/xexpr
        #:preamble #"<!DOCTYPE html>"
        `(html
            ,head
            ,body)))

; common head
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

; edit head
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

; list body
(define (response/xexpr/list/body items)
    `(body ((class "mdui-theme-primary-indigo mdui-theme-accent-deep-orange mdui-color-grey-200"))
        (div ((class "mdui-container mdui-typo"))
            (h1 "wiki:")

            (form ((method "POST") (action ,(string-append "/wiki/" (~a (current-milliseconds)))))
                (input ((type "hidden") (name "content") (value "# This is the title\n\n")))
                (input ((class "mdui-fab mdui-fab-fixed mdui-color-theme") (type "submit") (value "+"))))

            (div ,@items))))

; view page not exist
(define (response/xexpr/view-not-exist/body page)
    `(body ((class "mdui-theme-primary-indigo mdui-theme-accent-deep-orange"))
        (div ((class "mdui-container mdui-typo mdui-m-t-1"))
            (a ((class "mdui-btn mdui-btn-icon mdui-ripple") (href "/wiki/"))
                (i ((class "mdui-icon material-icons"))
                    "home"))
            (div ((class "mdui-divider mdui-m-t-1 mdui-m-b-1")))
            (p "Page does not exist")
            (form ([method "POST"] [action ,(string-append "/wiki/" page)])
                (input ([type "hidden"] [name "content"] [value "# This is the title\n\n"]))
                (input ([class "mdui-btn mdui-btn-raised mdui-color-theme"] [type "submit"] [value "Create page"]))))))

; edit body
(define (response/xexpr/edit/body md-file-path page)
    `(body ((class "mdui-theme-primary-indigo mdui-theme-accent-deep-orange"))
        (div ((class "mdui-container mdui-typo mdui-m-t-1"))
            (form ((method "POST") (action ,(string-append "/wiki/" page)))
                (div ((class "mdui-row mdui-m-b-2"))
                    (div ((class "mdui-col-xs-12"))
                        (textarea ((class "mdui-col-xs-12") (id "content") (name "content"))
                            ,(file->string md-file-path))))
                (input ((class "mdui-btn mdui-btn-raised mdui-ripple mdui-color-theme") (type "submit") (value "submit")))
                (a ((class "mdui-btn mdui-ripple mdui-m-l-3") (href ,(string-append "/wiki/" page)))
                    "cancel")))))

; jump to list page
(define (response/xexpr/jump2list)
    (response/xexpr/html
        `(head
            (meta ((http-equiv "refresh") (content "0.1;url=/wiki/"))))
        ""))

; 404
(define (response/xexpr/404 req)
    (response/xexpr
        #:preamble #"<!DOCTYPE html>"
        #:code     404
        #:message  #"Not found"
        `(html
            (head
                (title "Not found"))
            (body
                (p "Not found")))))
