#lang racket

(require web-server/http/xexpr)

(include "config.rkt")

(provide
    response/xexpr/html

    response/xexpr/head
    response/xexpr/view/head
    response/xexpr/edit/head

    response/xexpr/list/item
    response/xexpr/list/body
    response/xexpr/view-not-exist/body
    response/xexpr/view-menu-bar
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

; view head
(define (response/xexpr/view/head
         #:title [title "no title"])
    `(head 
        (title ,title)
        (link
            ((rel "stylesheet") (href "../css/mdui.min.css")))
        (link
            ((rel "stylesheet") (href "../css/main.css")))
        (script
            ((src "https://cdn.bootcss.com/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML")))
        (script
            ((src "../js/mdui.min.js")))
        (script
            ((src "../js/run_prettify.js")))
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

; list item
(define (response/xexpr/list/item pathstr modify-date-str title)
    `((div ((class "mdui-card mdui-m-b-2 mdui-hoverable"))
        (div ((class "mdui-card-content"))
            (a ((class "mdui-typo-title mdui-valign") (href ,(string-append "/wiki/" pathstr)))
                (i ((class "mdui-icon material-icons mdui-m-r-1"))
                    "description")
                ,(if title
                    (second title)
                    pathstr))
            (div ((class "mdui-divider mdui-m-t-2")))
            (div ((class "mdui-typo-caption-opacity"))
                ,modify-date-str)))))

; list body
(define (response/xexpr/list/body user items)
    `(body ((class "mdui-theme-primary-indigo mdui-theme-accent-deep-orange mdui-color-grey-200"))
        (div ((class "mdui-container mdui-typo"))
            (h1 ,(string-append user "'s wiki:"))

            (div ,@items)

            (form ((method "POST") (action ,(string-append "/wiki/" (~a (current-milliseconds)))))
                (input ((type "hidden") (name "content") (value "# This is the title\n\n")))
                (input ((class "mdui-fab mdui-fab-fixed mdui-color-theme") (type "submit") (value "+"))))))) 

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

; view menu bar
(define (response/xexpr/view-menu-bar page)
    `(div
        (p
            (a ((class "mdui-btn mdui-btn-icon mdui-ripple") (href "/wiki/"))
                (i ((class "mdui-icon material-icons"))
                    "home"))
            (a ((class "mdui-btn mdui-btn-icon mdui-ripple") (href ,(string-append "/wiki/" page "/edit")))
                (i ((class "mdui-icon material-icons"))
                    "edit"))
            (a ((class "mdui-btn mdui-btn-icon mdui-ripple mdui-float-right") (mdui-dialog "{target: '#deleteConfirm'}"))
                (i ((class "mdui-icon material-icons"))
                    "delete")))

        (div ((class "mdui-dialog") (id "deleteConfirm"))
            (div ((class "mdui-dialog-title"))
                "Delete file?")
            (div ((class "mdui-dialog-actions"))
                (button ((class "mdui-btn mdui-ripple") (mdui-dialog-close ""))
                    "cancel")
                (a ((class "mdui-btn mdui-ripple") (href ,(string-append "/wiki/" page "/delete")))
                    "delete")))
                    
        (div ((class "mdui-divider")))))

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
