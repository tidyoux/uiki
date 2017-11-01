#lang racket

; Uiki: A simple academic wiki.
; Copyright (C) 2015 Matthew Might

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.


; DEPENDENCIES:

; + racket 6.1.1+ (earlier versions might work)
; + git
; + multimarkdown
; + htpasswd (bundled with apache)

; FEATURES:

; + Markdown support via multimarkdown
; + LaTeX support via MathJax
; + Syntax highlighting support via code-prettify
; + Backup/versioning support via git


(require
    web-server/servlet
    web-server/servlet-env
    xml
    net/uri-codec
    (only-in 2htdp/batch-io
        read-file
        write-file))

(require
    "base/cmd.rkt"
    "base/xexpr.rkt"
    "base/git.rkt"
    "base/utils.rkt"
    "base/auth.rkt")

; Import configuration variables:
(include "base/config.rkt")

; list wiki
(define (list-wiki req)
    (define user (auth-user req))
    (define dir (string->path (database-dir user)))
    (define items
        (if (directory-exists? dir)
            (foldl
                (lambda (path result)
                    (define md-file-path (build-path dir path "content.md"))
                    (if (file-exists? md-file-path) ; when path is directory, not a file.
                        (let* ((pathstr (path->string path))
                                (modify-date (seconds->date (file-or-directory-modify-seconds
                                                                md-file-path
                                                                #f
                                                                (lambda () 0))))
                                (title (regexp-match #px"#(.*?)\n"
                                            (read-file (string-append
                                                            (database-page-dir user pathstr)
                                                            "/content.md")))))

                            (append (response/xexpr/list/item
                                        pathstr
                                        (format-date modify-date "year-month-day hour:minute")
                                        title)
                                result))
                        result))
                '()
                (directory-list dir))
            '()))

    (response/xexpr/html
        (response/xexpr/head
            #:title "wiki list")
        (response/xexpr/list/body user items)))

        
; make wiki view
(define (make-wiki-view md-file-path client-out head
            #:message [message #f]
            #:menu-bar [menu-bar #f])
            
    ; render the top:
    (write-bytes #"<!DOCTYPE>\n<html>\n" client-out)
    
    ; render the header:
    (write-string (xexpr->string head) client-out)
    
    ; render the body:
    (write-bytes #"<body class=\"mdui-theme-primary-indigo mdui-theme-accent-deep-orange\">" client-out)
    
    ; enable MathJax for LaTeX support:
    (write-bytes #"<script type=\"text/x-mathjax-config\">
MathJax.Hub.Config({
tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}
});
</script>" client-out)

    (write-bytes #"<div class=\"mdui-container mdui-typo mdui-m-t-1\">\n" client-out)

    ; Include a message, if any:
    (when message
        (write-string message client-out)
        (write-string "<hr />" client-out))
    
    ; Render the menu bar:
    (when menu-bar
        (write-string (xexpr->string menu-bar) client-out))
    
    ; Pass content.md through through a markdown formatter, and then
    ; through the wikifier to convert wiki syntax:
    (let ((temp-file-path (string-append md-file-path ".temp")))

        (write-file temp-file-path
            (regexp-replace* "\n```([a-z]+)" (read-file md-file-path) "\n```prettyprint lang-\\1"))

        (write-string ($ markdown-command temp-file-path) client-out)

        (delete-file temp-file-path))
    
    ; Write the footer:
    (write-bytes #"</div></body></html>" client-out))


; view wiki
(define (view-wiki req page
                #:message [message #f])
  
    (define user (auth-user req))

    ; markdown file containing the page:
    (define md-file-path (string-append
                            (database-page-dir user page)
                            "/"
                            "content.md"))

    (if (file-exists? md-file-path)

        (response
            200 #"OK"            ; code & message
            (current-seconds)    ; timestamp
            TEXT/HTML-MIME-TYPE  ; content type
            '()                  ; additional headers
            (λ (client-out)
                (make-wiki-view md-file-path
                    client-out
                    (response/xexpr/view/head 
                        #:title uiki-name)
                    #:message message
                    #:menu-bar (response/xexpr/view/menu-bar user page))))
    
        ; Or, if the page is not found:
        (response/xexpr/html
            (response/xexpr/head
                #:title "page not exist")
            (response/xexpr/view/not-exist/body page))))


; update wiki
(define (update-wiki req page)
    ; modifies the contents of the specified page.
    
    ; directory location:
    (define user (auth-user req))
    (define dir-path (database-page-dir user page))
    
    ; create the directory if it does not exist:
    (define created? #f)
    (when (not (directory-exists? dir-path))
        (set! created? #t)
        (make-directory* dir-path))
    
    ; location of the markdown file:
    (define md-file-path (string-append dir-path "/" "content.md"))

    ; grab the new contents:
    (define contents (cdr (assq 'content
                            (form-urlencoded->alist
                                (bytes->string/utf-8
                                    (request-post-data/raw req))))))

    ; edit the content file:
    (call-with-output-file
        md-file-path
        #:exists 'replace 
        (λ (out)
            (write-string contents out)))
    
    ; Notify git of changes.
    (git-commit dir-path)

    ; Render the page.
    (view-wiki req page
        #:message (if created? "Page created." "Page edited.")))


; edit wiki
(define (edit-wiki req page)
    ; creates a page to allow editing.
    
    (define user (auth-user req))
    (define dir-path (database-page-dir user page))
    (define md-file-path (string-append dir-path "/" "content.md"))
    
    (if (file-exists? md-file-path)
        (response/xexpr/html
            (response/xexpr/edit/head
                #:title (string-append "edit:" page))
            (response/xexpr/edit/body
                md-file-path
                page))
        (response/xexpr/jump2list)))


; delete wiki                
(define (delete-wiki req page)
    ; delete the wiki page.

    (define user (auth-user req))
    (delete-directory/files (database-page-dir user page)
        #:must-exist? #f)

    (response/xexpr/jump2list))


; view public wiki
(define (view-pub-wiki req user page)
    
    ; markdown file containing the page:
    (define md-file-path (string-append
                            (database-page-dir user page)
                            "/"
                            "content.md"))

    (if (file-exists? md-file-path)

        (response
            200 #"OK"            ; code & message
            (current-seconds)    ; timestamp
            TEXT/HTML-MIME-TYPE  ; content type
            '()                  ; additional headers
            (λ (client-out)
                (make-wiki-view md-file-path
                    client-out
                    (response/xexpr/view/pub/head
                        #:title uiki-name))))
    
        ; Or, if the page is not found:
        (response/xexpr/404 req)))


; dispatchs
(define dispatch
    (dispatch-case
        [("wiki") (with-auth list-wiki)]
        [("wiki" "") (with-auth list-wiki)]

        [("wiki" (string-arg)) #:method "get" (with-auth view-wiki)]
        [("wiki" (string-arg) "") #:method "get" (with-auth view-wiki)]

        [("wiki" (string-arg)) #:method "post" (with-auth update-wiki)]
        [("wiki" (string-arg) "") #:method "post" (with-auth update-wiki)]

        [("wiki" (string-arg) "edit") (with-auth edit-wiki)]
        [("wiki" (string-arg) "edit" "") (with-auth edit-wiki)]

        [("wiki" (string-arg) "delete") (with-auth delete-wiki)]
        [("wiki" (string-arg) "delete" "") (with-auth delete-wiki)]

        [("wiki" (string-arg) (string-arg)) view-pub-wiki]
        [("wiki" (string-arg) (string-arg) "") view-pub-wiki]))


(define (run)
    (serve/servlet dispatch 
        #:listen-ip uiki-listen-ip
        #:port uiki-port
        #:launch-browser? #f
        #:servlet-path "/wiki/main"
        #:servlet-regexp #rx""
        #:server-root-path (current-directory)
        #:extra-files-paths (list
                                (build-path (current-directory) "static"))
        #:file-not-found-responder response/xexpr/404
        #:ssl? use-ssl?
        #:ssl-cert ssl-cert-path
        #:ssl-key ssl-private-key-path
        #;end))

(run)
