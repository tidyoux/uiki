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
    "base/wikify.rkt"
    "base/git.rkt"
    "base/auth.rkt")

; Import configuration variables:
(include "base/config.rkt")

; list wiki
(define (list-wiki req)
    (define dir (string->path database-dir))
    (define items
        (foldl (lambda (path result)
                    (when (directory-exists? dir) ; when dir is directory, not a file.
                        (let* ((pathstr (path->string path))
                                (title (regexp-match #px"#(.*?)\n"
                                            (read-file (string-append database-dir
                                                            "/"
                                                            (wikify-target pathstr)
                                                            "/content.md")))))
                            (append `((div ((class "mdui-card mdui-m-b-2 mdui-hoverable"))
                                        (div ((class "mdui-card-content"))
                                            (a ((class "mdui-btn") (href ,(string-append "/wiki/" pathstr)))
                                                (i ((class "mdui-icon material-icons mdui-m-r-1")) "description")
                                                ,(if title
                                                    (second title)
                                                    pathstr)))))
                                result))))
            '()
            (directory-list dir)))

    (response/xexpr/html
        (response/xexpr/head
            #:title "wiki list")
        (response/xexpr/list/body items)))

        
; view wiki
(define (view-wiki req page
                #:message [message #f])
  
  ; directory containing page contents:
  (define dir-path (string-append database-dir "/" page))
  
  ; markdown file containing the page:
  (define md-file-path (string-append dir-path "/" "content.md"))

  (cond
    
    [(file-exists? md-file-path)
     (response
      200 #"OK"            ; code & message
      (current-seconds)    ; timestamp
      TEXT/HTML-MIME-TYPE  ; content type
      '()                  ; additional headers
      (λ (client-out)
                
        ; render the top:
        (write-bytes #"<!DOCTYPE>\n<html>\n" client-out)
        
        ; render the header:
        (define head (response/xexpr/head 
                      #:title uiki-name))
        
        (write-string (xexpr->string head) client-out)
        
        ; render the body:
        (write-bytes #"<body class=\"mdui-theme-primary-indigo mdui-theme-accent-deep-orange\">" client-out)
        
        ; enable MathJax for LaTeX support:
        (write-bytes #"<script type=\"text/x-mathjax-config\">
MathJax.Hub.Config({
  tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}
});
</script>" client-out)
        (write-bytes #"<script src=\"https://cdn.bootcss.com/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML\"></script>" client-out)
        
        ; Enable prettify for syntax highlighting:
        (write-bytes #"<script src=\"../js/run_prettify.js\"></script>\n" client-out)

        (write-bytes #"<div class=\"mdui-container mdui-typo mdui-m-t-1\">\n" client-out)

        ; Include a message, if any:
        (when message
          (write-string message client-out)
          (write-string "<hr />" client-out))
        
        ; Render the menu bar:
        (define wiki-top-bar 
          (string->bytes/utf-8 (string-append "
<p>
<a class=\"mdui-btn mdui-btn-icon mdui-ripple\" href=\"/wiki/\">
    <i class=\"mdui-icon material-icons\">home</i>
</a>
<a class=\"mdui-btn mdui-btn-icon mdui-ripple\" href=\"/wiki/" (wikify-target page) "/edit\">
    <i class=\"mdui-icon material-icons\">edit</i>
</a>
<a class=\"mdui-btn mdui-btn-icon mdui-ripple mdui-float-right\" mdui-dialog=\"{target: '#deleteConfirm'}\">
    <i class=\"mdui-icon material-icons\">delete</i>
</a>
</p>

<div class=\"mdui-dialog\" id=\"deleteConfirm\">
    <div class=\"mdui-dialog-title\">Delete file?</div>
    <div class=\"mdui-dialog-actions\">
        <button class=\"mdui-btn mdui-ripple\" mdui-dialog-close>cancel</button>
        <a class=\"mdui-btn mdui-ripple\" href=\"/wiki/" (wikify-target page) "/delete\">delete</a>
    </div>
</div>
<hr />")))
        
        ; Render the menu bar:
        (write-bytes wiki-top-bar client-out)
        
        ; Pass content.md through through a markdown formatter, and then
        ; through the wikifier to convert wiki syntax:
        (let ((temp-file-path (string-append md-file-path ".temp")))

            (write-file temp-file-path
                (regexp-replace* "\n```([a-z]+)" (read-file md-file-path) "\n```prettyprint lang-\\1"))

            (define input-port
                (open-input-string ($ markdown-command temp-file-path)))

            ; Convert contents to wiki:
            (define wikified (apply string-append (wikify-text input-port)))
            (write-string wikified client-out)

            (close-input-port input-port)
            (delete-file temp-file-path))
        
        ; Write the footer:
        (write-bytes #"</div>" client-out)
        (write-bytes #"</body>" client-out)
        (write-bytes #"</html>" client-out)))]
    
    ; Or, if the page is not found:
    [else  
        (response/xexpr/html
            (response/xexpr/head
                #:title "page not exist")
            (response/xexpr/view-not-exist/body page))]))


; update wiki
(define (update-wiki req page)
    ; modifies the contents of the specified page.
    
    ; directory location:
    (define dir-path (string-append database-dir "/" (wikify-target page)))
    
    ; create the directory if it does not exist:
    (define created? #f)
    (when (not (directory-exists? dir-path))
        (set! created? #t)
        (make-directory dir-path))
    
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
    
    (define dir-path (string-append database-dir "/" page))
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

    (delete-directory/files (string-append database-dir "/" page)
        #:must-exist? #f)

    (response/xexpr/jump2list))


; dispatchs
(define dispatch
    (dispatch-case
        [("wiki") list-wiki]
        [("wiki" "") list-wiki]

        [("wiki" (string-arg)) #:method "get" view-wiki]
        [("wiki" (string-arg) "") #:method "get" view-wiki]

        [("wiki" (string-arg)) #:method "post" update-wiki]
        [("wiki" (string-arg) "") #:method "post" update-wiki]

        [("wiki" (string-arg) "edit") edit-wiki]
        [("wiki" (string-arg) "edit" "") edit-wiki]

        [("wiki" (string-arg) "delete") delete-wiki]
        [("wiki" (string-arg) "delete" "") delete-wiki]))


; start
(define (start req)
    (if (and auth-db-path (not (authenticated? auth-db-path req)))
        (response
            401 #"Unauthorized" 
            (current-seconds) 
            TEXT/HTML-MIME-TYPE
            (list
                (make-basic-auth-header
                    "Authentication required"
                    ))
            void)

        (dispatch req)))
  
(define (run)
    (serve/servlet start
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
