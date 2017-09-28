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
                (when (directory-exists? dir)
                    (let ((pathstr (path->string path)))
                         (append result `((a ((href ,(string-append "/wiki/" pathstr))) ,pathstr) (br))))))
            '()
            (directory-list dir)))

    (response/xexpr
        #:preamble #"<!DOCTYPE html>"
        `(html
            ,(response/xexpr/head
                #:title "wiki list")
            (body
                (h1 "wiki:")
                (hr)
                (p ,@items)))))


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
    (define post-data (request-post-data/raw req))
    
    (define param-string (bytes->string/utf-8 post-data))
    
    (define params (form-urlencoded->alist param-string))
                    
    (define contents (cdr (assq 'content params)))

    ; edit the content file:
    (call-with-output-file
    md-file-path
    #:exists 'replace 
    (λ (out)
        (write-string contents out)))
    
    ; Notify git of changes.
    (git-commit dir-path)

    ; Render the page.
    (view-wiki 
    req page 
    #:message (if created? "Page created." "Page edited.")))

    
; edit wiki
(define (edit-wiki req page)
    ; creates a page to allow editing.
    
    (define dir-path (string-append database-dir "/" page))
    (define md-file-path (string-append dir-path "/" "content.md"))
    
    (when (file-exists? md-file-path)
        (response/xexpr
            #:preamble #"<!DOCTYPE html>"
            `(html
                ,(response/xexpr/head
                    #:title (string-append "edit: " page))
                (body
                    (form ((method "POST")
                            (action ,(string-append "/wiki/" page)))
                        (textarea
                            ((id "content")
                            (name "content"))
                            ,(file->string md-file-path))
                        (br)
                        (input ([type "submit"] [value "submit changes"]))
                        (a ([href ,(string-append "/wiki/" page)] [style "margin:2em;"]) "cancel")))))))
        

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
                      #:title (string-append page " :: " uiki-name)))
        
        (write-string (xexpr->string head) client-out)
        
        ; render the body:
        (write-bytes #"<body>" client-out)
        
        ; enable MathJax for LaTeX support:
        (write-bytes #"<script type=\"text/x-mathjax-config\">
MathJax.Hub.Config({
  tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}
});
</script>" client-out)
        (write-bytes #"<script src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML\"></script>" client-out)
        
        ; Enable prettify for syntax highlighting:
        (write-bytes #"<script src=\"http://localhost:1234/js/run_prettify.js\"></script>" client-out)

        ; Include a message, if any:
        (when message
          (write-string message client-out)
          (write-string "<hr />" client-out))
        
        ; Render the menu bar:
        (define wiki-top-bar 
          (string->bytes/utf-8 (string-append "
<p>
[<a href=\"/wiki/" (wikify-target page) "/edit\">edit</a>]
</p>
<hr />")))
        
        ; Render the menu bar:
        (write-bytes wiki-top-bar client-out)
        
        ; Pass content.md through through a markdown formatter, and then
        ; through the wikifier to convert wiki syntax:
        (let ((temp-file-path (string-append md-file-path ".temp")))

            (write-file temp-file-path
                (regexp-replace* "\n```([a-z]+)" (read-file md-file-path) "\n```prettyprint lang-\\1"))

            (define input-port
                (open-input-string ($ (string-append markdown-command " " temp-file-path))))

            ; Convert contents to wiki:
            (define wikified (apply string-append (wikify-text input-port)))
            (write-string wikified client-out)

            (close-input-port input-port)
            (delete-file temp-file-path))
        
        ; Write the footer:
        (write-bytes #"</body>" client-out)
        
        (write-bytes #"</html>" client-out)))]
    
    ; Or, if the page is not found:
    [else  
     ; =>
     (response/xexpr
      #:preamble #"<!DOCTYPE html>"
      `(html
        ,(response/xexpr/head
          #:title "page does not yet exist")
        (body
         (p "Page does not exist")
         (form ([method "POST"] [action ,(string-append "/wiki/" page)])
               (input ([type "hidden"] [name "content"] [value "Blank page"]))
               (input ([type "submit"] [value "Create page"]))))))]))


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
        [("wiki" (string-arg) "edit" "") edit-wiki]))


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
