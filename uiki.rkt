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
; + sed
; + htpasswd (bundled with apache)

; FEATURES:

; + Markdown support via multimarkdown
; + LaTeX support via MathJax
; + Syntax highlighting support via code-prettify
; + Backup/versioning support via git


(require web-server/servlet
         web-server/servlet-env)

(require web-server/private/mime-types)

(require xml)

(require net/uri-codec)


(require
  "base/command.rkt"
  "base/xexpr.rkt"
  "base/wikify.rkt"
  "base/auth.rkt"
  )

; Import configuration variables:
(include "base/config.rkt")


; Basic HTTP request processing helpers:
(define ext=>mime-type (read-mime-types mime-types-file))


; A handler for static files:
(define (handle-file-request req docroot path)
  
  ; NOTE: In practice, static requests can be replaced by
  ; providing an #:extra-files-paths to serve/servlet.
  
  ; identify the requested file:
  (define file (string-append docroot "/" (string-join path "/")))
  
  (cond
    [(file-exists? file)
     ; =>
     (define extension (string->symbol (bytes->string/utf-8 (filename-extension file))))
     (define content-type 
       (hash-ref ext=>mime-type extension 
                 (λ () TEXT/HTML-MIME-TYPE)))
     
     ; send the requested file back:
     (response
      200 #"OK"            ; code & message
      (current-seconds)    ; timestamp
      content-type         ; content-type
      '()                  ; additional headers
      (λ (client-out)
        (write-bytes (file->bytes file) client-out)))]
    
    [else (response/xexpr/404)]))
               
   
(define (handle-git-version-changes dir-path)
  ; if the git repository doesn't exist, create it:
  (when (not (directory-exists? (string-append dir-path "/.git")))
    
    ; TODO/WARNING/SECURITY: Injection attack vulnerability
    ; Need to verify that wikilink-name escapes path.
    
    (define git-init-cmd
      (string-append "git -C '" dir-path "' init;"
                     "git -C '" dir-path "' add content.md;"
                     "git -C '" dir-path "' commit -m 'Initial commit.'"))
    
    ($ git-init-cmd))
  
  ; commit changes:
  (define git-commit-cmd 
    (string-append "git -C '" dir-path "' add content.md;"
                   ; TODO: Let the user set the update comment.
                   "git -C '" dir-path "' commit -m 'Updated page.'"))
  
  ($ git-commit-cmd))


; Wiki-specific requests:

(define (handle-wiki-content-put-request req page)
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
  (handle-git-version-changes dir-path)

  ; Render the page.
  (handle-wiki-view-request 
   req page 
   #:message (if created? "Page created." "Page edited.")))


    
; A handler to render the interface for editing a page:
(define (handle-wiki-edit-request req page)
  ; creates a page to allow editing.
  
  (define dir-path (string-append database-dir "/" page))
  (define md-file-path (string-append dir-path "/" "content.md"))
  
  (cond
    
    [(file-exists? md-file-path)
     ; =>
     (response/xexpr
      #:preamble #"<!DOCTYPE html>"
      `(html
        ,(response/xexpr/head 
          #:title (string-append "edit: " page)
          #:style "

textarea#content {
  width: 50em;
  height: 30em;
}

")
        (body
         (form ((method "POST")
                (action "./"))
               (textarea
                ((id "content")
                 (name "content"))
                ,(file->string md-file-path))
               (br)
               (input ([type "submit"] [value "submit changes"]))))))]))


        

; A handler to render pages:
(define (handle-wiki-view-request req page
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
        (write-bytes #"<script src=\"http://localhost:8080/file/run_prettify.js\"></script>" client-out)

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
        (match (process (string-append markdown-preprocess-command " " md-file-path " | " markdown-command))
          [(list in out exit err interrupt)
           ; Convert contents to wiki:
           (define wikified (apply string-append (wikify-text in)))
           
           (write-string wikified client-out)])
        
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


(define (handle-wiki-request req resource)
  ; handle a top level /wiki/ request:
  (when (equal? (last resource) "")
    (set! resource (reverse (cdr (reverse resource)))))
  
  (match resource
    ; view the page:
    [`(,page)
     #:when (equal? (request-method req) #"GET")
     (handle-wiki-view-request req page)]
    
    ; modify the page contents:
    [`(,page)
     #:when (equal? (request-method req) #"POST")
     (handle-wiki-content-put-request req page)]
    
    ; edit the page:
    [`(,page "edit")
     (handle-wiki-edit-request req page)]
     
    [_ (response/xexpr/404)]))
    
(define (start req)
  
  ; extract the uri from the request:
  (define uri (request-uri req))
  
  ; extract the path from the uri:
  (define path (map path/param-path (url-path uri)))
    
  ; The first element of the path determines the service;
  ; choices are "wiki" or "file":
  (define service (car path))
  
  (define resource (cdr path))
  
  ; NOTE: This could be simplified with a higher-level
  ; interface for web-server/dispatch:
  
  ; http://docs.racket-lang.org/web-server/dispatch.html
  
  (cond
    [(and auth-db-path (not (authenticated? auth-db-path req)))
     (response
      401 #"Unauthorized" 
      (current-seconds) 
      TEXT/HTML-MIME-TYPE
      (list
       (make-basic-auth-header
        "Authentication required"
        ))
      void)]
    
    [(equal? service "file")
     (handle-file-request req document-root resource)]
    
    [(equal? service "wiki")
     (handle-wiki-request req resource)]

    [else (response/xexpr/404)]))
  
;
(serve/servlet start
               #:port uiki-port
               #:servlet-path "/wiki/main"
               #:servlet-regexp #rx""
               #:launch-browser? #f
               #:ssl? use-ssl?
               #:ssl-cert ssl-cert-path
               #:ssl-key ssl-private-key-path
               #;end)


