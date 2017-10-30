#lang racket

(require
    file/sha1
    net/base64
    web-server/servlet)

(include "config.rkt")

(provide
    auth-user
    with-auth)

; HTTP Basic Authentication:
(define (htpasswd-credentials-valid?
         passwd-file
         username
         password)
    ; checks if the given credentials match those in the database
    ; it assumes all entries as SHA1-encoded as in `htpasswd -s`.

    ; read in the lines from the password file:
    (define lines (call-with-input-file passwd-file 
                    (λ (port) (port->lines port))))
    
    ; convert the password to sha1:
    (define sha1-pass (sha1-bytes (open-input-bytes password)))
    
    ; then to base64 encoding:
    (define sha1-pass-b64 
            (bytes->string/utf-8 (base64-encode sha1-pass #"")))
    
    ; check if both the username and the password match:
    (define (password-matches? line)

        (define user:hash (string-split line ":"))
        
        (define user (car user:hash))
        (define hash (cadr user:hash))
        
        (match (string->list hash)
            ; check for SHA1 prefix
            [`(#\{ #\S #\H #\A #\} . ,hashpass-chars)
                (define hashpass (list->string hashpass-chars))
                (and (equal? username (string->bytes/utf-8 user)) 
                    (equal? hashpass sha1-pass-b64))]))
    
    ; check to see if any line validates:
    (ormap password-matches? lines))

; auth user
(define (auth-user req)
    (let ((auth-info (request->basic-credentials req)))
        (if auth-info
            (bytes->string/utf-8 (car auth-info))
            "")))

; authenticated?
(define (authenticated? passwd-file req)
    ; checks if a request has valid credentials:
    (match (request->basic-credentials req)
        [(cons user pass)
            (htpasswd-credentials-valid? passwd-file user pass)]
        
        [else #f]))

; with-auth
(define (with-auth handler)
    (lambda (req . args)
        (if (and auth-db-path
                (not (authenticated? auth-db-path req)))
            (response
                401 #"Unauthorized" 
                (current-seconds) 
                TEXT/HTML-MIME-TYPE
                (list (make-basic-auth-header
                        "Authentication required"))
                void)

            (apply handler req args))))
