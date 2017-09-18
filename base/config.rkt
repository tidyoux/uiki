
(define debug #f)

; the name of the giki:
(define uiki-name "generic uiki")

; TCP port number:
(define uiki-port 8080) ; set this to 8080 for testing
                        ; set this to 80 if unencrypted
                        ; set this to 443 if using SSL


; the root directory:
(define root (path->string (current-directory)))


; the database directory:
(define database-dir (string-append root "/db"))


; the command for processing markdown into html:
(define markdown-command "multimarkdown")

; the command for preprocessing markdown into markdown:
(define markdown-preprocess-command "sed -E -e 's/^```([a-z]+)/```prettyprint lang-\\1/'")

; the authentication database:
(define auth-db-path (string-append root "/passwd"))
; set this to #f to disable authentication


; SSL parameters
(define use-ssl? #f)

(define ssl-cert-path #f)
; If enabled, try:
;  (string-append root "/certs/server-cert.pem")

(define ssl-private-key-path #f)
; If enabled, try:
;  (string-append root "/certs/private-key.pem")

; see the racket documentation on Continue
; for how to generate the certificate and private key
; needed for SSL

