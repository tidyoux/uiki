#lang racket

(require
    parser-tools/lex
    (prefix-in : parser-tools/lex-sre))

(provide
    wikify-text
    wikify-target)


; Wiki text mark-up routines:
(define wikify-text
    ; convert a port into a list of strings,
    ; converting wiki mark-ups in the process:
    (lexer
    [(:: "[[" (complement (:: any-string "]]" any-string)) "]]")
      (begin
        (define text (substring lexeme 2 (- (string-length lexeme) 2)))
        (define target:text (string-split text "|"))
        (define link 
          (match target:text
            [`(,target)         (wikify-link target)]
            [`(,target ,text)   (wikify-link target text)]))
        (cons link (wikify-text input-port)))]
        
    [any-char
      (cons lexeme (wikify-text input-port))]
    
    [(eof)
      '()]))

(define (wikify-target target)
    ; sanitize a link target:
    (string-replace (string-downcase target) #px"[\\W]" "-"))

(define (wikify-link target [text #f])
    ; create an anchor tag:
    (define safe-target (wikify-target target))

    (string-append 
      "<a href=\"/wiki/" safe-target "\">" 
      (if text text target)
      "</a>"))
