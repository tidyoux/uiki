#lang racket

(require
    "cmd.rkt")

(provide
    git-commit)


(define (git-commit dir-path)
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