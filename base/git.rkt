#lang racket

(require
    "cmd.rkt")

(provide
    git-commit)


(define (git-commit dir-path)
    ; if the git repository doesn't exist, create it:
    (if (not (directory-exists? (string-append dir-path "/.git")))
        
        ; TODO/WARNING/SECURITY: Injection attack vulnerability
        ; Need to verify that wikilink-name escapes path.
        (begin
            ($ (format "git -C \"~a\" init" dir-path))
            ($ (format "git -C \"~a\" add content.md" dir-path))
            ; TODO: Let the user set the update comment.
            ($ (format "git -C \"~a\" commit -m \"Initial commit.\"" dir-path)))
        (begin
            ($ (format "git -C \"~a\" add content.md" dir-path))
            ; TODO: Let the user set the update comment.
            ($ (format "git -C \"~a\" commit -m \"Updated page.\"" dir-path)))))
