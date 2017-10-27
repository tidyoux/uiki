#lang racket

(provide
    format-date)


(define (format-date date format)
    (string-replace
        (string-replace
            (string-replace
                (string-replace
                    (string-replace
                        (string-replace format
                            "year"
                            (~a (date-year date)))
                        "month"
                            (~a (date-month date) #:min-width 2 #:align 'right #:left-pad-string "0"))
                    "day"
                        (~a (date-day date) #:min-width 2 #:align 'right #:left-pad-string "0"))
                "hour"
                    (~a (date-hour date) #:min-width 2 #:align 'right #:left-pad-string "0"))
            "minute"
                (~a (date-minute date) #:min-width 2 #:align 'right #:left-pad-string "0"))
        "second"
            (~a (date-second date) #:min-width 2 #:align 'right #:left-pad-string "0")))
