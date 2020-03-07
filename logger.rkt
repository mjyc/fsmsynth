#lang racket/base

(provide (all-defined-out))

(define (debug . lst)
  (if (getenv "DEBUG")
    (begin
      (for-each (lambda (val)
        (display val)
        (display " "))
      lst)
      (newline)
      )
    (void)
    )
  )

(define (log . lst)
  (for-each (lambda (val)
    (display val)
    (display " "))
  lst)
  (newline)
  )
