#lang racket

(require
 (only-in rosette/base/core/reflect symbolics)
 "./lang.rkt"
 )

(provide (all-defined-out))

(define (from-diagram diagramString [values (make-hash)])
  (define characters (cdr (drop-right (string-split diagramString "") 1)))
  (define (rec chars outstream)
    (match chars
      [(cons "-" xs) (rec xs (cons (smempty) outstream))]
      [(cons "t" xs) (rec xs (cons #t outstream))]
      [(cons "f" xs) (rec xs (cons #f outstream))]
      [(cons x xs)
        (define xsym (hash-ref values (string->symbol x) (void)))
        (if
          (not (eq? xsym (void)))
          (rec xs (cons xsym outstream))
          (match x
            [(regexp #rx"[0-9]") (rec xs (cons (string->number x) outstream))]
            [_ (rec xs (cons x outstream))]
            )
          )
        ]
      ['() outstream]
      )
    )
  (reverse (rec characters '()))
  )

; assumes input is a hash of lists with the same length
(define (hash-stream->list-stream hsh)
  (cond
    [(hash-empty? hsh) '()]
    [else
      (for/list
        ([i (build-list (length (hash-iterate-value hsh 0)) values)])
        (for/hash ([k (hash-keys hsh)])
          (values k (list-ref (hash-ref hsh k) i)))
        )
      ]
    )
  )

; assumes input is a list of hashes with the same set of keys
(define (list-stream->hash-stream lst)
  (cond
    [(empty? lst) #hash()]
    [else
      (for/hash ([k (hash-keys (list-ref lst 0))])
        (values k (map (lambda (hsh) (hash-ref hsh k)) lst)))
      ]
    )
  )

; prunes if statements that returns "nostatechange" or symbolic value
(define (smifstatements-prune stmts) (match stmts
  [(smif (smand (smequal? (sms) curs) (smequal? ink inv)) news elsestmt)
    (cond
      [(or (smempty? news) (not (empty? (symbolics news))))
        (smifstatements-prune elsestmt)
        ]
      [else
        (smif
          (smand (smequal? (sms) curs) (smequal? ink inv))
          news
          (smifstatements-prune elsestmt)
          )
        ]
      )
    ]
  [_ stmts]
  ))
