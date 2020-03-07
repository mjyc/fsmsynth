#lang rosette/safe

(require
  rosette/lib/angelic rosette/lib/match
  (only-in racket/base for/and for/hash hash-keys hash-ref hash->list string? string-append values error)
  )

(provide (all-defined-out))

; sm - "(finite) state machine"

; Data Types

(struct smval (val) #:transparent)
(struct smempty () #:transparent)

; Operators

(struct smequal? (a1 a2) #:transparent)
(struct smand (a1 a2) #:transparent)
(struct smor (a1 a2) #:transparent)
(struct smif (condition then else) #:transparent)

; Program-related

(struct sms () #:transparent)
(struct smin (key) #:transparent)
(struct smtr (ifstatements) #:transparent)
(struct smm (s s0 in tr) #:transparent)

; Interpreters

(define (smtr-interpret trans state input)
  (define (interp tr)
    (match tr
      [(smval val) val]
      [(smempty) tr]
      [(smequal? a1 a2) (equal? (interp a1) (interp a2))]
      [(smand a1 a2) (and (interp a1) (interp a2))]
      [(smor a1 a2) (or (interp a1) (interp a2))]
      [(smif a1 a2 a3)
        (if
          (interp a1)
          (interp a2)
          (interp a3)
          )]
      [(sms) state]
      ; [(smin key) (hash-ref input key)] ; hash-ref is not lifted!
      [(smin key) (cdr (assoc key (hash->list input)))]
      )
    )
  (interp (smtr-ifstatements trans))
  )

(define (smm-interpret machine input)
  (define tr (smm-tr machine))
  (define inputnames (hash-keys (smm-in machine)))

  (define (step in s)
    (define curinnames (hash-keys in))
    (for/and ([k curinnames])
      (unless (member k inputnames) (error
        'smtr-interpret
        "expected ~a inputnames, given ~a"
        inputnames
        k))
      (define curinval (hash-ref in k))
      (define validinputkvalues (cons (smempty) (hash-ref (smm-in machine) k)))
      (unless (member curinval validinputkvalues) (error
        'smtr-interpret
        "expected ~a inputvalues for ~a, given ~a"
        validinputkvalues
        k
        curinval))
      )
    (smtr-interpret tr s in)
    )

  (define (run stream curs lastin)
    (cond
      [(empty? stream) '()]
      [else
        (define inraw (first stream))
        (define in
          (for/hash ([k inputnames])
            (define curin (hash-ref inraw k))
            (if (smempty? curin) (values k (hash-ref lastin k)) (values k curin))
            )
          )
        (define news
          (if (empty? in) curs (step in curs)))
        (cons news (run (rest stream) (if (smempty? news) curs news) in))
        ]
      )
    )

  (define initlastin
    (for/hash ([k inputnames])
      (values k (smempty)))
    )

  (cons (smm-s0 machine) (run input (smm-s0 machine) initlastin))
  )

(define (smifstatements->string stmts [mode "js"] [lookup identity])
  (define (rec stmts mode) (match mode
    ["js" (match stmts
      [(smval val)
        (define v (lookup val))
        (cond
          [(string? v) (string-append "\"" v "\"")]
          [(boolean? v) (if v "true" "false")]
          [else v]
          )
        ]
      [(smempty) "state"]
      [(smequal? a1 a2) (format "~a === ~a" (rec a1 mode) (rec a2 mode))]
      [(smand a1 a2) (format "~a && ~a" (rec a1 mode) (rec a2 mode))]
      [(smor a1 a2) (format "~a || ~a" (rec a1 mode) (rec a2 mode))]
      [(smif a1 a2 a3)
        (if (smif? a3)
          (format "if (~a) {\n  return ~a;\n} else ~a"
            (rec a1 mode)
            (rec a2 mode)
            (rec a3 mode)
            )
          (format "if (~a) {\n  return ~a;\n} else {\n  return ~a;\n}"
            (rec a1 mode)
            (rec a2 mode)
            (rec a3 mode)
            )
          )
        ]
      [(sms) "state"]
      [(smin key) key]
      )]
    ["dot" (match stmts
      [(smval val) (lookup val)]
      [(smempty) ""]
      [(smequal? a1 a2) (format "(equal? ~a ~a)" (rec a1 mode) (rec a2 mode))]
      [(smand a1 a2) (format "(and ~a ~a)" (rec a1 mode) (rec a2 mode))]
      [(smor a1 a2) (format "(or ~a ~a)" (rec a1 mode) (rec a2 mode))]
      [(smif (smand (smequal? (sms) curs) in) news elsestmt)
        (format "~a -> ~a [ label = \"~a\" ]\n~a"
        (rec curs mode)
        (rec news mode)
        (rec in mode)
        (rec elsestmt mode)
        )]
      [(smin key) key] ;
      )]
    ["mmd" (match stmts ;
      [(smval val) (lookup val)]
      [(smempty) ""]
      [(smequal? a1 a2) (format "(equal? ~a ~a)" (rec a1 mode) (rec a2 mode))]
      [(smand a1 a2) (format "(and ~a ~a)" (rec a1 mode) (rec a2 mode))]
      [(smor a1 a2) (format "(or ~a ~a)" (rec a1 mode) (rec a2 mode))]
      [(smif (smand (smequal? (sms) curs) in) news elsestmt)
        (format "~a --> |\"~a\"|~a\n~a"
          (rec curs mode)
          (rec in mode)
          (rec news mode)
          (rec elsestmt mode)
          )
        ]
      [(smin key) key]
      )]
    [_ (error 'program->string "unknown mode ~a" mode)]
    )
  )
  (cond
    [(equal? mode "dot") (string-append "digraph G {\n" (rec stmts mode) "}\n")]
    [(equal? mode "dotmd") (string-append "```graphviz\ndigraph G {\n" (rec stmts "dot") "}\n```\n")]
    [(equal? mode "mmd") (string-append "graph TD\n" (rec stmts mode))]
    [else (rec stmts mode)]
    )
  )
