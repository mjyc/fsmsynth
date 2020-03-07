#lang rosette/safe

(require rackunit rackunit/text-ui
  rosette/lib/angelic
  (only-in racket/base exn:fail? for/list hash-set)
  (only-in racket/hash hash-union)
  ltl/rosette
  "../lang.rkt"
  "../hole.rkt"
  "../utils.rkt"
  "../logger.rkt"
  )

(provide (all-defined-out))

(define (test-smtr-interpret)
  (test-case
    "test-smtr-interpret"
    (define tr-noifstmts (smtr '()))
    (define test-input
      '()
      )
    (check-exn
      exn:fail?
      (lambda () (smtr-interpret tr-noifstmts "" test-input))
      )

    (define tr
      (smtr
        (smif
          (smand (smequal? (sms) (smval "ready")) (smin 'a))
          (smval #hash((out . "true")))
          (smval #hash((out . "false")))
          )
        )
      )
    (check-equal?
      (smtr-interpret
        tr
        "ready"
        #hash((a . #f))
        )
      #hash((out . "false"))
      )
    (check-equal?
      (smtr-interpret
        tr
        "ready"
        #hash((a . #t))
        )
      #hash((out . "true"))
      )
    )
  )

(define (test-smm-interpret)
  (test-case
    "test-smm-interpret"
    (define test-input (hash-stream->list-stream
      `#hash(
        (in1 . ,(from-diagram "f-t--"))
        (in2 . ,(from-diagram "f---t"))
        )
      ))

    (define s (list #hash((out . ())) #hash((out . outval))))
    (define s0 #hash((out . ())))
    (define in #hash((in1 . (#t #f))(in2 . (#t #f))))
    (define tr
      (smtr
        (smif
          (smand
            (smequal? (sms) (smval #hash((out . ()))))
            (smequal? (smin 'in1) (smval #t))
            )
          (smval #hash((out . outval)))
          (smif
            (smand
              (smequal? (sms) (smval #hash((out . outval))))
              (smequal? (smin 'in2) (smval #t))
              )
            (smval #hash((out . ())))
            (smempty)
            )
          )
        )
      )
    (define machine (smm s s0 in tr))

    (define test-output
      (smm-interpret machine test-input))
    (define expected
      (list
        #hash((out . ()))
        (smempty)
        (smempty)
        #hash((out . outval))
        (smempty)
        #hash((out . ()))
        ))
    (check-equal? test-output expected)

    (define spec-ifttt1
      (ltlalways
        (ltlif
          (ltlval (cons 'in1 #t))
          (ltlval (cons 'out #hash((out . outval))))
          )
        )
      )
    (define spec-ifttt2
      (ltlalways
        (ltlif
          (ltlval (cons 'in2 #t))
          (ltlval (cons 'out #hash((out . ()))))
          )
        )
      )
    (define spec-evsc1
      (ltleventually
        (ltland
          (ltlval (cons 'in1 #t))
          (ltland
            (ltlval (cons 'out #hash((out . outval))))
            (ltlnext
              (ltleventually
                (ltland
                  (ltlval (cons 'in2 #t))
                  (ltlval (cons 'out #hash((out . ()))))
                  )
                )
              )
            )
          )
        )
      )
    (define test-inout (for/list
      ([in test-input] [out (cdr test-output)])
      (hash-set in 'out out)
      ))
    (check-true (ltleval spec-ifttt1 test-inout))
    (check-true (ltleval spec-ifttt2 test-inout))
    (check-true (ltleval spec-evsc1 test-inout))
    )
  )

(define (test-smm-angexe)
  (test-case
    "test-smm-angexe"
    (define s (list #hash((out . ())) #hash((out . outval))))
    (define s0 #hash((out . ())))
    (define in #hash((in1 . (#t #f))(in2 . (#t #f))))
    (define tr-sketch (smtr (??ifstatements s in 2)))
    (define machine (smm s s0 in tr-sketch))

    (define inputspec (hash-stream->list-stream
      `#hash(
        (in1 . ,(from-diagram "f-t--"))
        (in2 . ,(from-diagram "f---t"))
        )
      ))
    (define outputspec
      (list #hash((out . ())) (smempty) (smempty) #hash((out . outval)) (smempty) #hash((out . ()))))
    (define M
      (time
        (solve
          (assert
            (equal?
              (smm-interpret machine inputspec)
              outputspec
              )
            ))))
    (check-true (sat? M))
    (debug (smifstatements->string
      (smifstatements-prune (smtr-ifstatements (evaluate tr-sketch M)))
      "dotmd"
      (lambda (x) (cond
        [(equal? x #hash((out . ()))) "ready"]
        [(equal? x #hash((out . outval))) "run"]
        [else x]
        ))
      ))
    )
  )

(define (test-smm-synth-ifttt)
  (test-case
    "test-smm-synth-ifttt"
    (define s (list #hash((out . ())) #hash((out . outval))))
    (define s0 #hash((out . ())))
    (define in #hash((in1 . (#t #f))(in2 . (#t #f))))
    (define tr-sketch (smtr (??ifstatements s in 2)))
    (define machine-sketch (smm s s0 in tr-sketch))

    (define sym-input (hash-stream->list-stream
      `#hash(
        (in1 . ,(list #f (choose* #t #f) (choose* #t #f) (choose* #t #f) (choose* #t #f)))
        (in2 . ,(list #f (choose* #t #f) (choose* #t #f) (choose* #t #f) (choose* #t #f)))
        )
      ))
    (define sym-output (smm-interpret machine-sketch sym-input))
    (define sym-inout (for/list
      ([in sym-input] [out (cdr sym-output)])
      (hash-set in 'out out)
      ))

    (define spec-ifttt1
      (ltlalways
        (ltlif
          (ltlval (cons 'in1 #t))
          (ltlval (cons 'out #hash((out . outval))))
          )
        )
      )
    (define spec-ifttt2
      (ltlalways
        (ltlif
          (ltlval (cons 'in2 #t))
          (ltlval (cons 'out #hash((out . ()))))
          )
        )
      )

    (define M
      (time
        (synthesize
          #:forall (symbolics sym-input)
          #:guarantee (assert
            (and
              (equal?
                (ltleval spec-ifttt1 sym-inout)
                #t
                )
              (equal?
                (ltleval spec-ifttt2 sym-inout)
                #t
                )
              )
            )
          )
        )
      )
    (check-true (sat? M))
    (debug (smifstatements->string
      (smifstatements-prune (smtr-ifstatements (evaluate tr-sketch M)))
      "dotmd"
      (lambda (x) (cond
        [(equal? x #hash((out . ()))) "ready"]
        [(equal? x #hash((out . outval))) "run"]
        [else x]
        ))
      ))
    )
  )

(define (test-smm-synth-evsc)
  (test-case
    "test-smm-synth-evsc"
    (define s (list #hash((out . ())) #hash((out . outval))))
    (define s0 #hash((out . ())))
    (define in #hash((in1 . (#t #f))(in2 . (#t #f))))
    (define tr-sketch (smtr (??ifstatements s in 2)))
    (define machine-sketch (smm s s0 in tr-sketch))

    (define sym-input (hash-stream->list-stream
      `#hash(
        (in1 . ,(list #f (choose* #t #f) (choose* #t #f) (choose* #t #f) (choose* #t #f)))
        (in2 . ,(list #f (choose* #t #f) (choose* #t #f) (choose* #t #f) (choose* #t #f)))
        )
      ))
    (define sym-output (smm-interpret machine-sketch sym-input))
    (define sym-inout (for/list
      ([in sym-input] [out (cdr sym-output)])
      (hash-set in 'out out)
      ))

    (define spec-evsc1
      (ltleventually
        (ltland ; replacing it with "ltlif" does not work
          (ltlval (cons 'in1 #t))
          (ltland
            (ltlval (cons 'out #hash((out . outval))))
            (ltlnext
              (ltleventually
                (ltland ; replacing it with "ltlif" does not work
                  (ltlval (cons 'in2 #t))
                  (ltlval (cons 'out #hash((out . ()))))
                  )
                )
              )
            )
          )
        )
      )

    (define M
      (time
        (synthesize
          #:forall (symbolics sym-input)
          #:guarantee (assert
            (and
              (equal?
                (ltleval spec-evsc1 sym-inout)
                #t
                )
              #t
              )
            )
          )
        )
      )
    (check-true (sat? M))
    (debug (smifstatements->string
      (smifstatements-prune (smtr-ifstatements (evaluate tr-sketch M)))
      "js"
      (lambda (x) (cond
        [(equal? x #hash((out . ()))) "ready"]
        [(equal? x #hash((out . outval))) "run"]
        [else x]
        ))
      ))
    )
  )

(module+ test
  (define/provide-test-suite lang-tests
    (test-smtr-interpret)
    (test-smm-interpret)
    (test-smm-angexe)
    (test-smm-synth-ifttt)
    (test-smm-synth-evsc)
    )
  (run-tests lang-tests)
  )
