#lang rosette/safe

(require rackunit rackunit/text-ui
  rosette/lib/angelic
  (only-in racket/base exn:fail? for/list hash-set)
  ltl/rosette
  "../lang.rkt"
  "../hole.rkt"
  "../utils.rkt"
  "../logger.rkt"
  )

(provide (all-defined-out))

(define (test-target-machine-runs)
  (test-case
    "test-target-machine-runs"
    (define s (list
      #hash((say . null))
      #hash((say . "hello"))
      #hash((say . "nice to meet you"))
      #hash((say . "bye"))
      ))
    (define s0 #hash((say . null)))
    (define in #hash(
      (start   . (#t #f))
      (faceVis . (#t #f))
      (sayDone . ("hello" "nice to meet you"))
      ))
    (define tr
      (smtr
        (smif
          (smand
            (smequal? (sms) (smval #hash((say . null))))
            (smand
              (smequal? (smin 'faceVis) (smval #t))
              (smequal? (smin 'start) (smval #t))
              )
            )
          (smval #hash((say . "hello")))
          (smif
            (smand
              (smequal? (sms) (smval #hash((say . "hello"))))
              (smand
                (smequal? (smin 'faceVis) (smval #t))
                (smequal? (smin 'sayDone) (smval "hello"))
                )
              )
            (smval #hash((say . "nice to meet you")))
            (smif
              (smand
                (smequal? (sms) (smval #hash((say . "nice to meet you"))))
                (smand
                  (smequal? (smin 'faceVis) (smval #t))
                  (smequal? (smin 'sayDone) (smval "nice to meet you"))
                  )
                )
              (smval #hash((say . "bye")))
              (smempty)
              )
            )
          )
        )
      )
    (define machine (smm s s0 in tr))
    (debug (smifstatements->string
      (smifstatements-prune (smtr-ifstatements tr))
      "dotmd"
      (lambda (x) (cond
        [(equal? x #hash((say . null))) "ready"]
        [(equal? x #hash((say . "hello"))) "say_hello"]
        [(equal? x #hash((say . "nice to meet you"))) "say_nice"]
        [(equal? x #hash((say . "bye"))) "say_bye"]
        [else x]
        ))
      ))

    (define test-input (hash-stream->list-stream
      `#hash(
        (start   . ,(from-diagram "ft----"))
        (faceVis . ,(from-diagram "ttt-t-")) ; not "t-----" because of spec-evsc1
        (sayDone . ,(from-diagram "--a-b-" #hash(
          (a . "hello")
          (b . "nice to meet you")
          )))
        )
      ))
    (define test-output (smm-interpret machine test-input))
    (define expected (list
      #hash((say . null))
      (smempty)
      #hash((say . "hello"))
      #hash((say . "nice to meet you"))
      (smempty)
      #hash((say . "bye"))
      (smempty)))
    (check-equal? test-output expected)

    (define spec-ifttt1
      (ltlalways
        (ltlif
          (ltland
            (ltlval (cons 'faceVis #t))
            (ltlval (cons 'start #t))
            )
          (ltlval (cons 'out #hash((say . "hello"))))
          )
        )
      )
    (define spec-ifttt2
      (ltlalways
        (ltlif
          (ltland
            (ltlval (cons 'faceVis #t))
            (ltlval (cons 'sayDone "hello"))
            )
          (ltlval (cons 'out #hash((say . "nice to meet you"))))
          )
        )
      )
    (define spec-ifttt3
      (ltlalways
        (ltlif
          (ltland
            (ltlval (cons 'faceVis #t))
            (ltlval (cons 'sayDone "nice to meet you"))
            )
          (ltlval (cons 'out #hash((say . "bye"))))
          )
        )
      )
    (define spec-evsc1
      (ltleventually
        (ltland
          (ltland (ltlval (cons 'start #t)) (ltlval (cons 'faceVis #t))) ; don't have "last event val" operator
          (ltland
            (ltlval (cons 'out #hash((say . "hello"))))
            (ltlnext
              (ltleventually
                (ltland
                  (ltland (ltlval (cons 'sayDone "hello")) (ltlval (cons 'faceVis #t))) ; don't have "last event val" operator
                  (ltland
                    (ltlval (cons 'out #hash((say . "nice to meet you"))))
                    (ltlnext
                      (ltleventually
                        (ltland
                          (ltland (ltlval (cons 'sayDone "nice to meet you")) (ltlval (cons 'faceVis #t))) ; don't have "last event val" operator
                          (ltlval (cons 'out #hash((say . "bye"))))
                          )
                        )
                      )
                    )
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
    (check-true (ltleval spec-ifttt3 test-inout))
    (check-true (ltleval spec-evsc1 test-inout))
    )
)

(define (test-synth-demonstration)
  (test-case
    "test-synth-demonstration"
    (define s (list
      #hash((say . null))
      #hash((say . "hello"))
      #hash((say . "nice to meet you"))
      #hash((say . "bye"))
      ))
    (define s0 #hash((say . null)))
    (define in #hash(
      (start   . (#t #f))
      (faceVis . (#t #f))
      (sayDone . ("hello" "nice to meet you"))
      ))
    (define tr-sketch (smtr (??ifstatements s in 3)))
    (define machine (smm s s0 in tr-sketch))

    (define inputspec (hash-stream->list-stream
      `#hash(
        (start   . ,(from-diagram "ft----"))
        (faceVis . ,(from-diagram "t-----"))
        (sayDone . ,(from-diagram "--a-b-" #hash(
          (a . "hello")
          (b . "nice to meet you")
          )))
        )
      ))
    (define outputspec (list
      #hash((say . null))
      (smempty)
      #hash((say . "hello"))
      #hash((say . "nice to meet you"))
      (smempty)
      #hash((say . "bye"))
      (smempty)))
    (define inputspec2 (hash-stream->list-stream
      `#hash(
        (start   . ,(from-diagram "ft----"))
        (faceVis . ,(from-diagram "f-----"))
        (sayDone . ,(from-diagram "------" #hash(
          (a . "hello")
          (b . "nice to meet you")
          )))
        )
      ))
    (define outputspec2 (list
      #hash((say . null))
      (smempty)
      (smempty)
      (smempty)
      (smempty)
      (smempty)
      (smempty)))
    (define inputspec3 (hash-stream->list-stream
      `#hash(
        (start   . ,(from-diagram "ft----"))
        (faceVis . ,(from-diagram "t-----"))
        (sayDone . ,(from-diagram "------" #hash(
          (a . "hello")
          (b . "nice to meet you")
          )))
        )
      ))
    (define outputspec3 (list
      #hash((say . null))
      (smempty)
      #hash((say . "hello"))
      (smempty)
      (smempty)
      (smempty)
      (smempty)))
    (define M
      (time
        (solve
          (assert
            (and
              (equal?
                (smm-interpret machine inputspec)
                outputspec
                )
              (equal?
                (smm-interpret machine inputspec2)
                outputspec2
                )
              (equal?
                (smm-interpret machine inputspec3)
                outputspec3
                )
              )
            ))))
    (check-true (sat? M))
    (debug (smifstatements->string
      (smifstatements-prune (smtr-ifstatements (evaluate tr-sketch M)))
      "dotmd"
      (lambda (x) (cond
        [(equal? x #hash((say . null))) "ready"]
        [(equal? x #hash((say . "hello"))) "say_hello"]
        [(equal? x #hash((say . "nice to meet you"))) "say_nice"]
        [(equal? x #hash((say . "bye"))) "say_bye"]
        [else x]
        ))
      ))
    )
  )

(define (test-synth-ifttt)
  (test-case
    "test-synth-ifttt"
    (define s (list
      #hash((say . null))
      #hash((say . "hello"))
      #hash((say . "nice to meet you"))
      #hash((say . "bye"))
      ))
    (define s0 #hash((say . null)))
    (define in #hash(
      (start   . (#t #f))
      (faceVis . (#t #f))
      (sayDone . ("hello" "nice to meet you"))
      ))
    (define tr-sketch (smtr (??ifstatements s in 3)))
    (define machine-sketch (smm s s0 in tr-sketch))

    (define sym-input (hash-stream->list-stream
      `#hash(
        (start . ,(from-diagram "ft----"))
        (faceVis . ,(list
          (choose* #t #f) ; can't add (smempty) yet
          (choose* #t #f) ; can't add (smempty) yet
          (choose* #t #f) ; can't add (smempty) yet
          (choose* #t #f) ; can't add (smempty) yet
          (choose* #t #f) ; can't add (smempty) yet
          (choose* #t #f) ; can't add (smempty) yet
          ))
        (sayDone . ,(list
          (choose* "hello" "nice to meet you") ; can't add (smempty) yet
          (choose* "hello" "nice to meet you") ; can't add (smempty) yet
          (choose* "hello" "nice to meet you") ; can't add (smempty) yet
          (choose* "hello" "nice to meet you") ; can't add (smempty) yet
          (choose* "hello" "nice to meet you") ; can't add (smempty) yet
          (choose* "hello" "nice to meet you") ; can't add (smempty) yet
          ))
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
          (ltland
            (ltlval (cons 'faceVis #t))
            (ltlval (cons 'start #t))
            )
          (ltlval (cons 'out #hash((say . "hello"))))
          )
        )
      )
    (define spec-ifttt2
      (ltlalways
        (ltlif
          (ltland
            (ltlval (cons 'faceVis #t))
            (ltlval (cons 'sayDone "hello"))
            )
          (ltlval (cons 'out #hash((say . "nice to meet you"))))
          )
        )
      )
    (define spec-ifttt3
      (ltlalways
        (ltlif
          (ltland
            (ltlval (cons 'faceVis #t))
            (ltlval (cons 'sayDone "nice to meet you"))
            )
          (ltlval (cons 'out #hash((say . "bye"))))
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
              (equal?
                (ltleval spec-ifttt3 sym-inout)
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
        [(equal? x #hash((say . null))) "ready"]
        [(equal? x #hash((say . "hello"))) "say_hello"]
        [(equal? x #hash((say . "nice to meet you"))) "say_nice"]
        [(equal? x #hash((say . "bye"))) "say_bye"]
        [else x]
        ))
      ))
    )
  )

(define (test-synth-evsc)
  (test-case
    "test-synth-evsc"
    (define s (list
      #hash((say . null))
      #hash((say . "hello"))
      #hash((say . "nice to meet you"))
      #hash((say . "bye"))
      ))
    (define s0 #hash((say . null)))
    (define in #hash(
      (start   . (#t #f))
      (faceVis . (#t #f))
      (sayDone . ("hello" "nice to meet you"))
      ))
    (define tr-sketch (smtr (??ifstatements s in 3)))
    (define machine-sketch (smm s s0 in tr-sketch))

    (define sym-input (hash-stream->list-stream
      `#hash(
        (start . ,(from-diagram "ft----"))
        (faceVis . ,(list
          (choose* #t #f)
          (choose* #t #f)
          (choose* #t #f)
          (choose* #t #f)
          (choose* #t #f)
          (choose* #t #f)))
        (sayDone . ,(list
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")))
        )
      ))
    (define sym-output (smm-interpret machine-sketch sym-input))
    (define sym-inout (for/list
      ([in sym-input] [out (cdr sym-output)])
      (hash-set in 'out out)
      ))

    (define spec-evsc1
      (ltleventually
        (ltland
          (ltland (ltlval (cons 'start #t)) (ltlval (cons 'faceVis #t)))
          (ltland
            (ltlval (cons 'out #hash((say . "hello"))))
            (ltlnext
              (ltleventually
                (ltland
                  (ltland (ltlval (cons 'sayDone "hello")) (ltlval (cons 'faceVis #t)))
                  (ltland
                    (ltlval (cons 'out #hash((say . "nice to meet you"))))
                    (ltlnext
                      (ltleventually
                        (ltland
                          (ltland (ltlval (cons 'sayDone "nice to meet you")) (ltlval (cons 'faceVis #t)))
                          (ltlval (cons 'out #hash((say . "bye"))))
                          )
                        )
                      )
                    )
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
        [(equal? x #hash((say . null))) "ready"]
        [(equal? x #hash((say . "hello"))) "say_hello"]
        [(equal? x #hash((say . "nice to meet you"))) "say_nice"]
        [(equal? x #hash((say . "bye"))) "say_bye"]
        [else x]
        ))
      ))
    )
  )

(define (test-synth-hybrid)
  (test-case
    "test-synth-hybrid"
    (define s (list
      #hash((say . null))
      #hash((say . "hello"))
      #hash((say . "nice to meet you"))
      #hash((say . "bye"))
      ))
    (define s0 #hash((say . null)))
    (define in #hash(
      (start   . (#t #f))
      (faceVis . (#t #f))
      (sayDone . ("hello" "nice to meet you"))
      ))
    (define tr-sketch (smtr (??ifstatements s in 3)))
    (define machine-sketch (smm s s0 in tr-sketch))

    (define sym-input (hash-stream->list-stream
      `#hash(
        (start . ,(from-diagram "ft----"))
        (faceVis . ,(list
          (choose* #t #f)
          (choose* #t #f)
          (choose* #t #f)
          (choose* #t #f)
          (choose* #t #f)
          (choose* #t #f)))
        (sayDone . ,(list
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")
          (choose* "hello" "nice to meet you")))
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
          (ltland
            (ltlval (cons 'faceVis #t))
            (ltlval (cons 'start #t))
            )
          (ltlval (cons 'out #hash((say . "hello"))))
          )
        )
      )
    (define spec-ifttt2
      (ltlalways
        (ltlif
          (ltland
            (ltlval (cons 'faceVis #t))
            (ltlval (cons 'sayDone "hello"))
            )
          (ltlval (cons 'out #hash((say . "nice to meet you"))))
          )
        )
      )
    (define spec-ifttt3
      (ltlalways
        (ltlif
          (ltland
            (ltlval (cons 'faceVis #t))
            (ltlval (cons 'sayDone "nice to meet you"))
            )
          (ltlval (cons 'out #hash((say . "bye"))))
          )
        )
      )
    (define spec-evsc1
      (ltleventually
        (ltland
          (ltland (ltlval (cons 'start #t)) (ltlval (cons 'faceVis #t)))
          (ltland
            (ltlval (cons 'out #hash((say . "hello"))))
            (ltlnext
              (ltleventually
                (ltland
                  (ltland (ltlval (cons 'sayDone "hello")) (ltlval (cons 'faceVis #t)))
                  (ltland
                    (ltlval (cons 'out #hash((say . "nice to meet you"))))
                    (ltlnext
                      (ltleventually
                        (ltland
                          (ltland (ltlval (cons 'sayDone "nice to meet you")) (ltlval (cons 'faceVis #t)))
                          (ltlval (cons 'out #hash((say . "bye"))))
                          )
                        )
                      )
                    )
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
                (ltleval spec-ifttt1 sym-inout)
                #t
                )
              (equal?
                (ltleval spec-ifttt2 sym-inout)
                #t
                )
              ; adding below yields unsat
              ; (equal?
              ;   (ltleval spec-ifttt3 sym-inout)
              ;   #t
              ;   )
              (equal?
                (ltleval spec-evsc1 sym-inout)
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
        [(equal? x #hash((say . null))) "ready"]
        [(equal? x #hash((say . "hello"))) "say_hello"]
        [(equal? x #hash((say . "nice to meet you"))) "say_nice"]
        [(equal? x #hash((say . "bye"))) "say_bye"]
        [else x]
        ))
      ))
    )
)


(module+ test
  (define/provide-test-suite lang-tests
    (test-target-machine-runs)
    (test-synth-demonstration)
    (test-synth-ifttt)
    (test-synth-evsc)
    (test-synth-hybrid)
    )
  (run-tests lang-tests)
  )
