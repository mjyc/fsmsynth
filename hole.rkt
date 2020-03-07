#lang rosette/safe

(require
  rosette/lib/angelic
  (only-in racket/base hash-keys hash->list)
  (only-in racket/list cartesian-product)
  "./lang.rkt"
  )

(provide (all-defined-out))

(define (??ifstatements states inputs [maxdepth 2])
  (define inputnames (hash-keys inputs))
  (define inputslist (hash->list inputs))

  (define (??ifcondition)
    (define sink (apply choose* inputnames))
    (define sinv (apply choose* (cdr (assoc sink inputslist))))
    (define terminals
      (list
        (smand
          (smequal? (sms) (smval (apply choose* states)))
          (smequal? (smin sink) (smval sinv))
          )
        ))
    (when
      (>= (length inputnames) 2)
      (define sink1 (apply choose* inputnames))
      (define sink2 (apply choose* inputnames))
      (define sinv1 (apply choose* (cdr (assoc sink1 inputslist))))
      (define sinv2 (apply choose* (cdr (assoc sink2 inputslist))))
      (set! terminals (cons
        (smand
          (smequal? (sms) (smval (apply choose* states)))
          (smand
            (smequal? (smin sink1) (smval sinv1))
            (smequal? (smin sink2) (smval sinv2))
            )
          )
        terminals))
      )
    (apply choose* terminals)
    )

  (define (build depth)
    (cond
      [(equal? depth 1)
        (smif (??ifcondition)
          (smval (apply choose* (cons (smempty) states)))
          (smempty)
          )
        ]
      [else
        (smif (??ifcondition)
          (smval (apply choose* (cons (smempty) states)))
          (build (sub1 depth))
          )
        ]
      )
    )

  (build maxdepth)
  )
