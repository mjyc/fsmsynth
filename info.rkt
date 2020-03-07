#lang info
(define collection "fsmsynth")
(define deps '("rosette" "base"))
(define build-deps '("rackunit-lib"))
(define pkg-authors '(mjyc))
(define test-omit-paths '(#rx"^((?!test\\.rkt).)*$"))
