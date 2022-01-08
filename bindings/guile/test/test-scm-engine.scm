(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash engine))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-engine")
  (test-engine)
  (test-end "test-engine"))

(define (test-engine)
  (test-begin "testing function availability")

  (test-end "testing deprecated functions"))
