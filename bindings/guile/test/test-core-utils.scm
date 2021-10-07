(setenv "GNC_UNINSTALLED" "1")

(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash core-utils))

(define (N_-tests)

  (test-assert "N_ defined"
    (module-ref (current-module) 'N_))

  (test-equal "N_ works properly"
    "foobar"
    (N_ "foobar")))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-core-utils")
  (N_-tests)
  (test-end "test-core-utils"))
