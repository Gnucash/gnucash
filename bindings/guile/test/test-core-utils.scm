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

(define (gnc-format-tests)
  (test-equal "null"
    ""
    (gnc:format ""))

  (test-equal "basic"
    "basic"
    (gnc:format "basic"))

  (test-equal "basic with unused symbols"
    "basic"
    (gnc:format "basic" 'task "testing"))

  (test-equal "one substitution"
    "basic test"
    (gnc:format "basic ${job}" 'job "test"))

  (test-equal "one substitution with hyphen"
    "master chief"
    (gnc:format "master ${job-title}" 'job-title "chief"))

  (test-equal "two substitutions out of order"
    "basic test"
    (gnc:format "${difficulty} ${job}" 'job "test" 'difficulty "basic"))

  (test-equal "trying to reference invalid symbol"
    "${symbol} does not exist"
    (gnc:format "${symbol} does not exist" 'existence "none"))

  (test-error "gnc:format syntax error" #t
    (gnc:format "${symbol} does not exist" 'existence)))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-core-utils")
  (N_-tests)
  (gnc-format-tests)
  (test-end "test-core-utils"))
