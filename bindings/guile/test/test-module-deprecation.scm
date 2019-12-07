(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))

(define (run-test)
(test-runner-factory gnc:test-runner)
(test-begin "test gnc-guile module deprecation")
(test-read-eval-string "(use-modules (gnucash deprecated-module))")
(test-end "test gnc-guile module deprecation")

(test-begin "test gnc-guile module deprecation with replacement module")
(test-read-eval-string "(begin (use-modules (gnucash superseded-module))(gnc:warn \"Successfully redirected to replacement module\"))")
(test-end "test gnc-guile module deprecation with replacement module"))
