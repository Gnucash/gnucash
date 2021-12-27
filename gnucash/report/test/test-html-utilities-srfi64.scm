(use-modules (gnucash app-utils))
(use-modules (gnucash report html-utilities))
(use-modules (tests test-engine-extras))
(use-modules (tests test-report-extras))
(use-modules (tests srfi64-extras))
(use-modules (srfi srfi-64))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-html-utilities-srfi64.scm")
  (test-gnc:assign-colors)
  (test-html-includes)
  (test-end "test-html-utilities-srfi64.scm"))

(define make-uri (@@ (gnucash report html-utilities) make-uri))

(define (test-gnc:assign-colors)
  (test-begin "test-gnc:assign-colors")
  (test-equal "assign-colors can request many colors"
    99
    (length (gnc:assign-colors 99)))
  (test-end "test-gnc:assign-colors"))

(define (test-html-includes)
  (test-begin "test-html-includes")

  (test-equal "windows path into rfc3986 uri"
    "file:///C:/Program%20Files%20%28x86%29/gnucash/share/gnucash/chartjs/Chart.bundle.min.js"
    (make-uri "C:\\Program Files (x86)\\gnucash/share\\gnucash\\chartjs/Chart.bundle.min.js"))

  (test-end "test-html-includes"))
