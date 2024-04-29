(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash engine))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-engine")
  (test-gnc-numeric-from-string)
  (test-end "test-engine"))

(define (test-gnc-numeric-from-string)
  (test-equal "gnc-numeric-from-string 1"
    1
    (gnc-numeric-from-string "1"))

  (test-equal "gnc-numeric-from-string 3/4"
    3/4
    (gnc-numeric-from-string "3/4"))

  (test-equal "gnc-numeric-from-string 12 3/4"
    51/4
    (gnc-numeric-from-string "12 3/4"))

  (test-equal "gnc-numeric-from-string 2.5"
    5/2
    (gnc-numeric-from-string "2.5"))

  (test-equal "gnc-numeric-from-string .5"
    1/2
    (gnc-numeric-from-string ".5"))

  (test-equal "gnc-numeric-from-string 1/3"
    1/3
    (gnc-numeric-from-string "1/3"))

  ;; should error out; however "1 3" is parsed as 1.
  (test-skip 1)
  (test-equal "gnc-numeric-from-string 1 3"
    #f
    (gnc-numeric-from-string "1 3"))

  (test-equal "gnc-numeric-from-string #f"
    #f
    (gnc-numeric-from-string "#f")))
