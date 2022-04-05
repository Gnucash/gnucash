(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-app-utils")
  (test-kvp-access)
  (test-end "test-app-utils"))

(define (setup book)
  (qof-book-set-option book "bla" '("top" "lvl1a"))
  (qof-book-set-option book "arg" '("top" "lvl1b"))
  (qof-book-set-option book "baf" '("top" "lvl1c" "lvl2" "lvl3")))

(define (teardown)
  (gnc-clear-current-session))

(define (test-kvp-access)
  (define book (gnc-get-current-book))
  (test-begin "kvp-access from guile")

  (setup book)

  (test-equal "top/lvl1a"
    "bla"
    (qof-book-get-option book '("top" "lvl1a")))

  (test-equal "top/lvl1b"
    "arg"
    (qof-book-get-option book '("top" "lvl1b")))

  (test-equal "top/lvl1c/lvl2/lvl3"
    "baf"
    (qof-book-get-option book '("top" "lvl1c" "lvl2" "lvl3")))

  (test-equal "top/lvl1c/lvl2"
    '(("lvl3" . "baf"))
    (qof-book-get-option book '("top" "lvl1c" "lvl2")))

  (test-equal "top/lvl1c"
    '(("lvl2" ("lvl3" . "baf")))
    (qof-book-get-option book '("top" "lvl1c")))

  ;; this tests the reading & writing of KvpFrame, copying branch
  ;; from top/lvl1c to top/lvl1d
  (qof-book-set-option book
                       (qof-book-get-option book '("top" "lvl1c"))
                       '("top" "lvl1d"))

  (test-equal "top/lvl1d, after copying from top/lvl1c"
    '(("lvl2" ("lvl3" . "baf")))
    (qof-book-get-option book '("top" "lvl1d")))

  (test-equal "top/lvl1c/lvl2/error"
    #f
    (qof-book-get-option book '("top" "lvl1c" "lvl2" "error")))

  (test-equal "top"
    '(("lvl1a" . "bla")
      ("lvl1b" . "arg")
      ("lvl1c" ("lvl2" ("lvl3" . "baf")))
      ("lvl1d" ("lvl2" ("lvl3" . "baf"))))
    (qof-book-get-option book '("top")))

  (test-end "kvp-access from guile")
  (teardown))
