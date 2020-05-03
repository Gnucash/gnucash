(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(load "../html-fonts.scm")

(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "html-fonts")
  (test-font-name-to-style-info)
  (test-end "html-fonts"))

(define (test-font-name-to-style-info)
  (test-begin "font-name-to-style-info")

  (test-equal "basic"
    "font-family: Courier Regular, Sans-Serif; font-size: 20pt; "
    (font-name-to-style-info "Courier Regular 20"))

  (test-equal "basic size 50"
    "font-family: Courier Regular, Sans-Serif; font-size: 50pt; "
    (font-name-to-style-info "Courier Regular 50"))

  (test-equal "basic size 50 bold"
    "font-family: Courier, Sans-Serif; font-size: 50pt; font-weight: bold; "
    (font-name-to-style-info "Courier bold 50"))

  (test-equal "basic size 50 italic"
    "font-family: Courier, Sans-Serif; font-size: 50pt; font-style: italic; "
    (font-name-to-style-info "Courier italic 50"))

  (test-equal "basic size 15 oblique"
    "font-family: Courier, Sans-Serif; font-size: 15pt; font-style: oblique; "
    (font-name-to-style-info "Courier oblique 15"))

  (test-end "font-name-to-style-info"))
