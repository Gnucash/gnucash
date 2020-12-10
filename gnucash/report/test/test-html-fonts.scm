(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash report html-fonts))

(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "html-fonts")
  (test-font-name-to-style-info)
  (test-end "html-fonts"))

(define (test-font-name-to-style-info)
  (test-begin "font-name-to-style-info")

  (test-equal "basic"
    "font-family: \"Courier\", sans-serif; font-size: 20pt; font-weight: normal; "
    (font-name-to-style-info "Courier Regular 20"))

  (test-equal "basic size 50"
    "font-family: \"Courier\", sans-serif; font-size: 50pt; font-weight: normal; "
    (font-name-to-style-info "Courier Regular 50"))

  (test-equal "basic size 50 bold"
    "font-family: \"Courier\", sans-serif; font-size: 50pt; font-weight: bold; "
    (font-name-to-style-info "Courier bold 50"))

  (test-equal "basic size 50 italic"
    "font-family: \"Courier\", sans-serif; font-size: 50pt; font-style: italic; "
    (font-name-to-style-info "Courier italic 50"))

  (test-equal "basic size 15 oblique"
    "font-family: \"Courier\", sans-serif; font-size: 15pt; font-style: oblique; "
    (font-name-to-style-info "Courier oblique 15"))

  (test-equal "basic size 15 numeric weight"
    "font-family: \"Courier\", sans-serif; font-size: 15pt; font-weight: 550; "
    (font-name-to-style-info "Courier weight=550 15"))

  (test-end "font-name-to-style-info"))
