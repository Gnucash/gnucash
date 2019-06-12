(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(gnc:module-begin-syntax (gnc:module-load "gnucash/report" 0))

(use-modules (tests test-engine-extras))
(use-modules (tests test-report-extras))
(use-modules (gnucash report report))
(use-modules (tests srfi64-extras))
(use-modules (srfi srfi-64))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-html-utilities-srfi64.scm")
  (test-gnc:html-string-sanitize)
  (test-gnc:assign-colors)
  (test-end "test-html-utilities-srfi64.scm"))

(define (test-gnc:html-string-sanitize)
  (test-begin "gnc:html-string-sanitize")
  (test-equal "null test"
              "abc"
              (gnc:html-string-sanitize "abc"))

  (test-equal "sanitize &copy;"
              "&amp;copy;"
              (gnc:html-string-sanitize "&copy;"))

  (if (not (string=? (with-output-to-string (lambda () (display "🎃"))) "🎃"))
      (test-skip 2))
  (test-equal "emoji unchanged"
              "🎃"
              (gnc:html-string-sanitize "🎃"))

  (test-equal "complex string"
              "Smiley:\"🙂\" something"
              (gnc:html-string-sanitize "Smiley:\"🙂\" something"))

  (test-equal "sanitize <b>bold tags</b>"
              "&lt;b&gt;bold tags&lt;/b&gt;"
              (gnc:html-string-sanitize "<b>bold tags</b>"))

  (test-equal "quotes are unchanged for html"
              "\""
              (gnc:html-string-sanitize "\""))

  (test-equal "backslash is unchanged for html"
              "\\"
              (gnc:html-string-sanitize "\\"))

  (test-end "gnc:html-string-sanitize"))

(define (test-gnc:assign-colors)
  (test-begin "test-gnc:assign-colors")
  (test-equal "assign-colors can request many colors"
    99
    (length (gnc:assign-colors 99)))
  (test-end "test-gnc:assign-colors"))

