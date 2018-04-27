(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(gnc:module-begin-syntax (gnc:module-load "gnucash/report/report-system" 0))

(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report report-system))
(use-modules (srfi srfi-64))

(define (test-runner)
  (let ((runner (test-runner-null))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (format #t "[~a] line:~a, test: ~a\n"
                (test-result-ref runner 'result-kind)
                (test-result-ref runner 'source-line)
                (test-runner-test-name runner))
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (1+ num-passed)))
          ((fail xfail)
           (if (test-result-ref runner 'expected-value)
               (format #t "~a\n -> expected: ~s\n -> obtained: ~s\n"
                       (string-join (test-runner-group-path runner) "/")
                       (test-result-ref runner 'expected-value)
                       (test-result-ref runner 'actual-value)))
           (set! num-failed (1+ num-failed)))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format #t "Source:~a\npass = ~a, fail = ~a\n"
                (test-result-ref runner 'source-file) num-passed num-failed)
        (zero? num-failed)))
    runner))

(define (run-test)
  (test-runner-factory test-runner)
  (test-begin "test-html-utilities-srfi64.scm")
  (test-gnc:html-string-sanitize)
  (test-end "test-html-utilities-srfi64.scm"))

(define (test-gnc:html-string-sanitize)
  (test-begin "gnc:html-string-sanitize")
  (test-equal "null test"
              "abc"
              (gnc:html-string-sanitize "abc"))

  (test-equal "sanitize &copy;"
              "&amp;copy;"
              (gnc:html-string-sanitize "&copy;"))

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
