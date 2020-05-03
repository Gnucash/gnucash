(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash utilities))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-libgnucash-scm-utilities.scm")
  (test-traverse-vec)
  (test-substring-replace)
  (test-sort-and-delete-duplicates)
  (test-gnc:html-string-sanitize)
  (test-gnc:list-flatten)
  (test-begin "test-libgnucash-scm-utilities.scm"))

(define (test-traverse-vec)
  (test-begin "traverse-vec")
  (test-equal "list->vec"
    (vector 1 (vector 2 3))
    (traverse-list->vec
     (list 1 (list 2 3))))
  (test-equal "vec->list"
    (list 1 (list 2 3))
    (traverse-vec->list
     (vector 1 (vector 2 3))))
  (test-end "traverse-vec"))

(define (test-substring-replace)
  (test-begin "substring-replace")

  ;; generic gnc:substring-replace used in qif-guess-map.scm
  (test-equal "gnc:substring-replace"
    "fooxyzfooxyz"
    (gnc:substring-replace "foobarfoobar" "bar" "xyz"))

  ;; note the following 2 tests illustrate code behaviour: start from
  ;; 2nd matched substring, and perform either 2 or 1 substitution.
  (test-equal "gnc:substring-replace-from-to ... ... 2 2"
    "foobarfooxyzfooxyz"
    (gnc:substring-replace-from-to "foobarfoobarfoobar" "bar" "xyz" 2 2))

  (test-equal "gnc:substring-replace-from-to ... ... 2 1"
    "foobarfooxyzfoobar"
    (gnc:substring-replace-from-to "foobarfoobarfoobar" "bar" "xyz" 2 1))

  ;; comprehensive test suite for gnc:substring-replace-from-to:
  (test-equal "gnc:substring-replace-from-to ... ... 2 1"
    "foo xxx foo foo foo foo foo foo"
    (gnc:substring-replace-from-to
     "foo foo foo foo foo foo foo foo"
     "foo" "xxx" 2 1))

  (test-equal "gnc:substring-replace-from-to ... ... 1 1"
    "xxx foo foo foo foo foo foo foo"
    (gnc:substring-replace-from-to
     "foo foo foo foo foo foo foo foo"
     "foo" "xxx" 1 1))

  (test-equal "gnc:substring-replace-from-to ... ... 4 -1"
    "foo foo foo xxx xxx xxx xxx xxx"
    (gnc:substring-replace-from-to
     "foo foo foo foo foo foo foo foo"
     "foo" "xxx" 4 -1))

  (test-end "substring-replace"))

(define (test-sort-and-delete-duplicates)
  (test-begin "sort-and-delete-duplicates")
  (test-equal "sort-and-delete-duplicates empty"
    '()
    (sort-and-delete-duplicates '() <))
  (test-equal "sort-and-delete-duplicates 1-element"
    '(1)
    (sort-and-delete-duplicates '(1) <))
  (test-equal "sort-and-delete-duplicates 2-element, equal"
    '(1)
    (sort-and-delete-duplicates '(1 1) <))
  (test-equal "sort-and-delete-duplicates 2-element, unequal"
    '(1 2)
    (sort-and-delete-duplicates '(2 1) <))
  (test-equal "sort-and-delete-duplicates 3-element, equal"
    '(1)
    (sort-and-delete-duplicates '(1 1 1) <))
  (test-equal "sort-and-delete-duplicates 3-element, 2-equal"
    '(1 2)
    (sort-and-delete-duplicates '(1 2 1) <))
  (test-equal "sort-and-delete-duplicates 3-element, unequal"
    '(1 2 3)
    (sort-and-delete-duplicates '(3 1 2) <))
  (test-end "sort-and-delete-duplicates"))

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

(define (test-gnc:list-flatten)
  (test-equal "gnc:list-flatten null"
    '()
    (gnc:list-flatten '()))
  (test-equal "gnc:list-flatten noop"
    '(1 2 3)
    (gnc:list-flatten '(1 2 3)))
  (test-equal "gnc:list-flatten deep"
    '(1 2 3 4 5 6)
    (gnc:list-flatten '(1 (2) (() () (((((3))) ())) 4 () ((5) (6)))))))
