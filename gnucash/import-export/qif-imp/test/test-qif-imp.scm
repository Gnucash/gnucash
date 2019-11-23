(use-modules (gnucash app-utils))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash qif-import))
(use-modules (gnucash string))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-qif-imp")
  (test-string)
  (test-qif-objects)
  (test-end "test-qif-imp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-string)
  (test-equal "string-rcontains"
    9
    (gnc:string-rcontains "foobarfoobarf" "bar"))

  (test-equal "string-rcontains"
    2
    (gnc:substring-count "foobarfoobarfoo" "bar"))

  (test-equal "substring-split"
    '("foo" "foo" "f")
    (gnc:substring-split "foobarfoobarf" "bar"))

  (test-equal "string-replace-char"
    "fcc"
    (gnc:string-replace-char "foo" #\o #\c))

  (test-equal "string-delete"
    "ad"
    (gnc:string-delete-chars "abcd" "cb"))

  (test-equal "list-display"
    "abc"
    (with-output-to-string
      (lambda ()
        (gnc:list-display '("a" "b" "c")))))

  (test-equal "list-display-to-string"
    "abc"
    (gnc:list-display-to-string '("a" "b" "c"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-objects.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-qif-objects)
  (test-assert "make-qif-file is called from C"
    (make-qif-file))

  (test-assert "make-ticker-map is called from C"
    (make-ticker-map)))

