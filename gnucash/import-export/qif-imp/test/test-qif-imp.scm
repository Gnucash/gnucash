(use-modules (gnucash app-utils))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash qif-import))
(use-modules (gnucash string))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-qif-imp")
  (test-string)
  (test-price-parse)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-file.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-price-parse)
  (let ((parsed (qif-file:parse-price-line "\"ABC\",1.0,\"1/1/04\""))
        (model (make-qif-price)))
    (qif-price:set-symbol! model "ABC")
    (qif-price:set-share-price! model "1")
    (qif-price:set-date! model "1/1/04")
    (test-equal "parse-price-line-decimal" model parsed))

  (let ((parsed (qif-file:parse-price-line "\"ABC\",1 3/4,\"1/1' 4\""))
        (model (make-qif-price)))
    (qif-price:set-symbol! model "ABC")
    (qif-price:set-share-price! model "7/4")
    (qif-price:set-date! model "1/1' 4")
    (test-equal "parse-price-line-fraction" parsed model))

  (let ((parsed (qif-file:parse-price-line "\"ABC\",,\"1/1' 4\"")))
    (test-equal "parse-price-line-empty" #f parsed))

  (let ((parsed (qif-file:parse-price-line "\"ABC\",\"1/1' 4\"")))
    (test-equal "parse-price-line-missingcomma" #f parsed)))
