 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; test-gnc-option-scheme-output.scm -- Test Scheme option i/o.     ;
 ; Copyright (C) 2021 John Ralls <jralls@ceridwen.us>               ;
 ;                                                                  ;
 ; This program is free software; you can redistribute it and/or    ;
 ; modify it under the terms of the GNU General Public License as   ;
 ; published by the Free Software Foundation; either version 2 of   ;
 ; the License, or (at your option) any later version.              ;
 ;                                                                  ;
 ; This program is distributed in the hope that it will be useful,  ;
 ; but WITHOUT ANY WARRANTY; without even the implied warranty of   ;
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    ;
 ; GNU General Public License for more details.                     ;
 ;                                                                  ;
 ; You should have received a copy of the GNU General Public License;
 ; along with this program; if not, contact:                        ;
 ;                                                                  ;
 ; Free Software Foundation           Voice:  +1-617-542-5942       ;
 ; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       ;
 ; Boston, MA  02110-1301,  USA       gnu@gnu.org                   ;
 ;                                                                  ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 format))            ; ~f requires (ice-9 format)
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash options))
(use-modules (sw_app_utils))
(use-modules (sw_engine))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-gnc-option-scheme-io")
  (test-gnc-string-option-to-scheme)
  (test-gnc-text-option-to-scheme)
  (test-gnc-font-option-to-scheme)
  (test-gnc-currency-option-to-scheme)
  (test-gnc-budget-option-to-scheme)
  (test-gnc-commodity-option-to-scheme)
  (test-gnc-bool-option-to-scheme)
  (test-gnc-pixmap-option-to-scheme)
  (test-gnc-date-option-to-scheme)
  (test-gnc-account-options-to-scheme)
  (test-gnc-multichoice-option-to-scheme)
  (test-gnc-list-option-to-scheme)
  (test-gnc-number-range-option-to-scheme)
  (test-gnc-number-plot-size-option-to-scheme)
  (test-gnc-query-option-to-scheme)
  (test-gnc-color-option-to-scheme)
  (test-gnc-invoice-option-to-scheme)
  (test-gnc-owner-option-to-scheme)
  (test-gnc-internal-option-to-scheme)
  (test-end "test-gnc-option-scheme-io"))

(define test-unchanged-section-output-template
  "
; Section: foo

"
  )

(define (test-string-output-template value)
  (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (o) (if o (gnc:option-set-value o ~s))) option))

" value))

(define (test-literal-output-template value)
  (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (o) (if o (gnc:option-set-value o '~a))) option))

" value))

(define (test-list-output-template value)
  (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (o) (if o (gnc:option-set-value o '~s))) option))

" value))

(define (test-currency-output-template value)
  (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (o) (if o (gnc:option-set-value o ~s))) option))

" value))

(define (test-commodity-output-template value)
  (let ((value-parts (string-split value #\:)))
       (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (o) (if o (gnc:option-set-value o '(commodity-scm ~s ~s)))) option))

" (car value-parts) (cadr value-parts))))

(define (test-budget-output-template value)
  (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (option) (if option ((gnc:option-setter option) (gnc-budget-lookup ~s (gnc-get-current-book))))) option))

"
          (gncBudgetGetGUID value)))

(define (test-number-range-output-template value)
  (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (o) (if o (gnc:option-set-value o ~a))) option))

" (if (exact-integer? value)
      (if (< value 100)
          (format #f "'(percent . ~d)" value)
          (format #f "'(pixels . ~d)" value))
      (format #f "'~f" value)
      )))

(define (test-option-scheme-output name make-option-func get-value-func test-template default value)
  (let ((odb (gnc:new-options))
        (option (make-option-func "foo" "bar" "baz" "Test Option" default)))
    (gnc:register-option odb option)
    (test-equal (string-append name " unchanged")
                test-unchanged-section-output-template
                (gnc:generate-restore-forms odb "options"))
    (gnc:option-set-value (gnc:lookup-option odb "foo" "bar") value)
    (test-equal (string-append name " value")
                (test-template (get-value-func (gnc:lookup-option odb "foo" "bar")))
                (gnc:generate-restore-forms odb "options"))))

(define (test-gnc-string-option-to-scheme)
  (test-begin "test-gnc-string-option-to-scheme")
  (test-option-scheme-output "string"
                             gnc:make-string-option GncOption-get-value
                             test-string-output-template
                             "waldo" "pepper")
  (test-end "test-gnc-string-option-to-scheme"))

(define (test-gnc-text-option-to-scheme)
  (test-begin "test-gnc-text-option-to-scheme")
  (test-option-scheme-output "text"
                             gnc:make-string-option GncOption-get-value
                             test-string-output-template
                             ""
"Sed ut perspiciatis, unde omnis iste natus error sit voluptatem accusantium
doloremque laudantium, totam rem aperiam eaque ipsa, quae ab illo inventore
veritatis et quasi architecto beatae vitae dicta sunt, explicabo.")
  (test-end "test-gnc-text-option-to-scheme"))

(define (test-gnc-font-option-to-scheme)
  (test-begin "test-gnc-font-option-to-scheme")
  (test-option-scheme-output "font"
                             gnc:make-font-option GncOption-get-value
                             test-string-output-template
                             "URW Bookman L Bold Italic 12"
                             "Helvetica 12")
  (test-end "test-gnc-font-option-to-scheme"))

(define (test-gnc-currency-option-to-scheme)
  (test-begin "test-gnc-currency-option-to-scheme")
  (let ((session (gnc-get-current-session))
        (book (gnc-get-current-book))
        (table (gnc-commodity-table-new)))
    (test-book-set-data book "gnc-commodity-table" table)
    (let ((USD (gnc-commodity-new book "United States Dollar" "CURRENCY" "USD" "" 100))
          (EUR (gnc-commodity-new book "European Union Euro" "CURRENCY" "EUR" "" 100)))
      (gnc-commodity-table-insert table USD)
      (gnc-commodity-table-insert table EUR)
      (test-option-scheme-output "currency"
                                 gnc:make-currency-option GncOption-serialize
                                 test-currency-output-template
                                 USD EUR)
;; Garbage collection has already eaten USD and EUR.
      (test-book-clear-data book "gnc-commodity-table")
      (gnc-commodity-table-destroy table)
      (gnc-clear-current-session)))
  (test-end "test-gnc-currency-option-to-scheme"))

(define (test-gnc-budget-option-to-scheme)
 (test-begin "test-gnc-budget-option-to-scheme")
  (let* ((book (gnc-get-current-book))
        (budget2 (gnc-budget-new book))
        (budget1 (gnc-budget-new book))
        (guid1 (gncBudgetGetGUID budget1))
        (guid2 (gncBudgetGetGUID budget2)))

    (test-book-set-default-budget book budget1)
    (gnc-budget-set-name budget1 "First Budget")
    (gnc-budget-set-name budget2 "Second Budget")

    (let ((odb (gnc:new-options))
          (option (gnc:make-budget-option "foo" "bar" "baz" "Test Option")))
    (gnc:register-option odb option)
    (test-equal "budget unchanged"
                test-unchanged-section-output-template
                (gnc:generate-restore-forms odb "options"))
    (gnc:option-set-value (gnc:lookup-option odb "foo" "bar") budget2)
    (test-equal "default budget value" (gnc-budget-get-default book) budget1)
    (test-equal "budget restore form" (test-budget-output-template budget2)
                (gnc:generate-restore-forms odb "options")))
    (gnc-clear-current-session))
  (test-end "test-gnc-budget-option-to-scheme"))

(define (test-gnc-commodity-option-to-scheme)
  (test-begin "test-gnc-commodity-option-to-scheme")
  (let* ((session (gnc-get-current-session))
         (book (gnc-get-current-book))
         (comm-tbl (gnc-commodity-table-get-table book))
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (FMAGX (gnc-commodity-new book "Fidelity Magellan Fund" "FUND" "FMAGX" "" 1000)))
         (gnc-commodity-table-insert comm-tbl AAPL)
         (gnc-commodity-table-insert comm-tbl FMAGX)
    (test-option-scheme-output "commodity"
                               gnc:make-commodity-option GncOption-serialize
                               test-commodity-output-template
                               AAPL FMAGX))
  (test-end "test-gnc-commodity-option-to-scheme"))

(define (test-gnc-bool-option-to-scheme)
  (test-begin "test-gnc-bool-option-to-scheme")
  (test-option-scheme-output "bool"
                             gnc:make-simple-boolean-option
                             GncOption-get-value
                             test-string-output-template #f #t)
  (test-end "test-gnc-bool-option-to-scheme"))

(define (test-gnc-pixmap-option-to-scheme)
  (test-begin "test-gnc-pixmap-option-to-scheme")
  (test-option-scheme-output "pixmap"
                             gnc:make-pixmap-option GncOption-get-value
                             test-string-output-template
                             "" "~/mybusiness/mylogo.png")
  (test-end "test-gnc-pixmap-option-to-scheme"))

(define (test-gnc-date-option-to-scheme)
  (test-begin "test-gnc-date-option-to-scheme")
  (let ((odb (gnc:new-options)))
    (gnc:options-make-end-date! odb "foo" "bar" "baz" "Phoney Option")
    (test-equal "Date Unchanged" test-unchanged-section-output-template
                (gnc:generate-restore-forms odb "options"))
    (let* ((option (gnc:lookup-option odb "foo" "bar"))
          (test-template test-literal-output-template)
          (time (gnc-dmy2time64 25 12 2020))
          (value `(absolute . ,time)))
      (gnc:option-set-value option value)
      (test-equal "Absolute Date" (test-template (GncOption-serialize option))
                  (gnc:generate-restore-forms odb "options"))
      (set! value '(relative . end-prev-year))
      (gnc:option-set-value option value)
      (test-equal "Relative Date Value" value (GncOption-get-value option))
      (test-equal "Relative Date" (test-template (GncOption-serialize option))
                  (gnc:generate-restore-forms odb "options"))))
  (test-end "test-gnc-date-option-to-scheme"))

(define (test-gnc-account-options-to-scheme)
  (define (create-account book parent type name)
    (let ((account (xaccMallocAccount book)))
      (xaccAccountBeginEdit account)
      (xaccAccountSetType account type)
      (xaccAccountSetName account name)
      (xaccAccountBeginEdit parent)
      (gnc-account-append-child parent account)
      (xaccAccountCommitEdit parent)
      (xaccAccountCommitEdit account)
      account))

  (define (make-account-tree book root)
    (let* ((assets (create-account book root ACCT-TYPE-ASSET "Assets"))
           (liabilities  (create-account book root ACCT-TYPE-LIABILITY "Liabilities"))
           (equity  (create-account book root ACCT-TYPE-EQUITY "Equity"))
           (expenses  (create-account book root ACCT-TYPE-EXPENSE "Expenses"))
           (equity  (create-account book root ACCT-TYPE-INCOME "Income"))
           (broker  (create-account book assets ACCT-TYPE-EQUITY "broker"))
           (stocks  (create-account book broker ACCT-TYPE-STOCK "Stocks")))
      (create-account book assets ACCT-TYPE-BANK "Bank")
      (create-account book stocks ACCT-TYPE-STOCK "AAPL")
      (create-account book stocks ACCT-TYPE-STOCK "MSFT")
      (create-account book stocks ACCT-TYPE-STOCK "HPE")
      (create-account book broker ACCT-TYPE-BANK "Cash Management")
      (create-account book expenses ACCT-TYPE-EXPENSE "Food")
      (create-account book expenses ACCT-TYPE-EXPENSE "Gas")
      (create-account book expenses ACCT-TYPE-EXPENSE "Rent")))

  (define (cleanup book root)
;; Destroying the book destroys the account tree too
    (gnc-option-test-book-destroy book))

  (define (test-gnc-account-list-option-to-scheme book)
    (define (test-account-list-output-template value)
        (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (o) (if o (gnc:option-set-value o '~s))) option))

" (reverse (string-split value #\ ))))

    (test-begin "test-gnc-account-list-option-to-scheme")
    (let ((odb (gnc:new-options))
          (acctlist (gnc-account-list-from-types book
                                                 (list ACCT-TYPE-STOCK))))
      (gnc:register-option odb
                           (gnc:make-account-list-option
                            "foo" "bar" "a" "baz" (lambda () acctlist)
                            (lambda (ac)
                              (let ((type (xaccAccountGetType ac)))
                                (or (eqv? type ACCT-TYPE-STOCK)
                                    (eqv? type ACCT-TYPE-BANK)))) #t))
      (test-equal "account list unchanged"
                  test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-account-list-output-template)
            (new-acclist (gnc-account-list-from-types book (list ACCT-TYPE-BANK))))
        (gnc:option-set-value option new-acclist)
        (test-equal "account list form" ;;fails
                    (test-template (GncOption-serialize option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-account-list-option-to-scheme"))

  (define (test-gnc-account-sel-option-to-scheme book)
    (test-begin "test-gnc-account-sel-option-to-scheme")
    (let ((odb (gnc:new-options))
          (bank (gnc-account-lookup-by-name(gnc-book-get-root-account book)
                                                "Bank")))
      (gnc:register-option odb
                           (gnc:make-account-sel-option
                            "foo" "bar" "a" "baz" (lambda () '())
                            (lambda (ac)
                              (let ((type (xaccAccountGetType ac)))
                                (or (eqv? type ACCT-TYPE-STOCK)
                                    (eqv? type ACCT-TYPE-BANK))))))
      (test-equal "account sel unchanged" test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-string-output-template))
        (gnc:option-set-value option bank)
        (test-equal "account sel form" ;; fails
                    (test-template (GncOption-serialize option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-account-sel-option-to-scheme"))

  (let* ((book (gnc-option-test-book-new))
         (root-account (gnc-account-create-root book)))
    (test-group-with-cleanup "test-gnc-account-options-to-schemes"
                             (make-account-tree book root-account)
                             (test-gnc-account-list-option-to-scheme book)
                             (test-gnc-account-sel-option-to-scheme book)
                             (cleanup book root-account))))


(define (test-gnc-multichoice-option-to-scheme)
  (test-begin "test-gnc-multichoice-option-to-scheme")
  (let ((odb (gnc:new-options))
        (test-template test-list-output-template)
        (valuenum 1)
        (valuestr "two")
        (valuelstr "two plus three")
        (valuesym 'four)
        (valuelsym (string->symbol "three plus three")))
    (gnc:register-option
     odb
     (gnc:make-multichoice-option
          "foo" "bar" "baz" "Phoney Option" 3
          (list (vector 'all "All")
          (vector 1 "1") (vector "two" "Two") (vector 3 "3")
          (vector 'four "4") (vector "two plus three" "5")
          (vector  (string->symbol "three plus three") "6"))))
    (test-equal "multichoice unchanged" test-unchanged-section-output-template
                (gnc:generate-restore-forms odb "options"))
    (let ((option (gnc:lookup-option odb "foo" "bar")))
      (gnc:option-set-value option valuenum)
      (test-equal "multichoice number key" (test-literal-output-template
                                            (GncOption-serialize option))
                    (gnc:generate-restore-forms odb "options"))
      (gnc:option-set-value option valuestr)
      (test-equal "multichoice simple string key" (test-template (GncOption-serialize option))
                      (gnc:generate-restore-forms odb "options"))
      (gnc:option-set-value option valuelstr)
      (test-equal "multichoice long string key" (test-template (GncOption-serialize option))
                        (gnc:generate-restore-forms odb "options"))
      (gnc:option-set-value option valuesym)
      (test-equal "multichoice symbol key" (test-template
                                            (string->symbol (GncOption-serialize option)))
                          (gnc:generate-restore-forms odb "options"))
      (gnc:option-set-value option valuelsym)
      (test-equal "multichoice long symbol key" (test-template
                                                 (string->symbol (GncOption-serialize option)))
      (gnc:generate-restore-forms odb "options"))))
  (test-end "test-gnc-multichoice-option-to-scheme"))

(define (test-gnc-list-option-to-scheme)
    (test-begin "test-gnc-list-option-to-scheme")
    (let ((odb (gnc:new-options))
          (choices (list (vector 'good "The Good")
                         (vector 'bad  "The Bad")
                         (vector 'ugly "The Ugly"))))
      (gnc:register-option odb
                           (gnc:make-list-option
                            "foo" "bar" "a" "baz" '(bad) choices))
      (test-equal "list unchanged" test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-literal-output-template))
        (gnc:option-set-value option '(ugly))
        (test-equal "list form" (test-template (GncOption-serialize option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-list-option-to-scheme"))

(define (test-gnc-number-range-option-to-scheme)
    (test-begin "test-gnc-number-range-option-to-scheme")
    (let ((odb (gnc:new-options))
          (min-value 0.0)
          (max-value 100.0)
          (dec-places 2.0)
          (step 0.10))
      (gnc:register-option odb
                           (gnc:make-number-range-option
                            "foo" "bar" "a" "baz" 49.0 min-value
                            max-value dec-places step))
      (test-equal "number-range unchanged" test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-number-range-output-template))
        (gnc:option-set-value option 42.0)
        (test-equal "number-range form"
                    (test-template (string->number (GncOption-serialize option)))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-number-range-option-to-scheme"))

(define (test-gnc-number-plot-size-option-to-scheme)
    (test-begin "test-gnc-number-plot-size-option-to-scheme")
    (let ((odb (gnc:new-options))
          (min-value 10)
          (max-value 100)
          (dec-places 0)
          (step 5))
      (gnc:register-option odb
                           (gnc:make-number-plot-size-option
                            "foo" "bar" "a" "baz" 49 min-value
                            max-value dec-places step))
      (test-equal "number-plot unchanged" test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-number-range-output-template))
        (gnc:option-set-value option 48)
        (test-equal "number-plot form"
                    (test-template (string->number (GncOption-serialize option)))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-number-plot-size-option-to-scheme"))

(define (test-gnc-query-option-to-scheme)
  (define query-unchanged-section-output-template
    "
; Section: __reg

"
    )

  (define (query-literal-output-template value)
    (format #f "
; Section: __reg

(let ((option (gnc:lookup-option options
                                 \"__reg\"
                                 \"query\")))
  ((lambda (o) (if o (gnc:option-set-value o '~a))) option))

" value))
 
  (test-begin "test-gnc-query-option-to-scheme")
  (let ((odb (gnc:new-options))
        (query-scm '(query-v2
                     (terms (((("book" "guid") #f guid 3 1 ("3a5a4bc736d84b879b776ea8caadd3b2"))
                              (("account" "guid") #f guid 3 1 ("b7e4ca23652049fca62a0e4f95296a15")))))
                     (search-for Split)
                     (primary-sort (("QofQueryDefaultSort") 0 #t))
                     (secondary-sort #f)
                     (tertiary-sort #f)
                     (max-results -1))))
    (gnc:register-option odb
                         (gnc:make-query-option "__reg" "query" '()))
      (test-equal "query unchanged" query-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "__reg" "query"))
            (test-template query-literal-output-template))
        (gnc:option-set-value option (gnc-scm2query query-scm))
        (test-equal  "query form" (test-template (GncOption-get-value option))
                    (gnc:generate-restore-forms odb "options"))
        ))
  (test-end "test-gnc-query-option-to-scheme"))

(define (test-gnc-color-option-to-scheme)
  (define (test-color-output-template value)
    (let* ((len (string-length value))
           (red (string->number (substring/shared value 0 2) 16))
           (blue (string->number (substring/shared value 2 4) 16))
           (green (string->number (substring/shared value 4 6) 16))
           (alpha (if (> len 7)
                      (string->number (substring/shared value 6 8) 16)
                      #xff)))
    (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (o) (if o (gnc:option-set-value o '(~f ~f ~f ~f)))) option))

" red blue green alpha)))
    (test-begin "test-gnc-coloroption-to-scheme")
    (let ((odb (gnc:new-options))
          (default-color (list #xb2 #x22 #x22 #xff))
          (new-color (list #x00 #xca #x3b #xff)))
      (gnc:register-option odb
                           (gnc:make-color-option
                            "foo" "bar" "a" "baz" default-color #f #t))
      (test-equal "color unchanged" test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-color-output-template))
        (gnc:option-set-value option new-color)
        (test-equal "color form"
                    (test-template (GncOption-serialize option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-color-option-to-scheme"))

(define (test-gnc-invoice-option-to-scheme)
  (test-begin "test-gnc-invoice-option-to-scheme")
  (let ((odb (gnc:new-options)))
       (gnc:register-option odb
                           (gnc:make-invoice-option "foo" "bar" "a" "baz"
                                                  (lambda () '()) (lambda () #t)))
      (test-equal "invoice unchanged" test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let* ((book (gnc-get-current-book))
            (inv (gncInvoiceCreate book))
            (option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-string-output-template))
        (gnc:option-set-value option inv)
        (test-equal "invoice form" (test-template (GncOption-serialize option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-invoice-option-to-scheme"))

(define (test-gnc-owner-option-to-scheme)
  (test-begin "test-owner-option-to-scheme")
    (let ((odb (gnc:new-options)))
      (gnc:register-option odb
                           (gnc:make-owner-option "foo" "bar" "a" "baz"
                                                  (lambda () '()) (lambda () #t)
                                                  GNC-OWNER-CUSTOMER))
      (test-equal "owner unchanged" test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let* ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-list-output-template)
            (book (gnc-get-current-book))
            (owner (gncOwnerNew)))
        (gncOwnerInitCustomer owner (gncCustomerCreate book))
        (gnc:option-set-value option owner)
        (test-equal "owner form"
                    (test-template (cons (gncOwnerGetType owner)
                                         (gncOwnerReturnGUID owner)))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-owner-option-to-scheme"))

(define (test-gnc-internal-option-to-scheme)
  (define (test-output-template name value)
    (format #f "
(let ((option (gnc:lookup-option options
                                 \"__reg\"
                                 ~s)))
  ((lambda (o) (if o (gnc:option-set-value o ~s))) option))
" name value))
  (test-begin "test-gnc-internal-option-to-scheme")
  (let ((odb (gnc:new-options))
        (option-b (gnc:make-internal-option "__reg" "bar" #f))
        (option-s (gnc:make-internal-option "__reg" "baz" "waldo")))
    (gnc:register-option odb option-b)
    (gnc:register-option odb option-s)
    (test-equal "Internal unchanged" "
; Section: __reg

"
                (gnc:generate-restore-forms odb "options"))
    (gnc:option-set-value (gnc:lookup-option odb "__reg" "bar") #t)
    (gnc:option-set-value (gnc:lookup-option odb "__reg" "baz") "pepper")
    (test-equal "internal form" (format #f "
; Section: __reg
~a~a
"
                                        (test-output-template "bar" #t)
                                        (test-output-template "baz" "pepper"))
                (gnc:generate-restore-forms odb "options"))
                )
  (test-end "test-gnc-internal-option-to-scheme"))

;; The following are saved only to KVP, no Scheme generator needed:
;;(define (test-gnc-dateformat-option-to-scheme)
;;(define (test-gnc-taxtable-option-to-scheme)
;;(define (test-gnc-counter-option-to-scheme)
;;(define (test-gnc-counter-format-option-to-scheme)
