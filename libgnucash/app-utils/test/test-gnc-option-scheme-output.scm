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

(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash app-utils options))
(use-modules (sw_app_utils))
(use-modules (sw_engine))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-gnc-option-scheme-io")
  (test-gnc-string-option-to-scheme)
  (test-gnc-text-option-to-scheme)
  (test-gnc-pixmap-option-to-scheme)
  (test-gnc-currency-option-to-scheme)
  (test-gnc-budget-option-to-scheme)
  (test-gnc-font-option-to-scheme)
  (test-gnc-commodity-option-to-scheme)
  (test-gnc-date-option-to-scheme)
  (test-gnc-multichoice-option-to-scheme)
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
  ((lambda (o) (if o (gnc:option-set-value o '~s))) option))

" value))

(define (test-budget-output-template value)
  (format #f "
; Section: foo

(let ((option (gnc:lookup-option options
                                 \"foo\"
                                 \"bar\")))
  ((lambda (option) (if option ((gnc:option-setter option) (gnc-budget-lookup ~s (gnc-get-current-book))))) option))

"
          (gncBudgetGetGUID value)))


(define (test-option-scheme-output make-option-func test-template default value)
  (let ((odb (gnc:new-options))
        (option (make-option-func "foo" "bar" "baz" "Test Option" default)))
    (gnc:register-option odb option)
    (test-equal test-unchanged-section-output-template
                (gnc:generate-restore-forms odb "options"))
    (gnc:option-set-value (gnc:lookup-option odb "foo" "bar") value)
    (test-equal (test-template value)
                (gnc:generate-restore-forms odb "options"))))

(define (test-gnc-string-option-to-scheme)
  (test-begin "test-gnc-string-option-to-scheme")
  (test-option-scheme-output gnc:make-string-option test-string-output-template
                             "waldo" "pepper")
  (test-end "test-gnc-string-option-to-scheme"))

(define (test-gnc-text-option-to-scheme)
  (test-begin "test-gnc-text-option-to-scheme")
  (test-option-scheme-output gnc:make-string-option test-string-output-template
                             ""
"Sed ut perspiciatis, unde omnis iste natus error sit voluptatem accusantium
doloremque laudantium, totam rem aperiam eaque ipsa, quae ab illo inventore
veritatis et quasi architecto beatae vitae dicta sunt, explicabo.")
  (test-end "test-gnc-text-option-to-scheme"))

(define (test-gnc-font-option-to-scheme)
  (test-begin "test-gnc-font-option-to-scheme")
  (test-option-scheme-output gnc:make-font-option test-string-output-template
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
      (test-option-scheme-output gnc:make-currency-option test-literal-output-template
                               USD EUR)
;; Garbage collection has already eaten USD and EUR.
      (test-book-clear-data book "gnc-commodity-table")
      (gnc-commodity-table-destroy table)
      (gnc-clear-current-session)))
  (test-end "test-gnc-currency-option-to-scheme"))

(define (test-gnc-budget-option-to-scheme)
 (test-begin "test-gnc-budget-option-to-scheme")
  (let* ((session (gnc-get-current-session))
        (book (gnc-get-current-book))
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
    (test-equal test-unchanged-section-output-template
                (gnc:generate-restore-forms odb "options"))
    (gnc:option-set-value (gnc:lookup-option odb "foo" "bar") budget2)
    (test-equal (gnc-budget-get-default book) budget1)
    (test-equal (test-budget-output-template budget2)
                (gnc:generate-restore-forms odb "options")))
    (gnc-clear-current-session))
  (test-end "test-gnc-budget-option-to-scheme"))

(define (test-gnc-commodity-option-to-scheme)
  (test-begin "test-gnc-commodity-option-to-scheme")
  (let* ((book (gnc-option-test-book-new))
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (FMAGX (gnc-commodity-new book "Fidelity Magellan Fund" "FUND" "FMAGX" "" 1000)))
  (test-option-scheme-output gnc:make-commodity-option test-literal-output-template
                             AAPL FMAGX))
  (test-end "test-gnc-commodity-option-to-scheme"))

(define (test-gnc-bool-option-to-scheme)
  (test-begin "test-gnc-bool-option-to-scheme")
  (test-option-scheme-output gnc:make-simple-boolean-option test-string-output-template #f #t)
  (test-end "test-gnc-bool-option-to-scheme"))

(define (test-gnc-pixmap-option-to-scheme)
  (test-begin "test-gnc-pixmap-option-to-scheme")
  (test-option-scheme-output gnc:make-pixmap-option test-string-output-template "" "~/mybusiness/mylogo.png")
  (test-end "test-gnc-pixmap-option-to-scheme"))

(define (test-gnc-date-option-to-scheme)
  (test-begin "test-gnc-date-option-to-scheme")
  (let ((odb (gnc:new-options)))
    (gnc:options-make-end-date! odb "foo" "bar" "baz" "Phoney Option")
    (test-equal test-unchanged-section-output-template
                (gnc:generate-restore-forms odb "options"))
    (let* ((option (gnc:lookup-option odb "foo" "bar"))
          (test-template test-literal-output-template)
          (time (gnc-dmy2time64 25 12 2020))
          (value `(absolute . ,time)))
      (gnc:option-set-value option value)
      (test-equal (test-template (GncOption-get-scm-value option))
                  (gnc:generate-restore-forms odb "options"))
      (set! value '(relative . end-prev-year))
      (gnc:option-set-value option value)
      (test-equal value (GncOption-get-scm-value option))
      (test-equal (test-template (GncOption-get-scm-value option))
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

  (define (test-gnc-account-list-option-to-scheme)
    (test-begin "test-gnc-account-list-option-to-scheme")
    (let ((odb (gnc:new-options))
          (acctlist (gnc-account-list-from-types book
                                                 (list ACCT-TYPE-STOCK))))
      (gnc-register-option odb
                           (gnc:make-account-list-option
                            "foo" "bar" "a" "baz" acctlist
                            (lambda (ac)
                              (let ((type (xaccAccountGetAccountType ac)))
                                (or (eq type ACCT-TYPE-STOCK)
                                    (eq type ACCT-TYPE-BANK)))) #t))
      (test-equal test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-literal-output-template)
            (new-acclist (gnc-account-list-from-types book (list ACCT-TYPE-BANK))))
        (gnc-option-set-value option new-acclist)
        (test-equal (test-template (GncOption-get-scm-value option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-account-list-option-to-scheme"))

  (define (test-gnc-account-sel-option-to-scheme)
    (test-begin "test-gnc-account-sel-option-to-scheme")
    (let ((odb (gnc:new-options))
          (acctlist (gnc-account-list-from-types book
                                                 (list ACCT-TYPE-STOCK))))
      (gnc-register-option odb
                           (gnc:make-account-list-option
                            "foo" "bar" "a" "baz" acctlist
                            (lambda (ac)
                              (let ((type (xaccAccountGetAccountType ac)))
                                (or (eq type ACCT-TYPE-STOCK)
                                    (eq type ACCT-TYPE-BANK)))) #t))
      (test-equal test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-literal-output-template)
            (new-acclist (gnc-account-list-from-types book (list ACCT-TYPE-BANK))))
        (gnc-option-set-value option new-acclist)
        (test-equal (test-template (GncOption-get-scm-value option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-account-sel-option-to-scheme"))

  (let* ((book (gnc-option-test-book-new))
         (root-account (gnc-account-create-root book)))
    (test-group-with-cleanup "test-gnc-account-options-to-schemes"
                             (make-account-tree book root-account)
                             (test-gnc-account-list-option-to-scheme)
                             (test-gnc-account-sel-option-to-scheme)
                             (cleanup book root-account))))


(define (test-gnc-multichoice-option-to-scheme)
  (test-begin "test-gnc-multichoice-option-to-scheme")
  (let ((odb (gnc:new-options))
        (test-template test-literal-output-template)
        (value "5"))
    (gnc:register-option
     odb
     (gnc:make-multichoice-option
          "foo" "bar" "baz" "Phoney Option" 3
          (list (vector 'all "All")
          (vector 1 "1") (vector 2 "2") (vector 3 "3")
          (vector 4 "4") (vector 5 "5") (vector 6 "6"))))
    (test-equal test-unchanged-section-output-template
                (gnc:generate-restore-forms odb "options"))
    (let ((option (gnc:lookup-option odb "foo" "bar")))
      (gnc:option-set-value option value)
      (test-equal (test-template (GncOption-get-scm-value option))
                  (gnc:generate-restore-forms odb "options"))))
  (test-end "test-gnc-multichoice-option-to-scheme"))

(define (test-gnc-list-option-to-scheme)
    (test-begin "test-gnc-list-option-to-scheme")
    (let ((odb (gnc:new-options))
          (choices (list (vector 'good "The Good")
                         (vector 'bad  "The Bad")
                         (vector 'ugly "The Ugly"))))
      (gnc-register-option odb
                           (gnc:make-list-option
                            "foo" "bar" "a" "baz" '(bad) choices))
      (test-equal test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-literal-output-template))
        (gnc-option-set-value option '(ugly))
        (test-equal (test-template (GncOption-get-scm-value option))
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
      (gnc-register-option odb
                           (gnc:make-number-range-option
                            "foo" "bar" "a" "baz" 49.0 min-value
                            max-value dec-places step))
      (test-equal test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-literal-output-template))
        (gnc-option-set-value option 42.0)
        (test-equal (test-template (GncOption-get-scm-value option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-number-range-option-to-scheme"))

(define (test-gnc-number-plot-size-option-to-scheme)
    (test-begin "test-gnc-number-plot-size-option-to-scheme")
    (let ((odb (gnc:new-options))
          (min-value 100)
          (max-value 10000)
          (dec-places 0)
          (step 5))
      (gnc-register-option odb
                           (gnc:make-number-plot-size-option
                            "foo" "bar" "a" "baz" 490 min-value
                            max-value dec-places step))
      (test-equal test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-literal-output-template))
        (gnc-option-set-value option 420)
        (test-equal (test-template (GncOption-get-scm-value option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-number-plot-size-option-to-scheme"))

(define (test-gnc-query-option-to-scheme)
  (test-begin "test-gnc-number-plot-size-option-to-scheme")
  (let ((odb (gnc:new-options))
        (query-scm '(query-v2
                     (terms (((("book" "guid") #f guid 3 1 ("3a5a4bc736d84b879b776ea8caadd3b2"))
                              (("account" "guid") #f guid 3 1 ("b7e4ca23652049fca62a0e4f95296a15")))))
                     (search-for Split)
                     (primary-sort (("QofQueryDefaultSort") 0 #t))
                     (secondary-sort #f)
                     (tertiary-sort #f)
                     (max-results -1))))
    (gnc-register-option odb
                         (gnc:make-query-option "__reg" "query" '()))
      (test-equal test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "__reg" "query"))
            (test-template test-literal-output-template))
        (gnc-option-set-value option (gnc-scm2query query-scm))
        (test-equal (test-template (GncOption-get-scm-value option))
                    (gnc:generate-restore-forms odb "options"))
        ))
  (test-end "test-gnc-number-plot-size-option-to-scheme"))

(define (test-gnc-color-option-to-scheme)
    (test-begin "test-gnc-coloroption-to-scheme")
    (let ((odb (gnc:new-options))
          (default-color (list #xb2 #x22 $x22 #xff))
          (new-color (list #x00 #xca #x3b #xff)))
      (test-option-scheme-output gnc:make-color-option
                                 test-literal-output-template
                                 default-color new-color))
    (test-end  "test-gnc-color-option-to-scheme"))

(define (test-gnc-invoice-option-to-scheme)
    (test-begin "test-gnc-invoice-option-to-scheme")
    (let ((odb (gnc:new-options))
          (invoice '"13b305236443451a86c5366b7f890ecb"))
      (test-option-scheme-output gnc:make-color-option
                                 test-literal-output-template
                                 (lambda () '()) invoice))
    (test-end  "test-gnc-invoice-option-to-scheme"))

(define (test-gnc-owner-option-to-scheme)
    (test-begin "test-owner-option-to-scheme")
    (let ((odb (gnc:new-options)))
      (gnc-register-option odb
                           (gnc:make-owner-option "foo" "bar" "a" "baz"
                                                  (lambda () '()) #f
                                                  'GNC-OWNER-CUSTOMER))
      (test-equal test-unchanged-section-output-template
                  (gnc:generate-restore-forms odb "options"))
      (let ((option (gnc:lookup-option odb "foo" "bar"))
            (test-template test-literal-output-template))
        (gnc-option-set-value option '"13b305236443451a86c5366b7f890ecb")
        (test-equal (test-template (GncOption-get-scm-value option))
                    (gnc:generate-restore-forms odb "options"))
        ))
    (test-end  "test-gnc-owner-option-to-scheme"))

;; The following are saved only to KVP, no Scheme generator needed:
;;(define (test-gnc-dateformat-option-to-scheme)
;;(define (test-gnc-taxtable-option-to-scheme) 
;;(define (test-gnc-counter-option-to-scheme)
;;(define (test-gnc-counter-format-option-to-scheme)
