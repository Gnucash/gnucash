 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; test-gnc-option.scm -- unit tests for GncOption class.           ;
 ; Copyright (C) 2019 John Ralls <jralls@ceridwen.us>               ;
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

;; Load the C++ option implementation, avoiding the options.scm ones.
(eval-when
 (compile load eval expand)
 (load-extension "libgnucash-guile" "scm_init_sw_app_utils_module"))

(use-modules (gnucash engine))
(use-modules (sw_app_utils))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-gnc-optiondb-scheme")
  (test-gnc-make-text-option)
  (test-gnc-make-account-list-options)
  (test-gnc-make-multichoice-option)
  (test-gnc-make-list-option)
  (test-gnc-make-date-option)
  (test-gnc-make-date-set-option)
  (test-gnc-make-number-range-option)
  (test-gnc-make-report-placement-option)
  (test-end "test-gnc-optiondb-scheme"))

(define (test-gnc-make-text-option)
  (test-begin "test-gnc-test-string-option")
  (let* ((option-db (gnc-new-optiondb))
         (string-opt (gnc-register-string-option option-db "foo" "bar" "baz"
                                                 "Phony Option" "waldo")))
    (test-equal "waldo" (gnc-option-value option-db "foo" "bar"))

    (gnc-set-option option-db "foo" "bar" "pepper")
    (test-equal "pepper" (gnc-option-value option-db "foo" "bar")))
  (test-end "test-gnc-test-string-option"))

(define (test-gnc-make-account-list-options)
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

  (define (test-make-account-list-option book)
    (test-group "test-make-account-list-option"
    (let ((option-db (gnc-new-optiondb))
          (acctlist (gnc-account-list-from-types book
                               (list ACCT-TYPE-STOCK))))
      (gnc-register-account-list-option option-db "foo" "bar" "baz"
                                        "Phony Option" acctlist)
      (let ((acct-list (gnc-option-value option-db "foo" "bar")))
        (test-equal (length acctlist) (length acct-list))
        (test-equal (car acctlist) (car acct-list))) )))

  (define (test-make-account-list-limited-option book)
    (test-group "test-make-account-list-limited-option"
    (let ((option-db (gnc-new-optiondb))
          (acctlist (gnc-account-list-from-types book
                               (list ACCT-TYPE-STOCK))))
      (gnc-register-account-list-limited-option ;; Error not account type twice
       option-db "foo" "bar" "baz"
       "Phony Option" acctlist (list ACCT-TYPE-STOCK))
      (let ((acct-list (gnc-option-value option-db "foo" "bar")))
        (test-equal (length acctlist) (length acct-list)) ;; fails acct-list 4 vs #f
        (test-equal (cadr acctlist) (cadr acct-list)))    ;; fails () vs. #f both wrong
      (gnc-register-account-list-limited-option
       option-db "waldo" "pepper" "baz"
       "Phony Option" acctlist (list ACCT-TYPE-BANK))
      (let ((acct-list (gnc-option-value option-db "waldo" "pepper")))
        (test-error 'wrong-type-arg (length acct-list))))))

  (define (test-make-account-sel-limited-option book)
    (test-group "test-make-account-list-option"
    (let ((option-db (gnc-new-optiondb))
          (acctlist (gnc-account-list-from-types book
                               (list ACCT-TYPE-STOCK))))
      (gnc-register-account-sel-limited-option
       option-db "salt" "pork" "baz"
       "Phony Option" (cadr acctlist) (list ACCT-TYPE-STOCK))
      (let ((acct (gnc-option-value option-db "salt" "pork")))
        (test-equal (cadr acctlist) acct)))))

  (let* ((book (gnc-option-test-book-new))
         (root-account (gnc-account-create-root book)))
    (test-group-with-cleanup "test-gnc-make-account-list-options"
                             (make-account-tree book root-account)
                             (test-make-account-list-option book)
                             (test-make-account-list-limited-option book)
                             (test-make-account-sel-limited-option book)
                             (cleanup book root-account))))


(define (test-gnc-make-multichoice-option)

  (define (keylist->vectorlist keylist)
  (map
   (lambda (item)
     (vector
      (car item)
      (keylist-get-info keylist (car item) 'text)
      (keylist-get-info keylist (car item) 'tip)))
   keylist))

  (define (keylist-get-info keylist key info)
  (assq-ref (assq-ref keylist key) info))

  (test-begin "test-gnc-test-multichoice-option")
  (let* ((option-db (gnc-new-optiondb))
         (multilist (list
                       (list "plugh" (cons 'text "xyzzy") (cons 'tip "thud"))
                       (list 'waldo (cons 'text "pepper") (cons 'tip "salt"))
                       (list "pork" (cons 'text "sausage") (cons 'tip "links"))
                       (list "corge" (cons 'text "grault") (cons 'tip "garply"))))
         (multichoice (keylist->vectorlist multilist))
         (multi-opt (gnc-register-multichoice-option
                     option-db "foo" "bar" "baz"
                     "Phony Option" "waldo" multichoice)))

    (test-equal 'waldo (gnc-option-value option-db "foo" "bar"))
    (gnc-set-option option-db "foo" "bar" "corge")
    (test-equal "corge" (gnc-option-value option-db "foo" "bar"))
    (test-equal 'waldo (gnc-option-default-value option-db "foo" "bar")))
    (test-end "test-gnc-test-multichoice-option"))

(define (test-gnc-make-list-option)
  (test-begin "test-gnc-test-list-option")
  (let* ((option-db (gnc-new-optiondb))
         (value-list (list (vector "AvgBalPlot" "Average" "Average Balance")
                           (vector "GainPlot" "Profit" "Profit (Gain minus Loss)")
                           (vector "GLPlot" "Gain/Loss" "Gain and Loss")))
         (list-op (gnc-register-list-option option-db "foo" "bar" "baz"
                                            "Phony Option" "AvgBalPlot"
                                            value-list)))
    (test-equal '("AvgBalPlot") (gnc-option-value option-db "foo" "bar"))
    (gnc-set-option option-db "foo" "bar" '("GainPlot" "GLPlot"))
    (test-equal '("GainPlot" "GLPlot") (gnc-option-value option-db "foo" "bar"))
    (test-equal '("AvgBalPlot") (gnc-option-default-value option-db "foo" "bar")))
  (test-end "test-gnc-test-list-option"))

(define (test-gnc-make-date-option)
  (test-begin "test-gnc-test-date-option")
  (let* ((option-db (gnc-new-optiondb))
         (date-opt (gnc-register-date-option option-db "foo" "bar"
                                             "baz" "Phony Option"
                                             (RelativeDatePeriod-TODAY)))
         (a-time (gnc-dmy2time64 11 07 2019)))
    (test-equal '(relative . today) (gnc-option-value option-db "foo" "bar"))
    (gnc-set-option option-db "foo" "bar" a-time)
    (test-equal `(absolute . ,a-time) (gnc-option-value option-db "foo" "bar")))
  (test-end "test-gnc-test-date-option"))

(define (test-gnc-make-date-set-option)
  (test-begin "test-gnc-test-date-set-option")
  (let* ((option-db (gnc-new-optiondb))
         (date-opt (gnc-register-date-option-set
                    option-db "foo" "bar" "baz" "Phony Option"
                    '(today
                          start-this-month
                          start-prev-month
                          start-current-quarter
                          start-prev-quarter
                          start-cal-year
                          start-cal-year
                          start-prev-year
                          start-accounting-period) #t)))
    (test-equal '(relative . start-accounting-period)
                (gnc-option-value option-db "foo" "bar")))
  (test-end "test-gnc-test-date-set-option"))

(define (test-gnc-make-number-range-option)
  (test-begin "test-gnc-number-range-option")
  (let* ((option-db (gnc-new-optiondb))
         (number-opt (gnc-register-number-range-option option-db "foo" "bar"
                                                       "baz" "Phony Option"
                                                       15 5 30 1)))
    (test-equal 15.0 (gnc-option-value option-db "foo" "bar"))
    (gnc-set-option option-db "foo" "bar" 20)
    (test-equal 20.0 (gnc-option-value option-db "foo" "bar")))
  (test-end "test-gnc-number-range-option"))

(define (test-gnc-make-report-placement-option)
  (test-begin "test-gnc-report-placement-option")
    (let* ((report1 123)
           (report2 456)
           (rp (list (list report1 2 3) (list report2 3 2)))
           (option-db (gnc-new-optiondb)))
           (gnc-register-report-placement-option option-db "foo" "bar")
           (gnc-set-option option-db "foo" "bar" rp)
           (test-equal report2 (car (cadr (gnc-option-value option-db "foo" "bar")))))
  (test-end "test-gnc-report-placement-option"))
