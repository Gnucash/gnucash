;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash engine))
(use-modules (sw_engine))

(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report standard-reports cashflow-barchart))
(use-modules (gnucash report stylesheets))

(use-modules (ice-9 format))
(use-modules (ice-9 streams))
(use-modules (srfi srfi-1))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
  (and (test-in-txn)
       (test-out-txn)
       (test-null-txn)))


(define (set-option report page tag value)
  ((gnc:option-setter (gnc:lookup-option (gnc:report-options report)
                                         page tag)) value))


(define constructor (record-constructor <report>))


(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Asset"
              (list "Bank")
              (list "Wallet"))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))))


;; Test two transactions from income to two different assets in two different days
(define (test-in-txn)
  (let* ((template (gnc:find-report-template cashflow-barchart-uuid))
         (options (gnc:make-report-options cashflow-barchart-uuid))
         (report (constructor cashflow-barchart-uuid "bar" options
                              #t #t #f #f ""))
         (renderer (gnc:report-template-renderer template)))
    (let* ((env (create-test-env))
           (account-alist (env-create-account-structure-alist env structure))
           (bank-account (cdr (assoc "Bank" account-alist)))
           (wallet-account (cdr (assoc "Wallet" account-alist)))
           (expense-account (cdr (assoc "Expenses" account-alist)))
           (income-account (cdr (assoc "Income" account-alist)))
           (date-0 (gnc:get-start-this-month))
           (date-1 (gnc:time64-next-day date-0))
           (date-2 (gnc:time64-next-day date-1)))
      (env-create-transaction env
                              date-1
                              bank-account
                              income-account
                              1/1)
      (env-create-transaction env
                              date-2
                              wallet-account
                              income-account
                              5/1)
      (begin
        (set-option report gnc:pagename-display "Show Table" #t)
        (set-option report gnc:pagename-general "Start Date" (cons 'absolute date-0))
        (set-option report gnc:pagename-general "End Date" (cons 'absolute date-2))
        (set-option report gnc:pagename-general "Step Size" 'DayDelta)
        (set-option report gnc:pagename-general "Price Source" 'pricedb-nearest)
        (set-option report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
        (set-option report gnc:pagename-accounts "Accounts" (list wallet-account bank-account))
        ;; (format #t "Create first transaction on ~a~%" (gnc-ctime date-1))
        ;; (format #t "Create second transaction on ~a~%" (gnc-ctime date-2))
        (let ((doc (renderer report)))
          (gnc:html-document-set-style-sheet! doc (gnc:report-stylesheet report))
          (let* ((result (gnc:html-document-render doc #f))
                 (tbl (stream->list
                       (pattern-streamer "<tr>"
					 (list (list "<td>([0-9]+)/([0-9]+)/([0-9]+)</td>"
						     1 2 3)
                                               (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
					       (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
					       (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1))
					 result)))
                 (total (stream->list
                         (pattern-streamer "<tr><td>Total</td>"
                                           (list (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
                                                 (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
                                                 (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1))
                                           result))))
            ;; (format #t "Report Result ~a~%" result)
            (and (every (lambda (row)                 ; test in=net & out=0 in all rows (all days)
                          (and (or (equal? (second row) (fourth row))
                                   (begin (format #t "Failed, ~a and ~a differ~%" (second row) (fourth row)) #f))
                               (or (= 0 (string->number (car (third row))))
                                   (begin (format #t "Failed ~d isn't 0~%" (car (third row))) #f))))
                        tbl)
                 (or (= 0 (tbl-ref->number tbl 0 1))
                     (begin (format #t "Failed refnum ~g isn't 0~%" (tbl-ref->number tbl 0 1)) #f))      ; 1st day in =0
                 (or (= 1 (tbl-ref->number tbl 1 1)) (begin (format #t "Failed refnum ~g isn't 1~%" (tbl-ref->number tbl 1 1)) #f))      ; 2nd day in =1
                 (or (= 5 (tbl-ref->number tbl 2 1)) (begin (format #t "Failed refnum ~g isn't 5~%" (tbl-ref->number tbl 2 1)) #f))     ; 3rd day in =5
                 (or (= (tbl-ref->number total 0 0) (tbl-ref->number total 0 2)) (begin (format #t "Failed refnums ~g and ~g differ ~%" (tbl-ref->number total 0 0) (tbl-ref->number total 0 2)) #f)); total in=total net
                 (or (= 0 (tbl-ref->number total 0 1)) (begin (format #t "Failed refnum ~g isn't 0~%" (tbl-ref->number total 0 1)) #f))   ; total out=0
                 (or (= 3 (tbl-row-count tbl)) (begin (format #t "Failed row count ~g isn't 3~%" (tbl-row-count tbl)) #f))
                 (or (= 4 (tbl-column-count tbl)) (begin (format #t "Failed column count ~g isn't 4~%" (tbl-column-count tbl)) #f))))
        )
      )
    )
  )
)

;; Test two transactions from two different assets to expense in two different days
(define (test-out-txn)
  (let* ((template (gnc:find-report-template cashflow-barchart-uuid))
         (options (gnc:make-report-options cashflow-barchart-uuid))
         (report (constructor cashflow-barchart-uuid "bar" options
                              #t #t #f #f ""))
         (renderer (gnc:report-template-renderer template)))
    (let* ((env (create-test-env))
           (account-alist (env-create-account-structure-alist env structure))
           (bank-account (cdr (assoc "Bank" account-alist)))
           (wallet-account (cdr (assoc "Wallet" account-alist)))
           (expense-account (cdr (assoc "Expenses" account-alist)))
           (income-account (cdr (assoc "Income" account-alist)))
           (date-0 (gnc:get-start-this-month))
           (date-1 (gnc:time64-next-day date-0))
           (date-2 (gnc:time64-next-day date-1)))
      (env-create-transaction env
                              date-1
                              bank-account
                              income-account
                              100/1)   ; large in txn to avoid negative net (hard to parse)
      (env-create-transaction env
                              date-1
                              expense-account
                              bank-account
                              1/1)
      (env-create-transaction env
                              date-2
                              wallet-account
                              income-account
                              100/1)   ; large in txn to avoid negative net (hard to parse)
      (env-create-transaction env
                              date-2
                              expense-account
                              wallet-account
                              5/1)
      (begin
        (set-option report gnc:pagename-display "Show Table" #t)
        (set-option report gnc:pagename-general "Start Date" (cons 'absolute date-0))
        (set-option report gnc:pagename-general "End Date" (cons 'absolute date-2))
        (set-option report gnc:pagename-general "Step Size" 'DayDelta)
        (set-option report gnc:pagename-general "Price Source" 'pricedb-nearest)
        (set-option report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
        (set-option report gnc:pagename-accounts "Accounts" (list wallet-account bank-account))

        (let ((doc (renderer report)))
          (gnc:html-document-set-style-sheet! doc (gnc:report-stylesheet report))
          (let* ((result (gnc:html-document-render doc #f))
                 (tbl (stream->list
                       (pattern-streamer "<tr>"
					 (list (list "<td>([0-9]+)/([0-9]+)/([0-9]+)</td>"
						     1 2 3)
                                               (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
					       (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
					       (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1))
					 result)))
                 (total (stream->list
                         (pattern-streamer "<tr><td>Total</td>"
                                           (list (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
                                                 (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
                                                 (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1))
                                           result))))
            (and (every (lambda (row)                 ; test in-out=net in all rows (all days)
                          (let ((in (string->number (car (second row))))
                                (out (string->number (car (third row))))
                                (net (string->number (car (fourth row)))))
                            (= (- in out) net)))
                        tbl)
                 (= 0 (tbl-ref->number tbl 0 2))      ; 1st day out =0
                 (= 1 (tbl-ref->number tbl 1 2))      ; 2nd day out =1
                 (= 5 (tbl-ref->number tbl 2 2))      ; 3rd day out =5
                 (= (- (tbl-ref->number total 0 0) (tbl-ref->number total 0 1)) ; total in-total out=total net
                    (tbl-ref->number total 0 2))
                 (= 6 (tbl-ref->number total 0 1))    ; total out=6
                 (= 3 (tbl-row-count tbl))
                 (= 4 (tbl-column-count tbl)))))
        )
      )
    )
  )


;; Test null transaction (transaction between assets)
;; This test is identical to test-in-txn but with an extra transaction between assets
(define (test-null-txn)
  (let* ((template (gnc:find-report-template cashflow-barchart-uuid))
         (options (gnc:make-report-options cashflow-barchart-uuid))
         (report (constructor cashflow-barchart-uuid "bar" options
                              #t #t #f #f ""))
         (renderer (gnc:report-template-renderer template)))
    (let* ((env (create-test-env))
           (account-alist (env-create-account-structure-alist env structure))
           (bank-account (cdr (assoc "Bank" account-alist)))
           (wallet-account (cdr (assoc "Wallet" account-alist)))
           (expense-account (cdr (assoc "Expenses" account-alist)))
           (income-account (cdr (assoc "Income" account-alist)))
           (date-0 (gnc:get-start-this-month))
           (date-1 (gnc:time64-next-day date-0))
           (date-2 (gnc:time64-next-day date-1)))
      (env-create-transaction env
                              date-1
                              bank-account
                              income-account
                              1/1)
      (env-create-transaction env
                              date-1
                              bank-account
                              wallet-account
                              20/1)  ; this transaction should not be counted
      (env-create-transaction env
                              date-2
                              wallet-account
                              income-account
                              5/1)

      (begin
        (set-option report gnc:pagename-display "Show Table" #t)
        (set-option report gnc:pagename-general "Start Date" (cons 'absolute date-0))
        (set-option report gnc:pagename-general "End Date" (cons 'absolute date-2))
        (set-option report gnc:pagename-general "Step Size" 'DayDelta)
        (set-option report gnc:pagename-general "Price Source" 'pricedb-nearest)
        (set-option report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
        (set-option report gnc:pagename-accounts "Accounts" (list wallet-account bank-account))

        (let ((doc (renderer report)))
          (gnc:html-document-set-style-sheet! doc (gnc:report-stylesheet report))
          (let* ((result (gnc:html-document-render doc #f))
                 (tbl (stream->list
                       (pattern-streamer "<tr>"
					 (list (list "<td>([0-9]+)/([0-9]+)/([0-9]+)</td>"
						     1 2 3)
                                               (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
					       (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
					       (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1))
					 result)))
                 (total (stream->list
                         (pattern-streamer "<tr><td>Total</td>"
                                           (list (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
                                                 (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1)
                                                 (list "<td class=\"number-cell\">[^0-9]*([^<]*)</td>" 1))
                                           result))))
            (and (every (lambda (row)                 ; test in=net & out=0 in all rows (all days)
                          (and (equal? (second row) (fourth row))
                               (= 0 (string->number (car (third row))))))
                        tbl)
                 (= 0 (tbl-ref->number tbl 0 1))      ; 1st day in =0
                 (= 1 (tbl-ref->number tbl 1 1))      ; 2nd day in =1
                 (= 5 (tbl-ref->number tbl 2 1))      ; 3rd day in =5
                 (= (tbl-ref->number total 0 0) (tbl-ref->number total 0 2)) ; total in=total net
                 (= 0 (tbl-ref->number total 0 1))    ; total out=0
                 (= 3 (tbl-row-count tbl))
                 (= 4 (tbl-column-count tbl)))))
        )
      )
    )
  )
