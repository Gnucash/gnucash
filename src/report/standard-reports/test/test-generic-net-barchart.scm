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

(define-module (gnucash report standard-reports test test-generic-net-barchart))

(use-modules (ice-9 format))
(use-modules (ice-9 streams))
(use-modules (srfi srfi-1))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash report report-system test test-extras))

(export run-net-asset-income-test)

(define (set-option report page tag value)
  ((gnc:option-setter (gnc:lookup-option (gnc:report-options report)
					 page tag)) value))


(define constructor (record-constructor <report>))

(define (run-net-asset-income-test asset-report-uuid income-report-uuid)
  (logging-and  (two-txn-test asset-report-uuid)
		(two-txn-test-2 asset-report-uuid)
		(two-txn-test-income income-report-uuid)

		(null-test asset-report-uuid)
		(null-test income-report-uuid)
		(single-txn-test asset-report-uuid)
		(closing-test income-report-uuid)
		#t))

;; Just prove that the report exists.
(define (null-test uuid)
  (let* ((template (gnc:find-report-template uuid))
	 (options (gnc:make-report-options uuid))
	 (report (constructor uuid "bar" options
			      #t #t #f #f ""))
	 (renderer (gnc:report-template-renderer template)))

    (let ((doc (renderer report)))
      (gnc:html-document-set-style-sheet! doc
					  (gnc:report-stylesheet report))
      ;;(format #t "render: ~a\n" (gnc:html-document-render doc #f))
      #t
      )))

(define (single-txn-test uuid)
  (let* ((template (gnc:find-report-template uuid))
	 (options (gnc:make-report-options uuid))
	 (report (constructor uuid "bar" options
				     #t #t #f #f ""))
	 (renderer (gnc:report-template-renderer template)))
    (let* ((env (create-test-env))
	   (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
						      (gnc-default-report-currency)))
	   (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
							(gnc-default-report-currency)))
	   (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
						       (gnc-default-report-currency))))
      (env-create-transaction env
			       (gnc:get-start-this-month)
			       my-income-account
			       my-asset-account
			       (gnc:make-gnc-numeric -1 1))
      (begin
	(set-option report gnc:pagename-display "Show table" #t)
	(set-option report gnc:pagename-general "Start Date"
		    (cons 'absolute (gnc:get-start-this-month)))
	(set-option report gnc:pagename-general "End Date"
		    (cons 'absolute (gnc:get-start-this-month)))
	(set-option report gnc:pagename-general "Step Size" 'DayDelta)
	(set-option report gnc:pagename-general "Price Source" 'pricedb-nearest)
	(set-option report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
	(set-option report gnc:pagename-accounts "Accounts" (list my-asset-account))

	(let ((doc (renderer report)))
	  (gnc:html-document-set-style-sheet! doc
					      (gnc:report-stylesheet report))
	  (let* ((result (gnc:html-document-render doc #f))
		 (tbl (stream->list
		       (pattern-streamer "<tr>"
					 (list (list "<string> ([0-9][0-9])/([0-9][0-9])/([0-9][0-9])</td>"
						     1 2 3)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1))
					 result))))
	    (and (= 1 (tbl-ref->number tbl 0 1))
			 (= 0 (tbl-ref->number tbl 0 2))
			 (= 1 (tbl-ref->number tbl 0 3))
			 (= 1 (tbl-row-count tbl))
			 (= 4 (tbl-column-count tbl)))))))))


(define (two-txn-test uuid)
  (let* ((template (gnc:find-report-template uuid))
	 (options (gnc:make-report-options uuid))
	 (report (constructor uuid "bar" options
				     #t #t #f #f ""))
	 (renderer (gnc:report-template-renderer template)))
    (let* ((env (create-test-env))
	   (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
						      (gnc-default-report-currency)))
	   (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
							(gnc-default-report-currency)))
	   (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
						       (gnc-default-report-currency)))
	   (date-0 (gnc:get-start-this-month))
	   (date-1 (gnc:timepair-next-day date-0))
	   (date-2 (gnc:timepair-next-day date-1)))
      (env-create-transaction env
			       date-1
			       my-income-account
			       my-asset-account
			       (gnc:make-gnc-numeric -1 1))
      (env-create-transaction env
			       date-2
			       my-income-account
			       my-asset-account
			       (gnc:make-gnc-numeric -5 1))
      (begin
	(set-option report gnc:pagename-display "Show table" #t)
	(set-option report gnc:pagename-general "Start Date" (cons 'absolute date-0))
	(set-option report gnc:pagename-general "End Date" (cons 'absolute date-2))
	(set-option report gnc:pagename-general "Step Size" 'DayDelta)
	(set-option report gnc:pagename-general "Price Source" 'pricedb-nearest)
	(set-option report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
	(set-option report gnc:pagename-accounts "Accounts" (list my-asset-account))

	(let ((doc (renderer report)))
	  (gnc:html-document-set-style-sheet! doc
					      (gnc:report-stylesheet report))
	  (let* ((result (gnc:html-document-render doc #f))
		 (tbl (stream->list
		       (pattern-streamer "<tr>"
					 (list (list "<string> ([0-9][0-9])/([0-9][0-9])/([0-9][0-9])</td>"
						     1 2 3)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1))
					 result))))
	    (and (every (lambda (row)
				  (and (equal? (second row) (fourth row))
				       (= 0 (string->number (car (third row))))))
				tbl)
			 (= 0 (tbl-ref->number tbl 0 1))
			 (= 1 (tbl-ref->number tbl 1 1))
			 (= 6 (tbl-ref->number tbl 2 1))
			 (= 3 (tbl-row-count tbl))
			 (= 4 (tbl-column-count tbl)))))))))


(define (two-txn-test-2 uuid)
  (let* ((template (gnc:find-report-template uuid))
	 (options (gnc:make-report-options uuid))
	 (report (constructor uuid "bar" options
				     #t #t #f #f ""))
	 (renderer (gnc:report-template-renderer template)))
    (let* ((env (create-test-env))
	   (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
						      (gnc-default-report-currency)))
	   (my-liability-account (env-create-root-account env ACCT-TYPE-LIABILITY
						      (gnc-default-report-currency)))
	   (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
							(gnc-default-report-currency)))
	   (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
						       (gnc-default-report-currency)))
	   (date-0 (gnc:get-start-this-month))
	   (date-1 (gnc:timepair-next-day date-0))
	   (date-2 (gnc:timepair-next-day date-1)))
      (env-create-transaction env date-1 my-income-account my-asset-account (gnc:make-gnc-numeric -1 1))
      (env-create-transaction env date-1 my-expense-account my-liability-account (gnc:make-gnc-numeric -1 1))
      (env-create-transaction env date-2 my-income-account my-asset-account (gnc:make-gnc-numeric -5 1))
      (env-create-transaction env date-2 my-expense-account my-liability-account (gnc:make-gnc-numeric -5 1))
      (begin
	(set-option report gnc:pagename-display "Show table" #t)
	(set-option report gnc:pagename-general "Start Date" (cons 'absolute date-0))
	(set-option report gnc:pagename-general "End Date" (cons 'absolute date-2))
	(set-option report gnc:pagename-general "Step Size" 'DayDelta)
	(set-option report gnc:pagename-general "Price Source" 'pricedb-nearest)
	(set-option report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
	(set-option report gnc:pagename-accounts "Accounts" (list my-asset-account my-liability-account))

	(let ((doc (renderer report)))
	  (gnc:html-document-set-style-sheet! doc
					      (gnc:report-stylesheet report))
	  (let* ((result (gnc:html-document-render doc #f))
		 (tbl (stream->list
		       (pattern-streamer "<tr>"
					 (list (list "<string> ([0-9][0-9])/([0-9][0-9])/([0-9][0-9])</td>"
						     1 2 3)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1))
					 result))))
	    (and (every (lambda (row)
				  (and (= (string->number (car (fourth row)))
					  (+ (string->number (car (second row)))
					     (string->number (car (third row)))))
				       ;; txns added in pairs, so assets = liability
				       (equal? (second row) (third row))))
				tbl)
			 (= 0 (tbl-ref->number tbl 0 1))
			 (= 1 (tbl-ref->number tbl 1 1))
			 (= 6 (tbl-ref->number tbl 2 1))
			 (= 3 (tbl-row-count tbl))
			 (= 4 (tbl-column-count tbl)))))))))

(define (two-txn-test-income uuid)
  (let* ((template (gnc:find-report-template uuid))
	 (options (gnc:make-report-options uuid))
	 (report (constructor uuid "bar" options
				     #t #t #f #f ""))
	 (renderer (gnc:report-template-renderer template)))
    (let* ((env (create-test-env))
	   (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
						      (gnc-default-report-currency)))
	   (my-liability-account (env-create-root-account env ACCT-TYPE-LIABILITY
						      (gnc-default-report-currency)))
	   (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
							(gnc-default-report-currency)))
	   (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
						       (gnc-default-report-currency)))
	   (date-0 (gnc:get-start-this-month))
	   (date-1 (gnc:timepair-next-day date-0))
	   (date-2 (gnc:timepair-next-day date-1)))
      (env-create-transaction env date-1 my-income-account my-asset-account (gnc:make-gnc-numeric -1 1))
      (env-create-transaction env date-1 my-expense-account my-liability-account (gnc:make-gnc-numeric -1 1))
      (env-create-transaction env date-2 my-income-account my-asset-account (gnc:make-gnc-numeric -5 1))
      (env-create-transaction env date-2 my-expense-account my-liability-account (gnc:make-gnc-numeric -5 1))
      (begin
	(set-option report gnc:pagename-display "Show table" #t)
	(set-option report gnc:pagename-general "Start Date" (cons 'absolute date-0))
	(set-option report gnc:pagename-general "End Date" (cons 'absolute date-2))
	(set-option report gnc:pagename-general "Step Size" 'DayDelta)
	(set-option report gnc:pagename-general "Price Source" 'pricedb-nearest)
	(set-option report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
	(set-option report gnc:pagename-accounts "Accounts" (list my-income-account my-expense-account))

	(let ((doc (renderer report)))
	  (gnc:html-document-set-style-sheet! doc
					      (gnc:report-stylesheet report))
	  (let* ((result (gnc:html-document-render doc #f))
		 (tbl (stream->list
		       (pattern-streamer "<tr>"
					 (list (list "<string> ([0-9][0-9])/([0-9][0-9])/([0-9][0-9])</td>"
						     1 2 3)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1))
					 result))))
	    (and (every (lambda (row)
				  (and (= (string->number (car (fourth row)))
					  (+ (string->number (car (second row)))
					     (string->number (car (third row)))))
				       ;; txns added in pairs, so assets = liability
				       (equal? (second row) (third row))))
				tbl)
			 (= 0 (tbl-ref->number tbl 0 1))
			 (= 1 (tbl-ref->number tbl 1 1))
			 (= 5 (tbl-ref->number tbl 2 1))
			 (= 3 (tbl-row-count tbl))
			 (= 4 (tbl-column-count tbl)))))))))


(define (closing-test uuid)
  (let* ((template (gnc:find-report-template uuid))
	 (options (gnc:make-report-options uuid))
	 (report (constructor uuid "bar" options
			      #t #t #f #f ""))
	 (renderer (gnc:report-template-renderer template)))
    (let* ((env (create-test-env))
	   (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
						      (gnc-default-report-currency)))
	   (my-liability-account (env-create-root-account env ACCT-TYPE-LIABILITY
						      (gnc-default-report-currency)))
	   (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
							(gnc-default-report-currency)))
	   (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
						       (gnc-default-report-currency)))
	   (my-equity-account (env-create-root-account env ACCT-TYPE-EQUITY
						       (gnc-default-report-currency)))
	   (date-0 (gnc:get-start-this-month))
	   (date-1 (gnc:timepair-next-day date-0))
	   (date-2 (gnc:timepair-next-day date-1))
	   (date-3 (gnc:timepair-next-day date-2)))

      (env-create-transaction env date-1 my-income-account my-asset-account (gnc:make-gnc-numeric -1 1))
      (env-create-transaction env date-2 my-income-account my-asset-account (gnc:make-gnc-numeric -2 1))
      (env-create-transaction env date-3 my-income-account my-asset-account (gnc:make-gnc-numeric -3 1))

      (let ((closing-txn (env-create-transaction env date-2 my-asset-account my-equity-account
						 (gnc:make-gnc-numeric 300 1))))
	(xaccTransSetIsClosingTxn closing-txn #t))

      (begin
	(set-option report gnc:pagename-display "Show table" #t)
	(set-option report gnc:pagename-general "Start Date" (cons 'absolute date-0))
	(set-option report gnc:pagename-general "End Date" (cons 'absolute date-3))
	(set-option report gnc:pagename-general "Step Size" 'DayDelta)
	(set-option report gnc:pagename-general "Price Source" 'pricedb-nearest)
	(set-option report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
	(set-option report gnc:pagename-accounts "Accounts" (list my-income-account my-expense-account))

	(let ((doc (renderer report)))
	  (gnc:html-document-set-style-sheet! doc
					      (gnc:report-stylesheet report))
	  (let* ((result (gnc:html-document-render doc #f))
		 (tbl (stream->list
		       (pattern-streamer "<tr>"
					 (list (list "<string> ([0-9][0-9])/([0-9][0-9])/([0-9][0-9])</td>"
						     1 2 3)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1))
					 result))))
	    (and (every (lambda (row)
				  (and (= (string->number (car (fourth row)))
					  (+ (string->number (car (second row)))
					     (string->number (car (third row)))))))
				tbl)
			 (= 0 (tbl-ref->number tbl 0 1))
			 (= 1 (tbl-ref->number tbl 1 1))
			 (= 2 (tbl-ref->number tbl 2 1))
			 (= 3 (tbl-ref->number tbl 3 1))
			 (= 4 (tbl-row-count tbl))
			 (= 4 (tbl-column-count tbl)))))))))

