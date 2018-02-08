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

(define-module (gnucash report standard-reports test test-generic-category-report))

(use-modules (ice-9 format))
(use-modules (ice-9 streams))
(use-modules (srfi srfi-1))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash printf))
(use-modules (gnucash report report-system))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (sw_engine))

(use-modules (gnucash report report-system collectors))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system test test-extras))

(export run-category-income-expense-test)
(export run-category-asset-liability-test)

(define (set-option report page tag value)
  ((gnc:option-setter (gnc:lookup-option (gnc:report-options report)
					 page tag)) value))


(define constructor (record-constructor <report>))

;(set-option income-report gnc:pagename-general "Start Date" (cons 'relative 'start-prev-year))
;(set-option income-report gnc:pagename-general "End Date" (cons 'relative 'end-this-month))
;(set-option income-report gnc:pagename-general "Show table" #t)
;(set-option income-report gnc:pagename-general "Price Source" 'pricedb-nearest)
;(set-option income-report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))

(define (run-category-income-expense-test income-report-uuid expense-report-uuid)
  (and  (null-test income-report-uuid)
	(null-test expense-report-uuid)
	(single-txn-test income-report-uuid)
	(multi-acct-test expense-report-uuid)
	#t))

(define (run-category-asset-liability-test asset-report-uuid liability-report-uuid)
  (and (null-test asset-report-uuid)
       (null-test liability-report-uuid)
       (asset-test asset-report-uuid)
       (liability-test liability-report-uuid)
       #t))

;; No real test here, just confirm that no exceptions are thrown
(define (null-test uuid)
  (let* ((template (gnc:find-report-template uuid))
	 (options (gnc:make-report-options uuid))
	 (report (constructor uuid "bar" options
				     #t #t #f #f ""))
	 (renderer (gnc:report-template-renderer template)))

    (let ((doc (renderer report)))
      (gnc:html-document-set-style-sheet! doc
					  (gnc:report-stylesheet report))
      #t
      )))


(define (single-txn-test uuid)
  (let* ((income-template (gnc:find-report-template uuid))
	 (income-options (gnc:make-report-options uuid))
	 (income-report (constructor uuid "bar" income-options
				     #t #t #f #f ""))
	 (income-renderer (gnc:report-template-renderer income-template)))
    (let* ((env (create-test-env))
	   (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
						      (gnc-default-report-currency)))
	   (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
							(gnc-default-report-currency)))
	   (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
						       (gnc-default-report-currency))))
      (env-create-daily-transactions env
				     (gnc:get-start-this-month)
				     (gnc:get-end-this-month)
				     my-asset-account my-income-account)
      (begin
	(set-option income-report gnc:pagename-display "Show table" #t)
	(set-option income-report gnc:pagename-general "Start Date" (cons 'relative 'start-this-month))
	(set-option income-report gnc:pagename-general "End Date" (cons 'relative 'end-this-month))
	(set-option income-report gnc:pagename-general "Step Size" 'DayDelta)
	(set-option income-report gnc:pagename-general "Price Source" 'pricedb-nearest)
	(set-option income-report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
	(set-option income-report gnc:pagename-accounts "Accounts" (list my-income-account))
	(set-option income-report gnc:pagename-accounts "Show Accounts until level"  'all)

	(let ((doc (income-renderer income-report)))
	  (gnc:html-document-set-style-sheet! doc
					      (gnc:report-stylesheet income-report))
	  (let* ((result (gnc:html-document-render doc #f))
		 (tbl (stream->list
		       (pattern-streamer "<tr>"
					 (list (list "<string> ([0-9][0-9])/([0-9][0-9])/([0-9][0-9])</td>" 1 2 3)
					       (list "<number> ([^<]*)</td>" 1))
					 result))))
	    (every (lambda (date value-list)
		     (let ((day (second date))
			   (value (first value-list)))
		       (= (string->number day) (string->number value))))
		   (map first tbl)
		   (map second tbl))))))))

(define (list-leaves list)
  (if (not (pair? list))
      (cons list '())
      (fold (lambda (next acc)
	      (append (list-leaves next)
		      acc))
	    '()
	    list)))

(define (multi-acct-test expense-report-uuid)
  (let* ((expense-template (gnc:find-report-template expense-report-uuid))
	 (expense-options (gnc:make-report-options expense-report-uuid))
	 (expense-report (constructor expense-report-uuid "bar" expense-options
				     #t #t #f #f ""))
	 (expense-renderer (gnc:report-template-renderer expense-template)))
    (let* ((env (create-test-env))
	   (expense-accounts (env-expense-account-structure env))
	   (asset-accounts (env-create-account-structure
			    env
			    (list "Assets"
				  (list (cons 'type ACCT-TYPE-ASSET))
				  (list "Bank"))))
	   (leaf-expense-accounts (list-leaves expense-accounts))
	   (bank-account (car (car (cdr asset-accounts)))))
      (for-each (lambda (expense-account)
		  (env-create-daily-transactions env
						 (gnc:get-start-this-month)
						 (gnc:get-end-this-month)
						 expense-account
						 bank-account))
		leaf-expense-accounts)
      (begin
	(set-option expense-report gnc:pagename-display "Show table" #t)
	(set-option expense-report gnc:pagename-general "Start Date" (cons 'relative 'start-this-month))
	(set-option expense-report gnc:pagename-general "End Date" (cons 'relative 'end-this-month))
	(set-option expense-report gnc:pagename-general "Step Size" 'DayDelta)
	(set-option expense-report gnc:pagename-general "Price Source" 'pricedb-nearest)
	(set-option expense-report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
	(set-option expense-report gnc:pagename-accounts "Accounts" leaf-expense-accounts)
	(set-option expense-report gnc:pagename-accounts "Show Accounts until level" 2)

	(let ((doc (expense-renderer expense-report)))
	  (gnc:html-document-set-style-sheet! doc
					      (gnc:report-stylesheet expense-report))
	  (let* ((html-document (gnc:html-document-render doc #f))
		 (columns (columns-from-report-document html-document))
		 (tbl (stream->list
		       (pattern-streamer "<tr>"
					 (list (list "<string> ([0-9][0-9])/([0-9][0-9])/([0-9][0-9])</td>" 1 2 3)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1)
					       (list "<number> ([^<]*)</td>" 1))
					 html-document))))
	    ;(format #t "~a" html-document)
	    (and (= 6 (length columns))
		 (equal? "Date" (first columns))
		 (equal? "Auto" (second columns))
		 ;; maybe should try to check actual values
		 )))))))

(define (columns-from-report-document doc)
  (let ((columns (stream->list (pattern-streamer "<th>"
						 (list (list "<string> ([^<]*)</" 1))
						 doc))))
    (map caar columns)))

;;
;;
;;

(define (asset-test uuid)
    (let* ((asset-template (gnc:find-report-template uuid))
	   (asset-options (gnc:make-report-options uuid))
	   (asset-report (constructor uuid "bar" asset-options
				      #t #t #f #f ""))
	   (asset-renderer (gnc:report-template-renderer asset-template)))
      (let* ((env (create-test-env))
	     (my-asset-account (env-create-root-account env ACCT-TYPE-ASSET
							(gnc-default-report-currency)))
	     (my-expense-account (env-create-root-account env ACCT-TYPE-EXPENSE
							  (gnc-default-report-currency)))
	     (my-income-account (env-create-root-account env ACCT-TYPE-INCOME
							 (gnc-default-report-currency))))
      (env-create-daily-transactions env
				     (gnc:get-start-this-month)
				     (gnc:get-end-this-month)
				     my-asset-account my-income-account)
      (begin
	(set-option asset-report gnc:pagename-display "Show table" #t)
	(set-option asset-report gnc:pagename-general "Start Date" (cons 'relative 'start-this-month))
	(set-option asset-report gnc:pagename-general "End Date" (cons 'relative 'end-this-month))
	(set-option asset-report gnc:pagename-general "Step Size" 'DayDelta)
	(set-option asset-report gnc:pagename-general "Price Source" 'pricedb-nearest)
	(set-option asset-report gnc:pagename-general "Price Source" 'pricedb-nearest)
	(set-option asset-report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
	(set-option asset-report gnc:pagename-accounts "Accounts" (list my-asset-account))
	(set-option asset-report gnc:pagename-accounts "Show Accounts until level"  'all)

	(let ((doc (asset-renderer asset-report)))
	  (gnc:html-document-set-style-sheet! doc
					      (gnc:report-stylesheet asset-report))
	  (let* ((html-document (gnc:html-document-render doc #f))
		 (columns (columns-from-report-document html-document))
		 (tbl (stream->list
		       (pattern-streamer "<tr>"
					 (list (list "<string> ([0-9][0-9])/([0-9][0-9])/([0-9][0-9])</td>" 1 2 3)
					       (list "<number> ([^<]*)</td>" 1))
					 html-document)))
		 (row-count (tbl-row-count tbl)))
	    (and (member "account-1" columns)
			 (= 2 (length columns))
			 (= 1 (string->number (car (tbl-ref tbl 0 1))))
			 (= (/ (* row-count (+ row-count 1)) 2)
			    (string->number (car (tbl-ref tbl (- row-count 1) 1))))
			 #t)))))))

(define (liability-test uuid)
  ;; this test is tailored for bug 793278
  ;; except we can't use $10,000 because the string->number
  ;; function cannot handle thousand separators. Use $100.
  (let* ((liability-template (gnc:find-report-template uuid))
         (liability-options (gnc:make-report-options uuid))
         (liability-report (constructor uuid "bar" liability-options
                                        #t #t #f #f ""))
         (liability-renderer (gnc:report-template-renderer liability-template)))
    (let* ((env (create-test-env))
           (asset--acc (env-create-root-account env ACCT-TYPE-ASSET (gnc-default-report-currency)))
           (liabil-acc (env-create-root-account env ACCT-TYPE-CREDIT (gnc-default-report-currency)))
           (income-acc (env-create-root-account env ACCT-TYPE-INCOME (gnc-default-report-currency))))
      (env-create-transaction env (gnc-dmy2timespec 01 10 2016) asset--acc liabil-acc (gnc:make-gnc-numeric 100 1)) ;loan
      (env-create-transaction env (gnc-dmy2timespec 01 01 2017) asset--acc income-acc (gnc:make-gnc-numeric 10 1))  ;salary#1
      (env-create-transaction env (gnc-dmy2timespec 02 01 2017) liabil-acc asset--acc (gnc:make-gnc-numeric 9 1))   ;repay#1
      (env-create-transaction env (gnc-dmy2timespec 01 02 2017) asset--acc income-acc (gnc:make-gnc-numeric 10 1))  ;salary#2
      (env-create-transaction env (gnc-dmy2timespec 02 02 2017) liabil-acc asset--acc (gnc:make-gnc-numeric 9 1))   ;repay#2
      (env-create-transaction env (gnc-dmy2timespec 01 03 2017) asset--acc income-acc (gnc:make-gnc-numeric 10 1))  ;salary#3
      (env-create-transaction env (gnc-dmy2timespec 02 03 2017) liabil-acc asset--acc (gnc:make-gnc-numeric 9 1))   ;repay#3
      (env-create-transaction env (gnc-dmy2timespec 01 04 2017) asset--acc income-acc (gnc:make-gnc-numeric 10 1))  ;salary#4
      (env-create-transaction env (gnc-dmy2timespec 02 04 2017) liabil-acc asset--acc (gnc:make-gnc-numeric 9 1))   ;repay#4
      (env-create-transaction env (gnc-dmy2timespec 01 05 2017) asset--acc income-acc (gnc:make-gnc-numeric 10 1))  ;salary#5
      (env-create-transaction env (gnc-dmy2timespec 02 05 2017) liabil-acc asset--acc (gnc:make-gnc-numeric 9 1))   ;repay#5
      (begin
        (set-option liability-report gnc:pagename-display "Show table" #t)
        (set-option liability-report gnc:pagename-general "Start Date" (cons 'absolute (gnc-dmy2timespec 01 01 2017)))
        (set-option liability-report gnc:pagename-general "End Date" (cons 'absolute (gnc-dmy2timespec 31 12 2018)))
        (set-option liability-report gnc:pagename-general "Step Size" 'MonthDelta)
        (set-option liability-report gnc:pagename-general "Price Source" 'pricedb-nearest)
        (set-option liability-report gnc:pagename-general "Report's currency"  (gnc-default-report-currency))
        (set-option liability-report gnc:pagename-accounts "Accounts" (list liabil-acc))
        (set-option liability-report gnc:pagename-accounts "Show Accounts until level"  'all)
        (let ((doc (liability-renderer liability-report)))
          (gnc:html-document-set-style-sheet! doc (gnc:report-stylesheet liability-report))
          (let* ((html-document (gnc:html-document-render doc #f))
                 (columns (columns-from-report-document html-document))
                 (tbl (stream->list
                       (pattern-streamer "<tr>"
                                         (list (list "<td><string> ([0-9][0-9])/([0-9][0-9])/([0-9][0-9])</td>" 1 2 3)
                                               (list "<td class=\"number-cell\"><number> [^0-9]*([^<]*)</td>" 1))
                                         html-document)))
                 (row-count (tbl-row-count tbl)))
            (format #t "\nrender:\n~a\n" html-document)
            (and (= 2 (length columns))
                 (= 100 (string->number (car (tbl-ref tbl 0 1))))
                 (= 55 (string->number (car (tbl-ref tbl (- row-count 1) 1))))
                 #t)))))))
