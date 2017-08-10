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

(define-module (gnucash engine test test-extras))

(use-modules (gnucash gnc-module))

(use-modules (gnucash printf))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (srfi srfi-1))
(use-modules (sw_app_utils))
(use-modules (sw_engine))

(export logging-and)
(export test)
(export make-test-sink)
(export env-test-sink)
(export test-sink-report)
(export test-sink-check)

(export delayed-format)
(export delayed-format-render)

(export with-account)
(export with-transaction)

(export create-test-env)
(export env-random-amount)
(export env-random)
(export env-counter-next)
(export env-string)
(export env-select-price-source)
(export env-any-date)
(export env-create-transaction)
(export env-create-account)
(export env-create-root-account)
(export env-create-test-accounts)
(export env-create-daily-transactions)
(export env-create-account-structure)
(export env-create-account-structure-alist)
(export env-expense-account-structure)

;;
;; Random test related syntax and the like
;;

;; logging-and is mostly for debugging tests
(define-macro (logging-and . args)
  (cons 'and (map (lambda (arg)
		    (list 'begin
			  (list 'format #t "Test: ~a\n" (list 'quote arg))
			  arg))
		  args)))

;; ..and 'test' gives nicer output
(define (test the-test)
  (format #t "(Running ~a " the-test)
  (let ((result (the-test)))
    (format #t "~a Completed)\n" result)
    result))

;;
;; Gnucash specifics
;;

;; Really could do with generalising and making into a 'with' macro
(define (with-account account fn)
  (begin (xaccAccountBeginEdit account)
	 (let ((result (fn)))
	   (xaccAccountCommitEdit account)
	   result)))

(define (with-accounts accounts fn)
  (begin (map xaccAccountBeginEdit accounts)
	 (let ((result (fn)))
	   (map xaccAccountCommitEdit accounts)
	   result)))

(define (with-transaction txn fn)
  (begin (xaccTransBeginEdit txn)
	 (let ((result (fn)))
	   (xaccTransCommitEdit txn)
	   result)))

;; Test environments.. an environment is just an alist with some well
;; known names.  The idea is that we can use it to pass around
;; "suitable" sets of values for things

(define (make-counter)
  (let ((x 0))
    (lambda ()
      (begin (set! x (+ x 1))
	     x))))

(define (create-test-env)
  (list (cons 'random (seed->random-state (random 1000)))
	(cons 'counter (make-counter))
	(cons 'sink (make-test-sink))))

(define (env-random-amount env n)
  (gnc:make-gnc-numeric (env-random env n) 1))

(define (env-random env n)
  (random n (assoc-ref env 'random)))

(define (env-counter-next env)
  ((assoc-ref env 'counter)))

(define (env-string env prefix)
  (format #f "~a-~a" prefix (env-counter-next env)))

(define (env-select-price-source env)
  'pricedb-nearest)

(define (env-test-sink env)
  (assoc-ref env 'sink))

(define (env-any-date env) (gnc:get-today))

(define (env-create-transaction env date credit debit aaa)
  (let ((txn (xaccMallocTransaction (gnc-get-current-book)))
	(split-1 (xaccMallocSplit  (gnc-get-current-book)))
	(split-2 (xaccMallocSplit  (gnc-get-current-book)))
	(gnc-localtime (gnc:timepair->date date)))
    (with-transaction txn
		      (lambda ()
			(xaccTransSetDescription txn (env-string env "ponies"))
			(xaccTransSetCurrency txn (gnc-default-report-currency))
			(xaccTransSetDate txn
					  (gnc:date-get-month-day gnc-localtime)
					  (gnc:date-get-month gnc-localtime)
					  (gnc:date-get-year gnc-localtime))
			(xaccSplitSetParent split-1 txn)
			(xaccSplitSetParent split-2 txn)
			(xaccSplitSetAccount split-1 credit)
			(xaccSplitSetAccount split-2 debit)
			(xaccSplitSetAmount split-1 aaa)
			(xaccSplitSetAmount split-2 (gnc-numeric-neg aaa))
			(xaccSplitSetValue split-1 aaa)
			(xaccSplitSetValue split-2 (gnc-numeric-neg aaa))

			))
    ;(format #t "tx ~a\n" (map xaccSplitGetAmount (list split-1 split-2)))
    ;(format #t "tx ~a\n" (map xaccSplitGetValue (list split-1 split-2)))
    txn))


(define (env-create-root-account env type commodity)
  (env-create-account env type commodity (gnc-get-current-root-account)))

(define (env-create-account env type commodity parent-account)
  (let ((new-account (xaccMallocAccount (gnc-get-current-book))))
    (with-accounts (list new-account parent-account)
		  (lambda ()
		    (xaccAccountSetCommodity new-account commodity)
		    (xaccAccountSetName new-account (env-string env "account"))
		    (xaccAccountSetType new-account type)
		    (gnc-account-append-child parent-account new-account)
		    new-account))))

;; Spend '1' on the 1st, '2' on the 2nd, etc.  Makes for pretty graphs
(define (env-create-daily-transactions env start-date end-date to-account from-account)
  (let ((dates-this-month (gnc:make-date-list start-date
					      end-date
					      DayDelta)))
      (for-each (lambda (date)
		  (env-create-transaction env date to-account
					  from-account
					  (gnc:make-gnc-numeric
					   (gnc:date-get-month-day (gnc:timepair->date date))
					   1)))
		(cdr (reverse dates-this-month)))))

(define (env-create-account-structure env account-structure)
  (define (lookup-options list)
    (if (null? list) (cons '() '())
	(if (not (pair? (car list)))
	    (cons '() list)
	    (if (not (pair? (car (car list))))
		(cons '() list)
		list))))

  (define (create-substructure parent options account-structure)
    ;;(format #t "Creating subaccounts for ~a ~a\n"
    ;; (xaccAccountGetName parent) account-structure)
    (let* ((account-name (car account-structure))
	   (options-pair (lookup-options (cdr account-structure)))
	   (options (append (car options-pair) options)))
      ;;(format #t "New Account ~a\n" account-name)
      ;;(format #t "Options ~a\n" (car options-pair))
      ;;(format #t "Child list ~a\n" (cdr options-pair))
      (let ((new-account (env-create-account env (assoc-ref options 'type)
					     (assoc-ref options 'commodity)
					     parent)))
	(with-accounts (list new-account)
		       (lambda ()
			 (xaccAccountSetName new-account account-name)))

	(cons new-account
	      (map (lambda (child)
		     (create-substructure new-account options child))
		   (cdr options-pair))))))
  (let ((options (list (cons 'commodity (gnc-default-report-currency))
		       (cons 'type '()))))
    (create-substructure (gnc-get-current-root-account)
			 options
			 account-structure)))

(define (env-create-account-structure-alist env account-structure)
  (let ((accounts (env-create-account-structure env account-structure)))
    (define (flatten l)
      (if (null? l) '()
	  (if (not (pair? l)) (list l)
	      (append (flatten (car l)) (flatten (cdr l))))))
    (map (lambda (acct) (cons (xaccAccountGetName acct) acct))
	 (flatten accounts))))

(define (env-expense-account-structure env)
  (env-create-account-structure
   env
   (list "Expenses"
	 (list (cons 'type ACCT-TYPE-EXPENSE))
	 (list "Groceries")
	 (list "Rent")
	 (list "Auto"
	       (list "Tax")
	       (list "Parking")
	       (list "Petrol")))))

(define (env-create-test-accounts env)
  (env-create-account-structure-alist env
				      (list "Root"
					    (list (cons 'type ACCT-TYPE-ASSET))
					    (list "Bank")
					    (list "Wallet")
					    (list "Other")
					    (list "Expenses"
						  (list (cons 'type ACCT-TYPE-EXPENSE))))))
;;
;; Test sinks
;;

(define (make-test-sink) (list 'sink 0 '()))

(define (test-sink-count sink)
  (second sink))

(define (test-sink-count! sink value)
  (set-car! (cdr sink) value))

(define (test-sink-messages sink)
  (third sink))

(define (test-sink-messages! sink messages)
  (set-car! (cdr (cdr sink)) messages))

(define (test-sink-check sink message flag)
  (test-sink-count! sink (+ (test-sink-count sink) 1))
  (if flag #t
      (test-sink-messages! sink (cons message (test-sink-messages sink)))))

(define (test-sink-report sink)
  (format #t "Completed ~a tests ~a\n"
	  (test-sink-count sink)
	  (if (null? (test-sink-messages sink)) "PASS" "FAIL"))
  (if (null? (test-sink-messages sink)) #t
      (begin (for-each (lambda (delayed-message)
			 (delayed-format-render #t delayed-message))
		       (test-sink-messages sink))
	     #f)))

(define (delayed-format . x) x)

(define (delayed-format-render stream msg)
  (apply format stream msg))

;;
;; options
;;


(define (create-option-set)
  (make-hash-table) )

(define (option-set-setter option-set)
  (lambda (category name value)
    (hash-set! option-set (list category name) value)))

(define (option-set-getter option-set)
  (lambda (category name)
    (hash-ref option-set (list category name))))

;;
;;
;;

(define (report-show-options stream expense-options)
  (gnc:options-for-each (lambda (option)
			  (format stream "Option: ~a.~a Value ~a\n"
				  (gnc:option-section option)
				  (gnc:option-name option)
				  (gnc:option-value option)))
			expense-options))

