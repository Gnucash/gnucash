(define-module (gnucash report report-system report-collectors))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (ice-9 format))
(use-modules (srfi srfi-1))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash printf))
(use-modules (gnucash report report-system))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (sw_engine))
(use-modules (gnucash report report-system collectors))
(use-modules (gnucash report report-system list-extras))

(export account-destination-alist)
(export category-by-account-report)
(export make-gnc-collector-collector)

(export splits-up-to)
(export split->commodity)

(define (split->commodity split)
  (xaccAccountGetCommodity (xaccSplitGetAccount split)))

(define (split->date split)
  (xaccTransGetDate (xaccSplitGetParent split)))

(define (splits-up-to accounts startdate enddate)
  (gnc:account-get-trans-type-splits-interval accounts #f
					      startdate
					      enddate))

(define (make-gnc-collector-collector)
  (let ((gnc-collector (gnc:make-commodity-collector)))
    (define collector
      (make-collector (lambda (split)
			(let* ((shares (xaccSplitGetAmount split))
			       (acct-comm (split->commodity split)))
			  (gnc-collector 'add acct-comm shares)
			  collector))
		      (lambda () gnc-collector)))
    collector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Plan:
;; We create reports via collectors - effectively per account, per date stores of values.
;; Values are held as report-system/collector objects (sorry about the name reuse..),
;; which can then be evaluated by a collector-reformat step.
;;
;; For a given report, we want to retrieve relevant transactions once
;; (this is the splits-up-to function), and then push the transactions
;; into a collector structure.  This way there's no O(n^2) or worse
;; complexity.

(define (build-account-collector accounts account-destination-alist
				 split->account
				 per-account-collector)
  (let ((slotset (slotset-map-input split->account
				    (alist->slotset account-destination-alist))))
    (collector-from-slotset slotset per-account-collector)))

(define (filter-for-account the-account destination-alist split->account)
  (let ((wanted-accounts (fold (lambda (pair acc)
				 (if (equal? (cdr pair) the-account)
				     (cons (car pair) acc)
				     acc))
			       '()
			       destination-alist)))
    (make-filter the-account
		 (lambda (split)
		   (member (split->account split) wanted-accounts)))))

(define (build-date-collector split->date dates per-date-collector)
  (let* ((date-vector (list->vector dates))
	 (slotset (make-slotset (lambda (split)
				  (let* ((date (split->date split))
					 (interval-index (binary-search-lt (lambda (pair date)
									     (gnc:timepair-le (car pair) date))
									   (cons date 0)
									   date-vector))
					 (interval (vector-ref date-vector interval-index)))
				    interval))
				dates)))
    (collector-from-slotset slotset per-date-collector)))

(define (build-category-by-account-collector accounts account-destination-alist dates cell-accumulator result-collector)
  (build-account-collector accounts account-destination-alist
			   xaccSplitGetAccount
			   (lambda (account)
			     (collector-reformat (lambda (result)
						   (list account (result-collector account result)))
						 (build-date-collector split->date dates
								       (lambda (date)
									 (cell-accumulator account date)))))))

(define (category-by-account-report do-intervals? datepairs account-alist split-collector result-collector progress-range)
  (if do-intervals?
      (category-by-account-report-intervals datepairs account-alist split-collector result-collector progress-range)
      (category-by-account-report-accumulate datepairs account-alist split-collector result-collector progress-range)))

(define (category-by-account-report-intervals datepairs account-alist split-collector result-collector progress-range)
  (let* ((min-date (car (list-min-max (map first datepairs) gnc:timepair-lt)))
	  (max-date (cdr (list-min-max (map second datepairs) gnc:timepair-lt)))
	  (dest-accounts (collector-add-all (make-eq-set-collector '())
					    (map cdr account-alist)))
	  (splits (splits-up-to (map car account-alist)
			      min-date max-date))
	  (collector (build-category-by-account-collector dest-accounts
							 account-alist datepairs
							 split-collector
							 result-collector)))
     (collector-add-all (collector-do collector
				      (progress-collector (length splits) progress-range))
			splits)))

(define (category-by-account-report-accumulate dates account-alist split-collector result-collector progress-range)
  (let* ((min-date (gnc:secs->timepair 0))
	 (max-date (cdr (list-min-max dates gnc:timepair-lt)))
	 (datepairs (reverse! (cdr (fold (lambda (next acc)
					   (let ((prev (car acc))
						 (pairs-so-far (cdr acc)))
					     (cons next (cons (list prev next) pairs-so-far))))
					 (cons min-date '()) dates))))
	 (dest-accounts (collector-add-all (make-eq-set-collector '())
					   (map cdr account-alist)))
	 (splits (splits-up-to (map car account-alist)
			       min-date max-date))
	 (collector (build-category-by-account-collector dest-accounts account-alist datepairs split-collector
							 result-collector)))
    (collector-add-all (collector-do collector
				   (progress-collector (length splits) progress-range))
		       splits)))

(define (progress-collector size range)
  (let* ((from (car range))
	 (to (cdr range))
	 (width (- to from)))
    (define (count->percentage count)
      (+ (* width (/ count size)) from))
    (function-state->collector (lambda (value state)
				 (let ((last (floor (count->percentage (- state 1))))
				       (next (floor (count->percentage state))))
				   (if (not (= last next))
				       (gnc:report-percent-done (+ (* width (/ state size)) from)))
				   (+ state 1)))
			       0)))

(define (gnc-account-child-accounts-recursive account)
  (define (helper account initial)
    (fold (lambda (child-account accumulator)
	    (append (helper child-account (list child-account))
		    accumulator))
	  initial
	  (gnc-account-get-children account)))
  (helper account '()))

(define (traverse-accounts tree-depth show-acct? account-types)
  (define (inner-traverse-accounts current-depth accounts)
    (if (< current-depth tree-depth)
	  (let ((res '()))
	    (for-each
	     (lambda (a)
	       (begin
		 (if (show-acct? a)
		     (set! res
			   (cons (cons a a) res)))
		 (set! res (append
			    (inner-traverse-accounts
			     (+ 1 current-depth)
			     (gnc-account-get-children a))
			    res))))
	     accounts)
	    res)
	  ;; else (i.e. current-depth == tree-depth)
	  (fold (lambda (account acc)
		 (let ((child-accounts (gnc-account-child-accounts-recursive account)))
		   (append (map (lambda (child-account)
				  (cons child-account account))
				child-accounts)
			   (list (cons account account))
			   acc)))
		'()
		(filter show-acct? accounts))))
  (let* ((topl-accounts (gnc:filter-accountlist-type
			account-types
			(gnc-account-get-children-sorted
			 (gnc-get-current-root-account))))
	 (account-head-list (inner-traverse-accounts 1 topl-accounts)))
    account-head-list))

(define (account-destination-alist accounts account-types tree-depth)
  (define (show-acct? a)
    (member a accounts))
  (traverse-accounts tree-depth show-acct? account-types))
