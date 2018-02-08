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
(export category-by-account-report-work)
(export category-by-account-report-do-work)
(export make-gnc-collector-collector)

(export splits-up-to)
(export split->commodity)

(define (split->commodity split)
  (xaccAccountGetCommodity (xaccSplitGetAccount split)))

(define (split->date split)
  (xaccTransGetDate (xaccSplitGetParent split)))

(define (split->account split)
  (xaccSplitGetAccount split))

(define (split-closing? split)
  (xaccTransGetIsClosingTxn (xaccSplitGetParent split)))

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

(define (build-account-collector account-destination-alist
				 per-account-collector)
  (let ((slotset (slotset-map-input split->account
				    (alist->slotset account-destination-alist))))
    (collector-from-slotset slotset per-account-collector)))

(define (build-date-collector dates per-date-collector)
  (let* ((date-vector (list->vector dates))
	 (slotset (make-slotset (lambda (split)
				  (let* ((date (split->date split))
					 (interval-index (binary-search-lt (lambda (pair date)
									     (or (not (car pair))
                                                                                 (gnc:timepair-le (car pair) date)))
									   (cons date 0)
									   date-vector))
					 (interval (vector-ref date-vector interval-index)))
				    interval))
				dates)))
    (collector-from-slotset slotset per-date-collector)))

(define (build-category-by-account-collector account-destination-alist dates cell-accumulator result-collector)
  (build-account-collector account-destination-alist
			   (lambda (account)
			     (collector-reformat (lambda (result)
						   (list account (result-collector account result)))
						 (build-date-collector dates
								       (lambda (date)
									 (cell-accumulator account date)))))))

(define (category-by-account-report do-intervals? datepairs account-alist
				    split-collector result-collector progress-range)
  (let* ((work (category-by-account-report-work do-intervals? datepairs
					       account-alist split-collector result-collector))
	 (splits-fn (car work))
	 (collector (cdr work))
	 (splits (splits-fn)))
    (collector-add-all (collector-do collector
				     (progress-collector (length splits) progress-range))
		       splits)))

(define (category-by-account-report-do-work work progress-range)
  (let* ((splits-fn (car work))
	 (collector (cdr work))
	 (splits (splits-fn)))
    (collector-add-all (collector-do collector
				     (progress-collector (length splits) progress-range))
		       splits)))

;; Decide how to run the given report (but don't actually do any work)

(define (category-by-account-report-work do-intervals? datepairs account-alist
				    split-collector result-collector)
  (let* ((dateinfo (if do-intervals? (category-report-dates-intervals datepairs)
		       (category-report-dates-accumulate datepairs)))
	 (processed-datepairs (third dateinfo))
	 (splits-fn (lambda () (category-report-splits dateinfo account-alist)))
	 (collector (collector-where (predicate-not split-closing?)
				     (build-category-by-account-collector account-alist
									  processed-datepairs split-collector
									  result-collector))))
    (cons splits-fn collector)))

(define (category-report-splits dateinfo account-alist)
  (let ((min-date (first dateinfo))
	(max-date (second dateinfo)))
    (splits-up-to (map car account-alist) min-date max-date)))

(define (category-report-dates-intervals datepairs)
  (let* ((min-date (car (list-min-max (map first datepairs) gnc:timepair-lt)))
	 (max-date (cdr (list-min-max (map second datepairs) gnc:timepair-lt))))
    (list min-date max-date datepairs)))

(define (category-report-dates-accumulate dates)
  (let* ((min-date #f)
	 (max-date (cdr (list-min-max dates gnc:timepair-lt)))
	 (datepairs (reverse! (cdr (fold (lambda (next acc)
					   (let ((prev (car acc))
						 (pairs-so-far (cdr acc)))
					     (cons next (cons (list prev next) pairs-so-far))))
					 (cons min-date '()) dates)))))
    (list min-date max-date datepairs)))



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
