;;; $Id$
;;; Import QIF File
(gnc:support "importqif.scm")
(gnc:depend "qifs/gc-import-qifs.scm")

(define testing? #f)  ;;; Should we do testing?

(define favorite-currency "USD")  ;;;; This may need to change...

(define (gnc:extensions-test-add-accs win)
  (let ((account-group (gnc:get-current-group)))
    (if (not account-group)
	(gnc:error-dialog 
	 "No account group available for account import.")
	(begin
	  (display "account-group:") 
	  (display account-group) (newline)
	  (let ((loadfun (lambda (x) (gnc:load (string-append "qifs/" x))))
		(loadlist '("testbed.scm" "analytical-qifs.scm"
			    "gc-import-qifs.scm"
			    "qifutils.scm" "acc-create.scm"))) 
	    (for-each loadfun loadlist))
	  (begin 
	    (get-all-types)
	    (display "Account type list:")
	    (display gnc:account-types)
	    (newline))
	  (gnc:test-load-accs account-group)))))

(define (gnc:extensions-test-add-txns win)
  (let ((account-group (gnc:get-current-group)))
    (if (not account-group)
	(gnc:error-dialog 
	 "No account group available for transaction import.")
	(begin
	  (display "account-group:") 
	  (display account-group) (newline)
	  (let ((loadfun (lambda (x) (gnc:load (string-append "qifs/" x))))
		(loadlist '("testbed.scm" "analytical-qifs.scm"
			    "gc-import-qifs.scm" "qifutils.scm"
			    "acc-create.scm" "txn-create.scm"))) 
	    (for-each loadfun loadlist))
	  (begin 
	    (get-all-types)
	    (display "Account type list:")
	    (display gnc:account-types)
	    (newline))
	  (gnc:test-load-txns account-group)))))

(define (gnc:extensions-qif-import win)
  (let ((account-group (gnc:get-current-group)))
    (if (not account-group)
	(gnc:error-dialog 
	 "No account group available for QIF import.")
	(begin
	  (display "account-group:") 
	  (display account-group) (newline)
	  (let ((loadfun (lambda (x) (gnc:load (string-append "qifs/" x))))
		(loadlist '("testbed.scm" 
			    "qifutils.scm" "dates-qif.scm"
			    "acc-create.scm"
			    "txn-create.scm"
			    "split-qif.scm" "qifcats.scm"
			    "parseqif.scm" "qifstate.scm"
			    "qifstat.scm" "qif2gc.scm"
			    "guess-category-qif.scm"
			    "analytical-qifs.scm" 
			    "gc-import-qifs.scm")))
	    (for-each loadfun loadlist))
	  (begin 
	    (get-all-types)
	    (display "Account type list:")
	    (display gnc:account-types)
	    (newline))
	  (gnc:import-file-into-account-group account-group)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Now, let's actually execute the code...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(for-each process-possible-qif-file indir)

;;;;; Open Issues:
;;;;;
;;;;; - What account do we load into?
;;;;;   1.  Hopefully this can be determined in an implicit manner...
;;;;;   2.  The alternative is that something interactive must be done for
;;;;;   a group of transactions, querying the user to select the appropriate
;;;;;   account.
;;;;;

