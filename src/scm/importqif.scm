;;; $Id$
;;; Import QIF File

(define testing? #f)  ;;; Should we do testing?

(define favorite-currency "USD")  ;;;; This may need to change...

(define (gnc:extensions-qif-import win)
  (let ((account-group (gnc:get-current-group)))
    (if (not account-group)
	(gnc:error-dialog 
	 "No account group available for text export.")
	(begin
	  (display "account-group:") (display account-group) (newline)
	  (gnc:load "testbed.scm")
	  (gnc:load "sstring-qif.scm")
	  (gnc:load "qifutils.scm")
	  (gnc:load "structure.scm")
	  (gnc:load "dates-qif.scm")
	  (gnc:load "split-qif.scm")
	  (gnc:load "qifcats.scm")
	  (gnc:load "parseqif.scm")
	  (gnc:load "qifstate.scm")
	  (gnc:load "qifstat.scm")
	  (gnc:load "qif2gc.scm")
	  (gnc:load "guess-category-qif.scm")
	  (gnc:load "analytical-qifs.scm")
	  (gnc:load "test.scm")
	  (gnc:load "gc-import-qifs.scm")
	  (begin 
	    (get-all-types)
	    (display "Account type list:")
	    (display gnc:account-types)
	    (newline))
	  (gnc:test-load account-group) ;  This tries to create some accounts
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

