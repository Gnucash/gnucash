;;; $Id$
(display "Started gc-impor.scm")
(newline)
(define (gnc:get-account-list account-group)
  (if testing?
      gc-accts
      (let ((fullacclist 
	     (flatten 
	      (gnc:group-map-accounts get-names-of-accounts
				      account-group))))
	(display "acclist:")
	(display fullacclist)
	(newline)
	(filteroutnulls fullacclist))))


(define (gnc:import-file-into-account-group account-group)
  ;(sample-dialog)
  (let ((file-name 
	 (gnc:file-selection-dialog "Select file for QIF import" "*.qif")))
    (if file-name
	(begin
	  (gnc:debug "Loading data from file " file-name)
	  (let* ((txn-list (read-qif-file file-name account-group))
		 (category-analysis (analyze-qif-transaction-categories txn-list)))
	    ;;; Now, take steps:
	    (qif-to-gnucash txn-list file-name)
	    (list txn-list category-analysis))))))

;;; Set up QIF Category


