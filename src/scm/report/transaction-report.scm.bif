;; -*-scheme-*-
  

;; something like 
;; for(i = first; i < last; i+= step) { thunk(i);}

(define (gnc:for-loop thunk first last step)
  (cond ((< first last) (thunk first) 
	 (gnc:for-loop thunk (+ first step) last step))
	(else #f)))

;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) (thunk (gnc:account-get-split account x)))
		0 (gnc:account-get-split-count account)  1))


(define (gnc:split-get-corresponding-account-name-and-values split) 
  (list (cons "Not implemented yet." 0)))

(define (gnc:split-get-transaction-date split)
  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))

(define (gnc:split-get-description-from-parent split)
  (gnc:transaction-get-description (gnc:split-get-parent split)))

(define (gnc:make-split-scheme-data split)
  (vector (gnc:split-get-memo split) 
	  (gnc:split-get-action split)
	  (gnc:split-get-description-from-parent split)
	  (gnc:split-get-transaction-date split)
	  (gnc:split-get-reconcile-state split)
	  (gnc:split-get-reconciled-date split)
	  (gnc:split-get-share-amount split)
	  (gnc:split-get-share-amount split)
	  (gnc:split-get-share-price split)
	  (gnc:split-get-value split)
	  (gnc:split-get-docref split)
	  (gnc:split-get-corresponding-account-name-and-values split)))

(define (gnc:split-represent-scheme-data-textually split)
  (call-with-output-string (lambda (x) (write (gnc:make-split-scheme-data split) x))))

(gnc:define-report
;; version
 1
 ;; Name
 "Account Transactions"
 ;; Options
 #f
 ;; renderer
 (lambda (options)
   (let ( (test-account (gnc:group-get-account (gnc:get-current-group) 0)) 
	  (prefix  (list "<HTML>" "<BODY bgcolor=#99ccff>" "<PRE>"))
	  (suffix  (list "</PRE>" "</BODY>" "</HTML>"))
	  (report-lines (list)))
    
     (gnc:for-each-split-in-account 
      test-account 
      (lambda (split)
;	(newline)
;	(write report-lines)
	(set! report-lines (append! report-lines (list (gnc:split-represent-scheme-data-textually split))))))
;     (write prefix)
;     (newline)
;     (write suffix)
;     (newline)
;     (write report-lines)
     (append prefix report-lines suffix))))
