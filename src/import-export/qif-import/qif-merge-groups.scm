;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-merge-groups.scm
;;;  eliminate duplicate xtns in a new (imported) account group 
;;;
;;;  Copyright 2001 Bill Gribble <grib@billgribble.com> 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:group-get-transactions group)
  (let ((query (qof-query-create-for-splits))
        (xtns #f))

    (qof-query-set-book query (xaccGroupGetBook group))

    ;; we want to find all transactions with every split inside the
    ;; account group.
    (xaccQueryAddAccountMatch query
                                 (xaccGroupGetSubAccountsSorted group)
                                 QOF-GUID-MATCH-ANY QOF-QUERY-AND)

    (set! xtns (xaccQueryGetTransactions query QUERY-TXN-MATCH-ALL))
    
    ;; lose the query 
    (qof-query-destroy query)
    xtns))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:group-find-duplicates 
;;  detect redundant splits/xtns from 'new' and return 
;;  them in a list. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:group-find-duplicates old-group new-group window)
  ;; get all the transactions in the new group, then iterate over them
  ;; trying to find matches in the new group.  If there are matches, 
  ;; push the matches onto a list. 
  (let* ((new-xtns (gnc:group-get-transactions new-group))
	 (progress-dialog #f)
	 (work-to-do (length new-xtns))
	 (work-done 0)
         (matches '()))    
    
    (if (> work-to-do 100)
	(begin 
	  (set! progress-dialog (gnc-progress-dialog-new window #f))
	  (gnc-progress-dialog-set-title progress-dialog (_ "Progress"))
	  (gnc-progress-dialog-set-heading progress-dialog
					   (_ "Finding duplicate transactions..."))))

    ;; for each transaction in the new group, build a query that could
    ;; match possibly similar transactions.
    (for-each
     (lambda (xtn) 
       (let ((query (qof-query-create-for-splits)))
	 (set! work-done (+ 1 work-done))
	 (if progress-dialog 
	     (begin 
	       (gnc-progress-dialog-set-value
		progress-dialog (/ work-done work-to-do))
	       (gnc-progress-dialog-update progress-dialog)))

	 (qof-query-set-book query (xaccGroupGetBook old-group))

	 ;; first, we want to find only transactions from the old group.
	 (xaccQueryAddAccountMatch query
				      (xaccGroupGetSubAccountsSorted old-group)
				      QOF-GUID-MATCH-ANY QOF-QUERY-AND)
         
         ;; the date should be close to the same.. +/- a week. 
         (let ((date (gnc-transaction-get-date-posted xtn)))
           (xaccQueryAddDateMatchTS
            query #t (decdate date WeekDelta) #t (incdate date WeekDelta)
            QOF-QUERY-AND))
         
         ;; for each split in the transaction, add a term to match the 
         ;; properties of one split 
         (let ((q-splits (qof-query-create-for-splits)))
           (for-each 
            (lambda (split)
              (let ((sq (qof-query-create-for-splits)))
		(qof-query-set-book sq (xaccGroupGetBook old-group))
                
                ;; we want to match the account in the old group that
                ;; has the same name as an account in the new group.  If
                ;; there's not one (new account), the match will be NULL
                ;; and we know the query won't find anything.  optimize
                ;; this later.
                (xaccQueryAddSingleAccountMatch
                 sq 
                 (xaccGetAccountFromFullName
                  old-group (gnc-account-get-full-name
                             (xaccSplitGetAccount split)))
                 QOF-QUERY-AND)
                
                ;; we want the value for the split to match the value
                ;; the old-group split.  We should really check for
                ;; fuzziness.
                (xaccQueryAddValueMatch
                 sq (xaccSplitGetValue split)
                 QOF-NUMERIC-MATCH-ANY QOF-COMPARE-EQUAL
                 QOF-QUERY-AND)
                
                ;; now merge into the split query.  Reminder: q-splits
                ;; is set up to match any split that matches any split
                ;; in the current xtn; every split in an old transaction
                ;; must pass that filter.
                (let ((q-new (qof-query-merge q-splits sq QOF-QUERY-OR)))
                  (qof-query-destroy q-splits)
                  (qof-query-destroy sq)
                  (set! q-splits q-new))))
            (xaccTransGetSplits xtn))
           
           ;; now q-splits will match any split that is the same as one
           ;; split in the old-group xtn.  Merge it in.
           (let ((q-new (qof-query-merge query q-splits QOF-QUERY-AND)))
             (qof-query-destroy query)
             (qof-query-destroy q-splits)
             (set! query q-new)))
         
         ;; now that we have built a query, get transactions in the old
         ;; account group that matches it.
         (let ((old-xtns (xaccQueryGetTransactions query QUERY-TXN-MATCH-ALL)))
           (set! old-xtns (map 
                           (lambda (elt)
                             (cons elt #f)) old-xtns))
           
           ;; if anything matched the query, push it onto the matches list 
           ;; along with the transaction
           (if (not (null? old-xtns))
               (set! matches (cons (cons xtn old-xtns) matches))))
         (qof-query-destroy query)))
     new-xtns)
    
    ;; get rid of the progress dialog 
    (if progress-dialog
	(gnc-progress-dialog-destroy progress-dialog))

    ;; return the matches 
    matches))
  
(define (gnc:prune-matching-transactions match-list)
  (for-each 
   (lambda (match)
     (let ((new-xtn (car match))
           (matches (cdr match))
           (do-delete #f))
       (for-each 
        (lambda (old)
          (if (cdr old)
              (set! do-delete #t)))
        matches)
       (if do-delete 
           (begin 
             (xaccTransBeginEdit new-xtn)
             (xaccTransDestroy new-xtn)
             (xaccTransCommitEdit new-xtn)))))
   match-list))

(define (gnc:group-catenate-and-merge old-group new-group)
  ;; stuff the new accounts into the old group and merge the accounts
  (xaccGroupConcatGroup old-group new-group)
  (xaccAccountGroupBeginEdit new-group)
  (xaccAccountGroupDestroy new-group)
  (xaccGroupMergeAccounts old-group))
