;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-merge-groups.scm
;;;  eliminate duplicate xtns in a new (imported) account group 
;;;
;;;  Copyright 2001 Bill Gribble <grib@billgribble.com> 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:group-get-transactions group)
  (let ((query (gnc:malloc-query))
        (xtns #f))

    ;; we want to find all transactions with every split inside the
    ;; account group.
    (gnc:query-add-account-match query
                                 (gnc:group-get-subaccounts group)
                                 'acct-match-any 'query-or)
    (d-gnc:query-set-group query group)
    (set! xtns (gnc:query-get-transactions query 'query-match-all))
    
    ;; lose the query 
    (gnc:free-query query)
    xtns))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:group-find-duplicates 
;;  detect redundant splits/xtns from 'new' and return 
;;  them in a list. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:group-find-duplicates old-group new-group)
  ;; get all the transactions in the new group, then iterate over them
  ;; trying to find matches in the new group.  If there are matches, 
  ;; push the matches onto a list. 
  (let* ((new-xtns (gnc:group-get-transactions new-group))
         (separator (string-ref (gnc:account-separator-char) 0))
         (matches '()))    
    
    ;; for each transaction in the new group, build a query that could
    ;; match possibly similar transactions.
    (for-each
     (lambda (xtn) 
       (let ((query (gnc:malloc-query)))         
         (d-gnc:query-set-group query old-group)
         
         ;; the date should be close to the same.. +/- a week. 
         (let ((date (gnc:transaction-get-date-posted xtn)))               
           (gnc:query-add-date-match-timepair
            query #t (decdate date WeekDelta) #t (incdate date WeekDelta)
            'query-and))
         
         ;; for each split in the transaction, add a term to match the 
         ;; properties of one split 
         (let ((q-splits (gnc:malloc-query)))
           (d-gnc:query-set-group q-splits old-group)
           (for-each 
            (lambda (split)
              (let ((sq (gnc:malloc-query)))
                (d-gnc:query-set-group sq old-group)
                
                ;; we want to match the account in the old group that
                ;; has the same name as an account in the new group.  If
                ;; there's not one (new account), the match will be NULL
                ;; and we know the query won't find anything.  optimize
                ;; this later.
                (gnc:query-add-single-account-match 
                 sq 
                 (gnc:get-account-from-full-name
                  old-group (gnc:account-get-full-name 
                             (gnc:split-get-account split)) separator)
                 'query-and)
                
                ;; we want the value for the split to match the value
                ;; the old-group split.  We should really check for
                ;; fuzziness.
                (d-gnc:query-add-value-match 
                 sq (gnc:numeric-to-double (gnc:split-get-value split))
                 'amt-sgn-match-either 'amt-match-exactly
                 'query-and)
                
                ;; now merge into the split query.  Reminder: q-splits
                ;; is set up to match any split that matches any split
                ;; in the current xtn; every split in an old transaction
                ;; must pass that filter.
                (let ((q-new (gnc:query-merge q-splits sq 'query-or)))
                  (gnc:free-query q-splits)
                  (gnc:free-query sq)
                  (set! q-splits q-new))))
            (gnc:transaction-get-splits xtn))
           
           ;; now q-splits will match any split that is the same as one
           ;; split in the old-group xtn.  Merge it in.
           (let ((q-new (gnc:query-merge query q-splits 'query-and)))
             (gnc:free-query query)
             (gnc:free-query q-splits)
             (set! query q-new)))
         
         ;; now that we have built a query, get transactions in the old
         ;; account group that matches it.
         (let ((old-xtns (gnc:query-get-transactions query 'query-match-all)))
           (set! old-xtns (map 
                           (lambda (elt)
                             (cons elt #f)) old-xtns))
           
           ;; if anything matched the query, push it onto the matches list 
           ;; along with the transaction
           (if (not (null? old-xtns))
               (set! matches (cons (cons xtn old-xtns) matches))))
         (gnc:free-query query)))
     new-xtns)
    
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
             (gnc:transaction-begin-edit new-xtn)
             (gnc:transaction-destroy new-xtn)
             (gnc:transaction-commit-edit new-xtn)))))
   match-list))

(define (gnc:group-catenate-and-merge old-group new-group)
  ;; stuff the new accounts into the old group and merge the accounts
  (gnc:group-concat-group old-group new-group)
  (gnc:account-group-begin-edit new-group)
  (gnc:account-group-destroy new-group)
  (gnc:group-merge-accounts old-group))
