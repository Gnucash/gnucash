;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-merge-groups.scm
;;;  eliminate duplicate xtns in a new (imported) account group
;;;
;;;  Copyright 2001 Bill Gribble <grib@billgribble.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:account-tree-get-transactions root)
  (let ((accounts (gnc-account-get-descendants-sorted root)))
    (if (null? accounts)
        '()
        (let ((query (qof-query-create-for-splits))
              (xtns #f))

          (qof-query-set-book query (gnc-account-get-book root))

          ;; we want to find all transactions with every split inside the
          ;; account group.
          (xaccQueryAddAccountMatch query accounts
                                    QOF-GUID-MATCH-ANY QOF-QUERY-AND)

          (set! xtns (xaccQueryGetTransactions query QUERY-TXN-MATCH-ALL))

          ;; lose the query
          (qof-query-destroy query)
          xtns))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:account-tree-find-duplicates
;;
;;  This procedure compares two account trees, given by old-root
;;  and new-root, and returns a list of splits/transactions in
;;  old-root that may be duplicates.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:account-tree-find-duplicates old-root new-root window)

  ;; Given a list of accounts, this predicate returns true if any
  ;; of those accounts are involved in a transaction.
  (define (has-any-xtns? acctlist)
    (if (null? acctlist)
        #f
        (let ((splits (xaccAccountGetSplitList (car acctlist))))
          (if (null? splits)
              (has-any-xtns? (cdr acctlist))
              #t))))

  (let ((old-accounts (gnc-account-get-descendants-sorted old-root)))
    (if (has-any-xtns? old-accounts)
        ;; Get all the transactions in the new tree, then iterate over them
        ;; trying to find matches in the old tree.  If there are matches,
        ;; push the matches onto a list.
        (let* ((new-xtns (gnc:account-tree-get-transactions new-root))
               (progress-dialog '())
               (work-to-do (length new-xtns))
               (work-done 0)
               (matches '()))

          ;; Use a progress dialog if this might take a while.
          (if (> work-to-do 100)
            (begin
              (set! progress-dialog (gnc-progress-dialog-new window #f))
              (gnc-progress-dialog-set-title progress-dialog (_ "Progress"))
              (gnc-progress-dialog-set-heading progress-dialog
                (_ "Finding duplicate transactions..."))))

          ;; For each transaction in the new account tree, build a query
          ;; that matches possibly duplicate transactions in the old tree.
          (for-each
            (lambda (xtn)
              (let ((query (qof-query-create-for-splits)))
                (set! work-done (+ 1 work-done))
                (if (not (null? progress-dialog))
                  (begin
                    (gnc-progress-dialog-set-value progress-dialog
                                                   (/ work-done work-to-do))
                    (gnc-progress-dialog-update progress-dialog)))

                (qof-query-set-book query (gnc-account-get-book old-root))

                ;; First, we only want to find only transactions
                ;; from accounts in the old tree.
                (xaccQueryAddAccountMatch query
                                          old-accounts
                                          QOF-GUID-MATCH-ANY QOF-QUERY-AND)

                ;; The date should be close to the same.. +/- a week.
                (let ((date (gnc-transaction-get-date-posted xtn)))
                  (xaccQueryAddDateMatchTS query #t
                                           (decdate date WeekDelta) #t
                                           (incdate date WeekDelta)
                                           QOF-QUERY-AND))

                ;; For each split in the transaction, add a term
                ;; to match the properties of one split.
                (let ((q-splits (qof-query-create-for-splits)))
                  (for-each
                    (lambda (split)
                      (let ((sq (qof-query-create-for-splits)))
                        (qof-query-set-book sq (gnc-account-get-book old-root))

                        ;; We want to match the account in the old tree that
                        ;; has the same name as an account in the new tree.
                        ;; If there's not one (indicating a new account),
                        ;; the match will be NULL and the query won't find
                        ;; anything.  Optimize this later.
                        (xaccQueryAddSingleAccountMatch
                          sq
                          (gnc-account-lookup-by-full-name old-root
                            (gnc-account-get-full-name
                              (xaccSplitGetAccount split)))
                          QOF-QUERY-AND)

                        ;; We want the value of the split in the new tree
                        ;; to match the the value of the split in the old
                        ;; tree.  We should really check for fuzziness.
                        (xaccQueryAddValueMatch sq
                                                (xaccSplitGetValue split)
                                                QOF-NUMERIC-MATCH-ANY
                                                QOF-COMPARE-EQUAL
                                                QOF-QUERY-AND)

                        ;; Now merge into the split query.  Reminder: q-splits
                        ;; is set up to match any split that matches any split
                        ;; in the new transaction; every split in an old
                        ;; transaction must pass that filter.
                        (let ((q-new (qof-query-merge q-splits
                                                      sq
                                                      QOF-QUERY-OR)))
                          (qof-query-destroy q-splits)
                          (qof-query-destroy sq)
                          (set! q-splits q-new))))
                    (xaccTransGetSplitList xtn))

                  ;; Now q-splits will match any split that is the same as one
                  ;; split in the old-root transaction.  Merge it in.
                  (let ((q-new (qof-query-merge query
                                                q-splits
                                                QOF-QUERY-AND)))
                    (qof-query-destroy query)
                    (qof-query-destroy q-splits)
                    (set! query q-new)))

                ;; Now that we have built a query, get transactions in the old
                ;; account tree that match it.
                (let ((old-xtns (xaccQueryGetTransactions query
                                                          QUERY-TXN-MATCH-ALL)))
                  (set! old-xtns (map
                                   (lambda (elt)
                                     (cons elt #f)) old-xtns))

                  ;; If anything matched the query, push it onto the matches
                  ;; list along with the transaction.
                  (if (not (null? old-xtns))
                      (set! matches (cons (cons xtn old-xtns) matches))))

                (qof-query-destroy query)))
            new-xtns)

          ;; Get rid of the progress dialog.
          (if (not (null? progress-dialog))
              (gnc-progress-dialog-destroy progress-dialog))

          ;; Return the matches.
          matches)

        ;; Since there are either no accounts or no transactions in the old
        ;; tree, duplicate checking is unnecessary. Return an empty list.
        '())))

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

(define (gnc:account-tree-catenate-and-merge old-root new-root)
  ;; stuff the new accounts into the old account tree and merge the accounts
  (gnc-account-join-children old-root new-root)
  (xaccAccountBeginEdit new-root)
  (xaccAccountDestroy new-root)
  (gnc-account-merge-children old-root))
