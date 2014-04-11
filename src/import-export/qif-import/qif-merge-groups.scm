;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-merge-groups.scm
;;;  eliminate duplicate xtns in a new (imported) account group
;;;
;;;  Copyright 2001 Bill Gribble <grib@billgribble.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:account-tree-get-transactions
;;
;;  Given an account tree, this procedure returns a list of all
;;  transactions whose splits only use accounts in the tree.
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
;;  Given two account trees, old-root and new-root, a search is
;;  performed to determine, for each transaction in new-root,
;;  whether there are any transactions in old-root that may be
;;  duplicated by it.
;;
;;  The search results are returned in an association list, with
;;  new-root transactions as the keys. The value associated with
;;  each key is a second association list of possibly duplicated
;;  transactions in the old-root, taking the form:
;;  ( (old-xtn . #f) (old-xtn . #f) (old-xtn . #f) ... )
;;
;;  The druid can then ask the user for a final determination,
;;  and change #f to #t where duplication is found.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:account-tree-find-duplicates old-root new-root progress-dialog)

  ;; This procedure does all the work. We'll define it, then call it safely.
  (define (private-find)

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
                 (work-to-do (length new-xtns))
                 (work-done 0)
                 (matches '()))

            ;; This procedure handles progress reporting, pause, and cancel.
            (define (update-progress)
              (set! work-done (+ 1 work-done))
              (if (and progress-dialog
                       (zero? (remainder work-done 8)))
                  (begin
                    (gnc-progress-dialog-set-value progress-dialog
                                                   (/ work-done work-to-do))
                    (qif-import:check-pause progress-dialog)
                    (if qif-import:canceled
                        (throw 'cancel)))))


            (if progress-dialog
                (gnc-progress-dialog-set-sub progress-dialog
                                         (_ "Finding duplicate transactions")))

            ;; For each transaction in the new account tree, build a query
            ;; that matches possibly duplicate transactions in the old tree.
            (for-each
              (lambda (xtn)
                (let ((query (qof-query-create-for-splits))
                      (num-splits 0))
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

                  ;; For each split in the new transaction, add a
                  ;; term that can match on its properties.
                  (let ((q-splits (qof-query-create-for-splits)))
                    (for-each
                      (lambda (split)
                        (set! num-splits (+ num-splits 1))
                        (let ((sq (qof-query-create-for-splits)))
                          (qof-query-set-book sq (gnc-account-get-book old-root))

                          ;; Require a match on the account name. If the name
                          ;; doesn't exist in the old tree (indicating a new
                          ;; account), the match will be NULL and the query
                          ;; won't find anything.  Optimize this later.
                          (xaccQueryAddSingleAccountMatch
                            sq
                            (gnc-account-lookup-by-full-name old-root
                              (gnc-account-get-full-name
                                (xaccSplitGetAccount split)))
                            QOF-QUERY-AND)

                          ;; Require the value of the split in the new tree
                          ;; to match the the value of the split in the old
                          ;; tree.  We should really check for fuzziness.
                          (xaccQueryAddValueMatch sq
                                                  (xaccSplitGetValue split)
                                                  QOF-NUMERIC-MATCH-ANY
                                                  QOF-COMPARE-EQUAL
                                                  QOF-QUERY-AND)

                          ;; Now merge into the split query.  Reminder: q-splits
                          ;; must be merged with an OR. Otherwise, nothing will
                          ;; match. (For example, something can be equal to 4 or
                          ;; to -4, but not both.)
                          (let ((q-new (qof-query-merge q-splits
                                                        sq
                                                        QOF-QUERY-OR)))
                            (qof-query-destroy q-splits)
                            (qof-query-destroy sq)
                            (set! q-splits q-new))))
                      (xaccTransGetSplitList xtn))

                    ;; Now q-splits will find every split that is the same as
                    ;; any one split of the new-root transaction.  Merge it in.
                    (let ((q-new (qof-query-merge query
                                                  q-splits
                                                  QOF-QUERY-AND)))
                      (qof-query-destroy query)
                      (qof-query-destroy q-splits)
                      (set! query q-new)))

                  ;; Now that we have built a query that finds matching splits
                  ;; in the old tree, run it and build a list of transactions
                  ;; from the results.
                  ;;
                  ;; If the transaction from the new tree has more than two
                  ;; splits, then we'll assume that it fully reflects what
                  ;; occurred, and only consider transactions in the old tree
                  ;; that match with every single split.
                  ;;
                  ;; All other new transactions could be incomplete, so we'll
                  ;; consider transactions from the old tree to be possible
                  ;; duplicates even if only one split matches.
                  ;;
                  ;; For more information, see bug 481528.
                  (let ((old-xtns (xaccQueryGetTransactions
                                    query
                                    (if (> num-splits 2)
                                        QUERY-TXN-MATCH-ALL
                                        QUERY-TXN-MATCH-ANY))))

                    ;; Turn the resulting list of possibly duplicated
                    ;; transactions into an association list.
                    (set! old-xtns (map
                                     (lambda (elt)
                                       (cons elt #f)) old-xtns))

                    ;; If anything matched the query, add it to our "matches"
                    ;; association list, keyed by the new-root transaction.
                    (if (not (null? old-xtns))
                        (set! matches (cons (cons xtn old-xtns) matches))))

                  (qof-query-destroy query))
                (update-progress))
              new-xtns)

            ;; Finished.
            (if progress-dialog
                (gnc-progress-dialog-set-value progress-dialog 1))

            ;; Return the matches.
            matches)

          ;; Since there are either no accounts or no transactions in the old
          ;; tree, duplicate checking is unnecessary.
          (begin
            ;; Finished.
            (if progress-dialog
                (gnc-progress-dialog-set-value progress-dialog 1))

            ;; Return an empty list.
            '()))))

  ;; Safely do the work and return the result.
  (gnc:backtrace-if-exception
    (lambda () (catch 'cancel private-find (lambda (key . args) #t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:prune-matching-transactions
;;
;;  The parameter, match-list, is an association list of the form
;;  returned by gnc:account-tree-find-duplicates. This procedure
;;  looks through the list and discards any transaction that has
;;  been definitively determined to be a duplicate of one of the
;;  possible matches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:account-tree-catenate-and-merge
;;
;;  The procedure moves the entire contents of one account tree,
;;  new-root, to a second account tree, old-root, and merges any
;;  duplicated accounts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (gnc:account-tree-catenate-and-merge old-root new-root)
  ;; stuff the new accounts into the old account tree and merge the accounts
  (gnc-account-join-children old-root new-root)
  (xaccAccountBeginEdit new-root)
  (xaccAccountDestroy new-root)
  (gnc-account-merge-children old-root))
