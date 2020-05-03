(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (gnucash import-export qif-import))
(use-modules (gnucash import-export string))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-qif-merge-groups")
  (test-gnc:account-tree-get-transactions)
  (test-gnc:account-tree-find-duplicates)
  (test-end "test-qif-merge-groups"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-merge-groups.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (teardown)
  (gnc-clear-current-session))

(define (test-gnc:account-tree-get-transactions)
  (define gnc:account-tree-get-transactions
    (@@ (gnucash import-export qif-import) gnc:account-tree-get-transactions))

  (test-group-with-cleanup "test-gnc:account-tree-get-transactions"
    (create-test-data)

    (test-equal "gnc:account-tree-get-transactions"
      59
      (length
       (gnc:account-tree-get-transactions (gnc-get-current-root-account))))

    (teardown)))

(define (test-gnc:account-tree-find-duplicates)
  (define gnc:account-tree-find-duplicates
    (@@ (gnucash import-export qif-import) gnc:account-tree-find-duplicates))
  (define new-structure
    (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
          (list "Asset"
                (list "Bank")
                (list "Wallet")
                (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
                (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE))))))

  (test-group-with-cleanup "test-gnc:account-tree-find-duplicates"
    (let* ((env (create-test-env))
           (old-alist (create-test-data))
           (old-root (assoc-ref old-alist "Root"))
           (old-bank (assoc-ref old-alist "Bank"))
           (old-expenses (assoc-ref old-alist "Expenses"))
           (old-wallet (assoc-ref old-alist "Wallet"))
           (new-alist (env-create-account-structure-alist env new-structure))
           (new-root (assoc-ref new-alist "Root"))
           (new-bank (assoc-ref new-alist "Bank"))
           (new-expenses (assoc-ref new-alist "Expenses"))
           (new-wallet (assoc-ref new-alist "Wallet")))

      ;; the following are the qif-transactions:
      (define new-txn1 (env-transfer env 01 01 1970 new-bank new-expenses 5))

      ;; note the old-book txn is dated 14.02.1971; the new-book dated
      ;; 20.02.1971 will match because it's less than 1wk away. note
      ;; the old-book txn is a multisplit, but it will still match
      ;; because the bank value is -100.
      (define new-txn2 (env-transfer env 20 02 1971 new-bank new-expenses 100))
      ;; old-book txn dated 13.02.1971 will also match above txn
      (define old-txn2 (env-transfer env 13 02 1971 old-bank old-expenses 100))

      ;; the following imported txn will not match an existing
      ;; txn because the date difference from 14.02.1971 is > 1 week
      (define new-txn3 (env-transfer env 22 02 1971 new-bank new-expenses 100))

      (let ((matches (gnc:account-tree-find-duplicates old-root new-root #f)))
        (test-equal "test-gnc:account-tree-find-duplicates - 2 txns matched"
          2
          (length matches))

        (display "before pruning\n")
        (test-equal "test-gnc:account-tree-find-duplicates - 1st txn matches 1"
          1
          (length (assoc-ref matches new-txn1)))

        (test-equal "test-gnc:account-tree-find-duplicates - 2nd txn matches 2"
          2
          (length (assoc-ref matches new-txn2)))

        (test-equal "test-gnc:account-tree-find-duplicates - 3nd txn matches none"
          #f
          (assoc-ref matches new-txn3))

        (test-assert "mark the new-txn2, 1st match as duplicate"
          (set-cdr! (car (assoc-ref matches new-txn2)) #t))

        (test-assert "gnc:prune-matching-transactions completed"
          (gnc:prune-matching-transactions matches)))

      (let ((matches (gnc:account-tree-find-duplicates old-root new-root #f)))

        (display "after pruning:\n")
        (test-equal "test-gnc:account-tree-find-duplicates - 1st txn matches 1"
          1
          (length (assoc-ref matches new-txn1)))

        (test-equal "test-gnc:account-tree-find-duplicates - 2nd txn destroyed"
          #f
          (assoc-ref matches new-txn2))))

    (teardown)))

