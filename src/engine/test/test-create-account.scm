;; test-create-account.scm
;; load the engine and create an account 

(use-modules (gnucash gnc-module))

(define (run-test)
  (gnc:module-system-init)
  (gnc:module-load "gnucash/engine" 0)

  (let* ((session (qof-session-new))
         (book (qof-session-get-book session))
         (root (xaccMallocAccount book))
         (acct (xaccMallocAccount book)))
    (xaccAccountBeginEdit acct)
    (xaccAccountSetName acct "foo")
    (xaccAccountCommitEdit acct)
    (gnc-account-append-child root acct))
  #t)
