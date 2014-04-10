;; test-create-account.scm
;; load the engine and create an account 

(use-modules (gnucash gnc-module))

(define (run-test)
  (gnc:module-system-init)
  (gnc:module-load "gnucash/engine" 0)

  (let* ((session (gnc:session-new))
         (book (gnc:session-get-book session))
         (group (gnc:malloc-account-group book))
         (acct (gnc:malloc-account book)))
    (gnc:account-begin-edit acct)
    (gnc:account-set-name acct "foo")
    (gnc:account-commit-edit acct)
    (gnc:group-insert-account group acct))
  #t)
