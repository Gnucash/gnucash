;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-bank-xtn-import.scm
;;;  routines for converting a QIF bank-type transaction to a gnc
;;;  transaction
;;;
;;;  Copyright (c) 2001 Linux Developers Group, Inc.
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:bank-xtn-opening-bal-acct 
;;  if this is an "opening balance" transaction, return the 
;;  account name from the transfer field 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:bank-xtn-opening-bal-acct qif-xtn)
  (let ((payee (qif-io:bank-xtn-payee qif-xtn)))
    (if (and (string? payee)
             (string-ci=? payee "Opening Balance"))
        (let ((category (qif-io:bank-xtn-category qif-xtn)))
          (if (string? category)
              (let ((parsed-cat (qif-io:parse-category category)))
                (if (list-ref parsed-cat 1)
                    (car parsed-cat)
                    #f))
              #f))
        #f)))
                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:bank-xtn-import 
;;  translate a single bank transaction into a GNC transaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:bank-xtn-import qif-xtn qif-file gnc-acct-info commodity)
  (let* ((format-info 
          (qif-io:file-bank-xtn-format qif-file))
         (gnc-xtn (gnc:transaction-create (gnc:get-current-book)))
         (near-split-amt
          ;; the u-amount has a larger range and is more correct,
          ;; but is optional
          (let ((uamt (qif-io:bank-xtn-u-amount qif-xtn)))
            (if uamt 
                (qif-io:parse-number/format 
                 uamt (qif-io:bank-xtn-u-amount format-info))
                (qif-io:parse-number/format 
                 (qif-io:bank-xtn-t-amount qif-xtn)
                 (qif-io:bank-xtn-t-amount format-info))))))
    
    ;; utility to make a new split and add it both to an 
    ;; account and to the transaction
    (define (add-split acct-info amount memo reconcile)
      (let* ((acct-name (car acct-info))
             (acct-type (cdr acct-info))
             (acct (qif-io:acct-table-lookup 
                    gnc-acct-info acct-name acct-type))
             (split (gnc:split-create (gnc:get-current-book))))
        ;; make the account if necessary 
        (if (not acct)
            (begin 
              (set! acct (gnc:malloc-account (gnc:get-current-book)))
              (gnc:account-begin-edit acct)
              (gnc:account-set-name acct acct-name)
              (gnc:account-commit-edit acct)
              (qif-io:acct-table-insert! gnc-acct-info 
                                         acct-name acct-type acct)))
        ;; fill in the split 
        (gnc:split-set-amount split amount)
        (gnc:split-set-value split amount)
        (gnc:split-set-memo split memo)
        (gnc:split-set-reconcile split reconcile)
        
        ;; add it to the account and the transaction
        (gnc:account-begin-edit acct)
        (gnc:account-insert-split acct split)
        (gnc:account-commit-edit acct)
        (gnc:transaction-append-split gnc-xtn split)
        split))

    (gnc:transaction-begin-edit gnc-xtn)
    (gnc:transaction-set-commodity gnc-xtn commodity)

    ;; set the transaction date, number and description 
    (let ((date (qif-io:parse-date/format 
                 (qif-io:bank-xtn-date qif-xtn) 
                 (qif-io:bank-xtn-date format-info))))
      (apply gnc:transaction-set-date gnc-xtn date))    
    
    (gnc:transaction-set-xnum gnc-xtn (qif-io:bank-xtn-number qif-xtn))
    (gnc:transaction-set-description gnc-xtn (qif-io:bank-xtn-payee qif-xtn))
    
    ;; create the near split (the one that goes to the source-acct)
    (let* ((near-acct-name (qif-io:bank-xtn-source-acct qif-xtn)))
      (if (not near-acct-name)
          (set! near-acct-name (qif-io:file-default-src-acct qif-file)))
      (add-split (cons near-acct-name 'account) near-split-amt 
                 (qif-io:bank-xtn-memo qif-xtn)
                 (qif-io:parse-cleared-field 
                  (qif-io:bank-xtn-cleared qif-xtn))))
    
    ;; create any far splits.  If no "S" splits were specified, 
    ;; make a magic mirroring split.
    (let ((qif-splits (qif-io:bank-xtn-splits qif-xtn)))
      (if (or (not (list? qif-splits)) (null? qif-splits))
          ;; common case: no splits are specified.  Make one with the
          ;; appropriate category and an amount that's the opposite of
          ;; the near-split amount.  Reuse the memo.
          (let* ((category (qif-io:bank-xtn-category qif-xtn))
                 (parsed-cat 
                  (if category (qif-io:parse-category category) #f))
                 (acct-name 
                  (if parsed-cat (list-ref parsed-cat 0) #f))
                 (acct-is-acct 
                  (if parsed-cat (list-ref parsed-cat 1) #f)))
            (add-split (cons acct-name 
                             (if acct-is-acct 'account 'category))
                       (gnc:numeric-neg near-split-amt)
                       (qif-io:bank-xtn-memo qif-xtn) #\n))
          
          ;; split case: iterate over a list of qif splits and make a
          ;; separate far-end split for each.
          (let ((amt-format 
                 (qif-io:split-amount 
                  (car (qif-io:bank-xtn-splits format-info)))))
            (for-each 
             (lambda (split)
               (let* ((category (qif-io:split-category split))
                      (parsed-cat 
                       (if category (qif-io:parse-category category) #f))
                      (acct-name 
                       (if parsed-cat (list-ref parsed-cat 0) #f))
                      (acct-is-acct 
                       (if parsed-cat (list-ref parsed-cat 1) #f)) 
                      (amount 
                       (qif-io:parse-number/format 
                        (qif-io:split-amount split) amt-format)))
                 (add-split (cons acct-name 
                                  (if acct-is-acct 'account 'category))
                            (gnc:numeric-neg amount)
                            (qif-io:split-memo split) #\n)))
             qif-splits))))
    
    ;; we're done.  
    (gnc:transaction-commit-edit gnc-xtn)
    gnc-xtn))

