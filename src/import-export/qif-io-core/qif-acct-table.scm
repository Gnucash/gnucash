;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-acct-table.scm
;;;  handle tables of qif-to-gnucash account mappings 
;;;
;;;  Copyright (c) 2001 Linux Developers Group, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; qif-io:acct-table-lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:acct-table-lookup table name type)
  (case type
    ((account)
     (hash-ref (qif-io:acct-table-accounts table) name))
    ((category)
     (hash-ref (qif-io:acct-table-categories table) name))
    ((security)
     (hash-ref (qif-io:acct-table-securities table) name))
    ((brokerage)
     (hash-ref (qif-io:acct-table-brokerage-accts table) name))
    (else
     (throw 'qif-io:unknown-acct-type type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; qif-io:acct-table-insert!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:acct-table-insert! table name type gnc-acct)
  (case type
    ((account)
     (hash-set! (qif-io:acct-table-accounts table) name gnc-acct))
    ((category)
     (hash-set! (qif-io:acct-table-categories table) name gnc-acct))
    ((security)
     (hash-set! (qif-io:acct-table-securities table) name gnc-acct))
    ((brokerage)
     (hash-set! (qif-io:acct-table-brokerage-accts table) name gnc-acct))
    (else
     (throw 'qif-io:unknown-acct-type 'qif-io:acct-table-insert! type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; qif-io:acct-table-make-gnc-group
;; fill in information for the gnucash accounts and organize them
;; in a group tree 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:acct-table-make-gnc-group acct-table qif-file commodity)
  (let ((group (xaccMallocAccountGroup (gnc-get-current-book))))
    ;; poke through the qif-file accounts to see if any of them
    ;; show up in the data 
    (let ((qif-acct-table (qif-io:acct-table-accounts acct-table)))
      (for-each
       (lambda (qif-acct)
         (let* ((name (qif-io:account-name qif-acct))
                (type (qif-io:account-type qif-acct))
                (desc (qif-io:account-description qif-acct))
                (gnc-acct (hash-ref qif-acct-table name)))
           (if (and gnc-acct (not (null? gnc-acct)))
               (let ((gnc-type (qif-io:parse-acct-type type)))
                 (xaccAccountBeginEdit gnc-acct)
                 (if gnc-type 
                     (xaccAccountSetType gnc-acct gnc-type)
                     (xaccAccountSetType gnc-acct GNC-BANK-TYPE))
                 (if desc 
                     (xaccAccountSetDescription gnc-acct desc))
                 (xaccAccountCommitEdit gnc-acct)))))
       (qif-io:file-accounts qif-file))
      
      (hash-fold
       (lambda (name acct p)
         (let ((cmdty (xaccAccountGetCommodity acct)))
           (if (null? cmdty)
               (begin 
                 (xaccAccountBeginEdit acct)
                 (xaccAccountSetCommodity acct commodity)
                 (xaccAccountCommitEdit acct))))
         (let ((type (xaccAccountGetType acct)))
           (if (= type -1)
               (xaccAccountSetType acct GNC-BANK-TYPE)))
         (xaccGroupInsertAccount group acct)
         #t) #t (qif-io:acct-table-accounts acct-table)))

    ;; now the categories 
    (let ((qif-cat-table (qif-io:acct-table-categories acct-table)))
      ;; poke through the qif-file accounts to see if any of them
      ;; show up in the data 
      (for-each
       (lambda (qif-cat)
         (let* ((name (qif-io:category-name qif-cat))
                (income? (qif-io:category-income-cat qif-cat))
                (desc (qif-io:category-description qif-cat))
                (gnc-acct (hash-ref qif-cat-table name)))
           (if (and gnc-acct (not (null? gnc-acct)))
               (begin 
                 (xaccAccountBeginEdit gnc-acct)
                 (cond (income?
                        (xaccAccountSetType gnc-acct GNC-INCOME-TYPE))
                       (#t
                        (xaccAccountSetType gnc-acct GNC-EXPENSE-TYPE)))
                 (xaccAccountSetDescription gnc-acct desc)
                 (xaccAccountCommitEdit gnc-acct)))))
       (qif-io:file-categories qif-file))

      (hash-fold
       (lambda (name acct p)
         (let ((cmdty (xaccAccountGetCommodity acct)))
           (if (not cmdty)
               (begin 
                 (xaccAccountBeginEdit acct)
                 (xaccAccountSetCommodity acct commodity)
                 (xaccAccountCommitEdit acct))))
         (let ((type (xaccAccountGetType acct)))
           (if (= type -1)
               (xaccAccountSetType acct GNC-EXPENSE-TYPE)))
         (xaccGroupInsertAccount group acct)
         #t) #t (qif-io:acct-table-categories acct-table)))

    ;; the securities 

    ;; the other brokerage-related accounts

    group))
