;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-acct-table.scm
;;;  handle tables of qif-to-gnucash account mappings 
;;;
;;;  Copyright (c) 2001 Linux Developers Group, Inc.
;;;  $Id$
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
  (let ((group (gnc:malloc-account-group)))
    ;; poke through the qif-file accounts to see if any of them
    ;; show up in the data 
    (let ((qif-acct-table (qif-io:acct-table-accounts acct-table)))
      (for-each
       (lambda (qif-acct)
         (let* ((name (qif-io:account-name qif-acct))
                (type (qif-io:account-type qif-acct))
                (desc (qif-io:account-description qif-acct))
                (gnc-acct (hash-ref qif-acct-table name)))
           (if gnc-acct
               (let ((gnc-type (qif-io:parse-acct-type type)))
                 (gnc:account-begin-edit gnc-acct)
                 (if gnc-type 
                     (gnc:account-set-type gnc-acct gnc-type)
                     (gnc:account-set-type gnc-acct GNC-BANK-TYPE))
                 (if desc 
                     (gnc:account-set-description gnc-acct desc))
                 (gnc:account-commit-edit gnc-acct)))))
       (qif-io:file-accounts qif-file))
      
      (hash-fold
       (lambda (name acct p)
         (let ((cmdty (gnc:account-get-commodity acct)))
           (if (not cmdty)
               (begin 
                 (gnc:account-begin-edit acct)
                 (gnc:account-set-commodity acct commodity)
                 (gnc:account-commit-edit acct))))
         (let ((type (gnc:account-get-type acct)))
           (if (= type -1)
               (gnc:account-set-type acct GNC-BANK-TYPE)))
         (gnc:group-insert-account group acct)
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
           (if gnc-acct
               (begin 
                 (gnc:account-begin-edit gnc-acct)
                 (cond (income?
                        (gnc:account-set-type gnc-acct GNC-INCOME-TYPE))
                       (#t
                        (gnc:account-set-type gnc-acct GNC-EXPENSE-TYPE)))
                 (gnc:account-set-description gnc-acct desc)
                 (gnc:account-commit-edit gnc-acct)))))
       (qif-io:file-categories qif-file))

      (hash-fold
       (lambda (name acct p)
         (let ((cmdty (gnc:account-get-commodity acct)))
           (if (not cmdty)
               (begin 
                 (gnc:account-begin-edit acct)
                 (gnc:account-set-commodity acct commodity)
                 (gnc:account-commit-edit acct))))
         (let ((type (gnc:account-get-type acct)))
           (if (= type -1)
               (gnc:account-set-type acct GNC-EXPENSE-TYPE)))
         (gnc:group-insert-account group acct)
         #t) #t (qif-io:acct-table-categories acct-table)))

    ;; the securities 

    ;; the other brokerage-related accounts

    group))
