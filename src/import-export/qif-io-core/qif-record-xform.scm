;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-record-xform
;;  routines to convert tag-value lists into various QIF data
;;  structures
;;  
;;  Copyright (c) 2001 Linux Developers Group, Inc.
;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:record->bank-xtn
;; take a list of key-value pairs representing a transaction and 
;; turn them into an actual transaction record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:record->bank-xtn record-pairs)
  (let ((tag #f)
        (value #f)
        (date #f)  
        (number #f)
        (payee #f)
        (memo #f)
        (address #f)
        (cleared #f)
        (t-amount #f)
        (u-amount #f)
        (category #f)
        (split-category #f)
        (split-amount #f)
        (split-memo #f)
        (complete-splits '())
        (split-records '()))
    (for-each 
     (lambda (pair)
       (set! tag (car pair))
       (set! value (cdr pair))
       (case tag         
         ((#\D) (set! date value))         ;; D : transaction date 
         ((#\N) (set! number value))       ;; N : check number 
         ((#\P) (set! payee value))        ;; P : payee
         ((#\M) (set! memo value))         ;; M : memo 
         ((#\T) (set! t-amount value))     ;; T : total amount 
         ((#\U) (set! u-amount value))     ;; U : total amount 
         ((#\C) (set! cleared value))      ;; C : cleared flag 
         ((#\L) (set! category value))     ;; L : category 
         ((#\A)                            ;; A : address 
          ;; multiple "A" lines are appended together with 
          ;; newlines; some Quicken files have a lot of 
          ;; A lines. 
          (if (string? address)
              (set! address 
                    (string-append address "\n" value))
              (set! address value)))
         
         ((#\S)                            ;; S : split category 
          ;; if we have already seen another split, this S line 
          ;; finishes it and starts a new one
          (if split-category
              (begin 
                (set! complete-splits 
                      (cons (list split-category split-amount split-memo)
                            complete-splits))
                (set! split-category value)
                (set! split-amount #f)
                (set! split-memo #f))
              (set! split-category value)))
         ((#\E) (set! split-memo value))       ;; E : split memo 
         ((#\$) (set! split-amount value))))
     record-pairs)
     
    ;; if there's an open split, do the right thing 
    (if (string? split-category)
        (set! complete-splits 
              (cons (list split-category split-amount split-memo)
                    complete-splits)))

    ;; convert the splits to split records 
    ;; (reversing the list again to get the right order)
    (for-each 
     (lambda (split)
       (set! split-records 
             (cons (qif-io:make-split (car split)
                                      (cadr split)
                                      (caddr split))
                   split-records)))
     complete-splits)

    ;; check for bogosity and make a record if everything's ok
    (if (and date t-amount)
        (qif-io:make-bank-xtn #f date number payee memo 
                              t-amount u-amount cleared category 
                              address split-records)
        (throw 'qif-io:record-error 'qif-io:record->bank-xtn record-pairs))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:bank-xtn->record
;; turn a bank-xtn into tag-value pairs.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:bank-xtn->record bank-xtn) 
  (let ((kvp '()))
    (let ((date (qif-io:bank-xtn-date bank-xtn)))
      (if date
          (set! kvp (cons (cons #\D date) kvp))))
    (let ((number (qif-io:bank-xtn-number bank-xtn)))
      (if number
          (set! kvp (cons (cons #\N number) kvp))))
    (let ((payee (qif-io:bank-xtn-payee bank-xtn)))
      (if payee
          (set! kvp (cons (cons #\P payee) kvp))))
    (let ((memo (qif-io:bank-xtn-memo bank-xtn)))
      (if memo
          (set! kvp (cons (cons #\M memo) kvp))))
    (let ((t-amount (qif-io:bank-xtn-t-amount bank-xtn)))
      (if t-amount
          (set! kvp (cons (cons #\T t-amount) kvp))))
    (let ((u-amount (qif-io:bank-xtn-u-amount bank-xtn)))
      (if u-amount
          (set! kvp (cons (cons #\U u-amount) kvp))))
    (let ((cleared (qif-io:bank-xtn-cleared bank-xtn)))
      (if cleared
          (set! kvp (cons (cons #\C cleared) kvp))))
    (let ((category (qif-io:bank-xtn-category bank-xtn)))
      (if category
          (set! kvp (cons (cons #\L category) kvp))))
    (let ((address (qif-io:bank-xtn-address bank-xtn)))
      (if address
          (with-input-from-string address
            (lambda ()
              (let loop ((line (read-line)))
                (if (not (eof-object? line))
                    (begin 
                      (set! kvp (cons (cons #\A line) kvp))
                      (loop (read-line)))))))))
    (let ((splits (qif-io:bank-xtn-splits bank-xtn)))
      (for-each
       (lambda (split)
         (let ((split-cat (qif-io:split-category split))
               (split-memo (qif-io:split-memo split))
               (split-amount (qif-io:split-amount split)))
           (if split-cat
               (set! kvp (cons (cons #\S split-cat) kvp))
               (if (or split-memo split-amount) 
                   (set! kvp (cons (cons #\S "") kvp))))
           (if split-memo
               (set! kvp (cons (cons #\E split-memo) kvp)))
           (if split-amount
               (set! kvp (cons (cons #\$ split-amount) kvp)))))
       splits))
    (reverse! kvp)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:record->invst-xtn
;; take a list of key-value pairs representing a transaction and 
;; turn them into an actual transaction record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:record->invst-xtn record-pairs)
  (let ((tag #f)
        (value #f)
        (date #f)  
        (action #f)
        (payee #f)
        (memo #f)
        (address #f)
        (cleared #f)
        (t-amount #f)
        (u-amount #f)
        (security #f)
        (category #f)
        (commission #f)
        ($-amount #f)
        (share-price #f)
        (share-amount #f))
    (for-each 
     (lambda (pair)
       (set! tag (car pair))
       (set! value (cdr pair))
       (case tag         
         ((#\D) (set! date value))         ;; D : transaction date 
         ((#\N) (set! action value))       ;; N : investment action
         ((#\P) (set! payee value))        ;; P : payee
         ((#\M) (set! memo value))         ;; M : memo 
         ((#\T) (set! t-amount value))     ;; T : total amount 
         ((#\U) (set! u-amount value))     ;; U : total amount 
         ((#\$) (set! $-amount value))     ;; $ : total amount 
         ((#\Y) (set! security value))     ;; Y : security
         ((#\I) (set! share-price value))  ;; I : share price
         ((#\Q) (set! share-amount value)) ;; Q : share quantity 
         ((#\O) (set! commission value))   ;; O : commission
         ((#\C) (set! cleared value))      ;; C : cleared flag 
         ((#\L) (set! category value))     ;; L : category 
         ((#\A)                            ;; A : address 
          ;; multiple "A" lines are appended together with 
          ;; newlines; some Quicken files have a lot of 
          ;; A lines. 
          (if (string? address)
              (set! address 
                    (string-append address "\n" value))
              (set! address value)))))
     record-pairs)
    (qif-io:make-invst-xtn #f date action security payee memo t-amount 
                           u-amount $-amount share-price share-amount 
                           commission cleared category address)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:invst-xtn->record
;; turn a invst-xtn into tag-value pairs.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:invst-xtn->record invst-xtn) 
  (let ((kvp '()))
    (let ((date (qif-io:invst-xtn-date invst-xtn)))
      (if date
          (set! kvp (cons (cons #\D date) kvp))))
    (let ((action (qif-io:invst-xtn-action invst-xtn)))
      (if action
          (set! kvp (cons (cons #\N action) kvp))))
    (let ((payee (qif-io:invst-xtn-payee invst-xtn)))
      (if payee
          (set! kvp (cons (cons #\P payee) kvp))))
    (let ((security (qif-io:invst-xtn-security invst-xtn)))
      (if security
          (set! kvp (cons (cons #\Y security) kvp))))
    (let ((share-price (qif-io:invst-xtn-share-price invst-xtn)))
      (if share-price
          (set! kvp (cons (cons #\I share-price) kvp))))
    (let ((share-amount (qif-io:invst-xtn-share-amount invst-xtn)))
      (if share-amount
          (set! kvp (cons (cons #\Q share-amount) kvp))))
    (let ((t-amount (qif-io:invst-xtn-t-amount invst-xtn)))
      (if t-amount
          (set! kvp (cons (cons #\T t-amount) kvp))))
    (let ((u-amount (qif-io:invst-xtn-u-amount invst-xtn)))
      (if u-amount
          (set! kvp (cons (cons #\U u-amount) kvp))))
    (let ((commission (qif-io:invst-xtn-commission invst-xtn)))
      (if commission
          (set! kvp (cons (cons #\O commission) kvp))))
    (let ((cleared (qif-io:invst-xtn-cleared invst-xtn)))
      (if cleared
          (set! kvp (cons (cons #\C cleared) kvp)))) 
    (let ((address (qif-io:invst-xtn-address invst-xtn)))
      (if address
          (with-input-from-string address
            (lambda ()
              (let loop ((line (read-line)))
                (if (not (eof-object? line))
                    (begin 
                      (set! kvp (cons (cons #\A line) kvp))
                      (loop (read-line)))))))))
    (let ((memo (qif-io:invst-xtn-memo invst-xtn)))
      (if memo
          (set! kvp (cons (cons #\M memo) kvp))))
    (let ((category (qif-io:invst-xtn-category invst-xtn)))
      (if category
          (set! kvp (cons (cons #\L category) kvp))))
    (let (($-amount (qif-io:invst-xtn-$-amount invst-xtn)))
      (if $-amount
          (set! kvp (cons (cons #\$ $-amount) kvp))))
    (reverse! kvp '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:record->account
;; take a list of key-value pairs representing a transaction and 
;; turn them into an actual transaction record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:record->account record-pairs)
  (let ((tag #f)
        (value #f)
        (name #f)
        (type #f)
        (description #f)
        (limit #f)
        (budget #f))
    (for-each 
     (lambda (pair)
       (set! tag (car pair))
       (set! value (cdr pair))
       (case tag         
         ((#\N) (set! name value))         ;; N : account name
         ((#\D) (set! description value))  ;; D : account descrip
         ((#\T) (set! type value))         ;; T : account type
         ((#\L) (set! limit value))        ;; L : credit limit
         ((#\B) (set! budget value))))     ;; B : budget amount (?)
     record-pairs)
    (qif-io:make-account name type description limit budget)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:account->record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:account->record acct)
  (let ((kvp '()))
    (let ((name (qif-io:account-name acct)))
      (if name
          (set! kvp (cons (cons #\N name) kvp))))
    (let ((type (qif-io:account-type acct)))
      (if type
          (set! kvp (cons (cons #\T type) kvp))))
    (let ((description (qif-io:account-description acct)))
      (if description
          (set! kvp (cons (cons #\D description) kvp))))
    (let ((limit (qif-io:account-limit acct)))
      (if limit
          (set! kvp (cons (cons #\L limit) kvp))))
    (let ((budget (qif-io:account-budget acct)))
      (if budget
          (set! kvp (cons (cons #\B budget) kvp))))
    (reverse! kvp '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:record->category
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:record->category record-pairs)
  (let ((tag #f)
        (value #f)
        (name #f)
        (taxable #f)
        (description #f)
        (expense-cat #f)
        (income-cat #f)
        (tax-class #f)
        (budget-amt #f))
    (for-each 
     (lambda (pair)
       (set! tag (car pair))
       (set! value (cdr pair))
       (case tag         
         ((#\N) (set! name value))         
         ((#\D) (set! description value))  
         ((#\T) (set! taxable value))      
         ((#\E) (set! expense-cat value))  
         ((#\I) (set! income-cat value))   
         ((#\R) (set! tax-class value))    
         ((#\B) (set! budget-amt value))))  
     record-pairs)
    (qif-io:make-category name description taxable 
                          expense-cat income-cat tax-class budget-amt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:category->record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:category->record cat)
  (let ((kvp '()))
    (let ((name (qif-io:category-name cat)))
      (if name
          (set! kvp (cons (cons #\N name) kvp))))
    (let ((description (qif-io:category-description cat)))
      (if description
          (set! kvp (cons (cons #\D description) kvp))))
    (let ((taxable (qif-io:category-taxable cat)))
      (if taxable
          (set! kvp (cons (cons #\T taxable) kvp))))
    (let ((tax-class (qif-io:category-tax-class cat)))
      (if tax-class
          (set! kvp (cons (cons #\R tax-class) kvp))))
    (let ((expense-cat (qif-io:category-expense-cat cat)))
      (if expense-cat
          (set! kvp (cons (cons #\E expense-cat) kvp))))
    (let ((income-cat (qif-io:category-income-cat cat)))
      (if income-cat
          (set! kvp (cons (cons #\I income-cat) kvp))))
    (let ((budget-amt (qif-io:category-budget-amt cat)))
      (if budget-amt
          (set! kvp (cons (cons #\B budget-amt) kvp))))
    (reverse! kvp '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:record->class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:record->class record-pairs)
  (let ((tag #f)
        (value #f)
        (name #f)
        (description #f))
    (for-each 
     (lambda (pair)
       (set! tag (car pair))
       (set! value (cdr pair))
       (case tag         
         ((#\N) (set! name value))         
         ((#\D) (set! description value))))
     record-pairs)
    (qif-io:make-class name description)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:class->record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:class->record class)
  (let ((kvp '()))
    (let ((name (qif-io:class-name class)))
      (if name
          (set! kvp (cons (cons #\N name) kvp))))
    (let ((description (qif-io:class-description class)))
      (if description
          (set! kvp (cons (cons #\D description) kvp))))
    (reverse! kvp '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:record->security
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:record->security record-pairs)
  (let ((tag #f)
        (value #f)
        (name #f)
        (symbol #f)
        (type #f))
    (for-each 
     (lambda (pair)
       (set! tag (car pair))
       (set! value (cdr pair))
       (case tag         
         ((#\N) (set! name value))         
         ((#\S) (set! symbol value))
         ((#\T) (set! type value))))
     record-pairs)
    (qif-io:make-security name symbol type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-io:security->record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:security->record security)
  (let ((kvp '()))
    (let ((name (qif-io:security-name security)))
      (if name
          (set! kvp (cons (cons #\N name) kvp))))
    (let ((type (qif-io:security-type security)))
      (if type
          (set! kvp (cons (cons #\T type) kvp))))
    (let ((symbol (qif-io:security-symbol security)))
      (if symbol
          (set! kvp (cons (cons #\S symbol) kvp))))
    (reverse! kvp '())))



