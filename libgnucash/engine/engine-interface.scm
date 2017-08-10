;; engine-interface.scm -- support for working with the GnuCash
;;                         engine data structures
;; Copyright (C) 2000 Dave Peticolas <dave@krondo.com>
;;                                                                  
;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org

;; This defines a scheme representation of splits.
(define gnc:split-structure
  (make-record-type
   "gnc:split-structure"
   '(split-guid account-guid transaction-guid memo action
                reconcile-state reconciled-date amount value)))

;; constructor
(define gnc:make-split-scm
  (record-constructor gnc:split-structure))

;; type predicate
(define gnc:split-scm?
  (record-predicate gnc:split-structure))

;; accessors
(define gnc:split-scm-get-split-guid
  (record-accessor gnc:split-structure 'split-guid))

(define gnc:split-scm-get-account-guid
  (record-accessor gnc:split-structure 'account-guid))

(define gnc:split-scm-get-transaction-guid
  (record-accessor gnc:split-structure 'transaction-guid))

(define gnc:split-scm-get-memo
  (record-accessor gnc:split-structure 'memo))

(define gnc:split-scm-get-action
  (record-accessor gnc:split-structure 'action))

(define gnc:split-scm-get-reconcile-state
  (record-accessor gnc:split-structure 'reconcile-state))

(define gnc:split-scm-get-reconciled-date
  (record-accessor gnc:split-structure 'reconciled-date))

(define gnc:split-scm-get-amount
  (record-accessor gnc:split-structure 'amount))

(define gnc:split-scm-get-value
  (record-accessor gnc:split-structure 'value))

;; modifiers
(define gnc:split-scm-set-split-guid
  (record-modifier gnc:split-structure 'split-guid))

(define gnc:split-scm-set-account-guid
  (record-modifier gnc:split-structure 'account-guid))

(define gnc:split-scm-set-transaction-guid
  (record-modifier gnc:split-structure 'transaction-guid))

(define gnc:split-scm-set-memo
  (record-modifier gnc:split-structure 'memo))

(define gnc:split-scm-set-action
  (record-modifier gnc:split-structure 'action))

(define gnc:split-scm-set-reconcile-state
  (record-modifier gnc:split-structure 'reconcile-state))

(define gnc:split-scm-set-reconciled-date
  (record-modifier gnc:split-structure 'reconciled-date))

(define gnc:split-scm-set-amount
  (record-modifier gnc:split-structure 'amount))

(define gnc:split-scm-set-value
  (record-modifier gnc:split-structure 'value))

;; This function take a C split and returns a representation
;; of it as a split-structure. Assumes the transaction is open
;; for editing.
(define (gnc:split->split-scm split use-cut-semantics?)
  (gnc:make-split-scm
   (gncSplitGetGUID split)
   (gncAccountGetGUID (xaccSplitGetAccount split))
   (gncTransGetGUID (xaccSplitGetParent split))
   (xaccSplitGetMemo split)
   (xaccSplitGetAction split)
   (xaccSplitGetReconcile split)
   (gnc-split-get-date-reconciled split)
   (xaccSplitGetAmount split)
   (xaccSplitGetValue split)))

;; Copy a scheme representation of a split onto a C split.
;; If possible, insert the C split into the account of the
;; scheme split. Not all values are copied. The reconcile
;; status and date are not copied. The C split's guid is,
;; of course, unchanged.
(define (gnc:split-scm-onto-split split-scm split book)
  (if (null? split)
      #f
      (begin
        (let ((memo     (gnc:split-scm-get-memo split-scm))
              (action   (gnc:split-scm-get-action split-scm))
              (amount   (gnc:split-scm-get-amount split-scm))
              (value    (gnc:split-scm-get-value split-scm)))
          (if memo     (xaccSplitSetMemo split memo))
          (if action   (xaccSplitSetAction split action))
          (if amount   (xaccSplitSetAmount split amount))
          (if value    (xaccSplitSetValue split value)))
        (let ((account (xaccAccountLookup
                        (gnc:split-scm-get-account-guid split-scm)
                        book)))
          (if (not (null? account))
              (begin
                (xaccAccountBeginEdit account)
                (xaccSplitSetAccount split account)
                (xaccAccountCommitEdit account)))))))

;; Defines a scheme representation of a transaction.
(define gnc:transaction-structure
  (make-record-type
   "gnc:transaction-structure"
   '(transaction-guid currency date-entered date-posted
                      num description notes split-scms)))

;; constructor
(define gnc:make-transaction-scm
  (record-constructor gnc:transaction-structure))

;; type predicate
(define gnc:transaction-scm?
  (record-predicate gnc:transaction-structure))

;; accessors
(define gnc:transaction-scm-get-transaction-guid
  (record-accessor gnc:transaction-structure 'transaction-guid))

(define gnc:transaction-scm-get-currency
  (record-accessor gnc:transaction-structure 'currency))

(define gnc:transaction-scm-get-date-entered
  (record-accessor gnc:transaction-structure 'date-entered))

(define gnc:transaction-scm-get-date-posted
  (record-accessor gnc:transaction-structure 'date-posted))

(define gnc:transaction-scm-get-num
  (record-accessor gnc:transaction-structure 'num))

(define gnc:transaction-scm-get-description
  (record-accessor gnc:transaction-structure 'description))

(define gnc:transaction-scm-get-notes
  (record-accessor gnc:transaction-structure 'notes))

(define gnc:transaction-scm-get-split-scms
  (record-accessor gnc:transaction-structure 'split-scms))

(define (gnc:transaction-scm-get-split-scm trans-scm index)
  (let ((split-scms (gnc:transaction-scm-get-split-scms trans-scm)))
    (cond ((< index 0) #f)
          ((not (pair? split-scms)) #f)
          ((>= index (length split-scms)) #f)
          (else (list-ref split-scms index)))))

(define (gnc:transaction-scm-get-other-split-scm trans-scm split-scm)
  (let ((split-scms (gnc:transaction-scm-get-split-scms trans-scm)))
  (cond ((not (= (length split-scms) 2)) #f)
        ((eq? split-scm (car split-scms)) (cadr split-scms))
        (else (car split-scms)))))

;; modifiers
(define gnc:transaction-scm-set-transaction-guid
  (record-modifier gnc:transaction-structure 'transaction-guid))

(define gnc:transaction-scm-set-currency
  (record-modifier gnc:transaction-structure 'currency))

(define gnc:transaction-scm-set-date-entered
  (record-modifier gnc:transaction-structure 'date-entered))

(define gnc:transaction-scm-set-date-posted
  (record-modifier gnc:transaction-structure 'date-posted))

(define gnc:transaction-scm-set-num
  (record-modifier gnc:transaction-structure 'num))

(define gnc:transaction-scm-set-description
  (record-modifier gnc:transaction-structure 'description))

(define gnc:transaction-scm-set-notes
  (record-modifier gnc:transaction-structure 'notes))

(define gnc:transaction-scm-set-split-scms
  (record-modifier gnc:transaction-structure 'split-scms))

(define (gnc:transaction-scm-append-split-scm trans-scm split-scm)
  (let ((split-scms (gnc:transaction-scm-get-split-scms trans-scm)))
    (gnc:transaction-scm-set-split-scms
     trans-scm (append split-scms (list split-scm)))))

;; This function takes a C transaction and returns
;; a representation of it as a transaction-structure.
(define (gnc:transaction->transaction-scm trans use-cut-semantics?)
  (define (trans-splits i)
    (let ((split (xaccTransGetSplit trans i)))
      (if (null? split)
          '()
          (cons (gnc:split->split-scm split use-cut-semantics?)
                (trans-splits (+ i 1))))))
  (gnc:make-transaction-scm
   (gncTransGetGUID trans)
   (xaccTransGetCurrency trans)
   (gnc-transaction-get-date-entered trans)
   (if use-cut-semantics?
       (gnc-transaction-get-date-posted trans)
       #f)
   (if use-cut-semantics?
       (xaccTransGetNum trans)
       #f)
   (xaccTransGetDescription trans)
   (xaccTransGetNotes trans)
   (trans-splits 0)))

;; Copy a scheme representation of a transaction onto a C transaction.
;; guid-mapping must be an alist, mapping guids to guids. This list is
;; used to use alternate account guids when creating splits.
(define (gnc:transaction-scm-onto-transaction trans-scm trans guid-mapping
                                              commit? book)
  (if (null? trans)
      #f
      (begin
        ;; open the transaction for editing
        (if (not (xaccTransIsOpen trans))
            (xaccTransBeginEdit trans))

        ;; copy in the transaction values
        (let ((currency    (gnc:transaction-scm-get-currency trans-scm))
              (description (gnc:transaction-scm-get-description trans-scm))
              (num         (gnc:transaction-scm-get-num trans-scm))
              (notes       (gnc:transaction-scm-get-notes trans-scm))
              (date-posted (gnc:transaction-scm-get-date-posted trans-scm)))
          (if currency    (xaccTransSetCurrency trans currency))
          (if description (xaccTransSetDescription trans description))
          (if num         (xaccTransSetNum trans num))
          (if notes       (xaccTransSetNotes trans notes))
          (if date-posted (gnc-transaction-set-date
                           trans date-posted)))

        ;; strip off the old splits
        (for-each (lambda (split)
                    (xaccSplitDestroy split))
                  (xaccTransGetSplitList trans))

        ;; and put on the new ones! Please note they go in the *same*
        ;; order as in the original transaction. This is important.
        (for-each 
         (lambda (split-scm) 
           (let* ((new-split (xaccMallocSplit book))
                  (old-guid  (gnc:split-scm-get-account-guid split-scm))
                  (new-guid  (assoc-ref guid-mapping old-guid)))
             (if (not new-guid)
                 (set! new-guid old-guid))
             (gnc:split-scm-set-account-guid split-scm new-guid)
             (gnc:split-scm-onto-split split-scm new-split book)
             (gnc:split-scm-set-account-guid split-scm old-guid)
             (xaccSplitSetParent new-split trans)))
         (gnc:transaction-scm-get-split-scms trans-scm))
        
        ;; close the transaction
        (if commit?
            (xaccTransCommitEdit trans)))))
