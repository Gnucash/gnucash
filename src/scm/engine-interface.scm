;; engine-interface.scm -- support for working with the GnuCash
;;                         engine data structures
;; Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>
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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;; This defines a scheme representation of splits.
(define gnc:split-structure
  (make-record-type
   "gnc:split-structure"
   '(split-guid account-guid transaction-guid memo action docref
                reconcile-state reconciled-date share-amount share-price)))

(define gnc:make-split-scm
  (record-constructor gnc:split-structure))

(define gnc:split-scm?
  (record-predicate gnc:split-structure))

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

(define gnc:split-scm-get-docref
  (record-accessor gnc:split-structure 'docref))

(define gnc:split-scm-get-reconcile-state
  (record-accessor gnc:split-structure 'reconcile-state))

(define gnc:split-scm-get-reconciled-date
  (record-accessor gnc:split-structure 'reconciled-date))

(define gnc:split-scm-get-share-amount
  (record-accessor gnc:split-structure 'share-amount))

(define gnc:split-scm-get-share-price
  (record-accessor gnc:split-structure 'share-price))

;; This function take a C split and returns a representation
;; of it as a split-structure.
(define (gnc:split->split-scm split)
  (gnc:make-split-scm
   (gnc:split-get-guid split)
   (gnc:account-get-guid (gnc:split-get-account split))
   (gnc:transaction-get-guid (gnc:split-get-parent split))
   (gnc:split-get-memo split)
   (gnc:split-get-action split)
   (gnc:split-get-docref split)
   (gnc:split-get-reconcile-state split)
   (gnc:split-get-reconciled-date split)
   (gnc:split-get-share-amount split)
   (gnc:split-get-share-price split)))

;; gnc:split-copy is a form of gnc:split->split-scm used by C routines.
;; It stores the split in an internal variable so C can safely register
;; it before it gets garbage collected.
(define gnc:copy-split #f)
(let ((last-split #f))
  (set! gnc:copy-split
        (lambda (split)
          (set! last-split (gnc:split->split-scm split))
          last-split)))

;; Copy a scheme representation of a split onto a C split.
;; If possible, insert the C split into the account of the
;; scheme split. Not all values are copied. The reconcile
;; status and date are not copied. The C split's guid is,
;; of course, unchanged.
(define (gnc:split-scm-onto-split split-scm split)
  (if (pointer-token-null? split)
      #f
      (begin
        (let ((account (gnc:account-lookup
                        (gnc:split-scm-get-account-guid split-scm))))
          (if (and account (gnc:account-can-insert-split? account split))
              (gnc:account-insert-split account split)))
        (gnc:split-set-memo split (gnc:split-scm-get-memo split-scm))
        (gnc:split-set-action split (gnc:split-scm-get-action split-scm))
        (gnc:split-set-docref split (gnc:split-scm-get-docref split-scm))
        (gnc:split-set-share-price-and-amount
         split
         (gnc:split-scm-get-share-price split-scm)
         (gnc:split-scm-get-share-amount split-scm)))))

;; Returns true if we can insert the C split into the given account.
(define (gnc:account-can-insert-split? account split)
  (let ((currency (gnc:account-get-currency account))
        (security (gnc:account-get-security account))
        (trans    (gnc:split-get-parent split)))
    (or (< (gnc:transaction-get-split-count trans) 2)
        (gnc:transaction-is-common-currency trans currency)
        (gnc:transaction-is-common-currency trans security))))


;; Defines a scheme representation of a transaction.
(define gnc:transaction-structure
  (make-record-type
   "gnc:transaction-structure"
   '(transaction-guid date-entered date-posted num description docref splits)))

(define gnc:make-transaction-scm
  (record-constructor gnc:transaction-structure))

(define gnc:transaction-scm?
  (record-predicate gnc:transaction-structure))

(define gnc:transaction-scm-get-transaction-guid
  (record-accessor gnc:transaction-structure 'transaction-guid))

(define gnc:transaction-scm-get-date-entered
  (record-accessor gnc:transaction-structure 'date-entered))

(define gnc:transaction-scm-get-date-posted
  (record-accessor gnc:transaction-structure 'date-posted))

(define gnc:transaction-scm-get-num
  (record-accessor gnc:transaction-structure 'num))

(define gnc:transaction-scm-get-description
  (record-accessor gnc:transaction-structure 'description))

(define gnc:transaction-scm-get-docref
  (record-accessor gnc:transaction-structure 'docref))

(define gnc:transaction-scm-get-splits
  (record-accessor gnc:transaction-structure 'splits))

;; This function takes a C transaction and returns
;; a representation of it as a transaction-structure.
(define (gnc:transaction->transaction-scm trans)
  (define (trans-splits i)
    (let (split ((gnc:transaction-get-split trans i)))
      (if (pointer-token-null? split)
          '()
          (cons split (trans-splits (+ i 1))))))
  (gnc:make-transaction-scm
   (gnc:transaction-get-guid trans)
   (gnc:transaction-get-date-entered trans)
   (gnc:transaction-get-date-posted trans)
   (gnc:transaction-get-num trans)
   (gnc:transaction-get-description trans)
   (gnc:transaction-get-docref trans)
   (trans-splits 0)))


;; Return a scheme symbol identifying the type of guid passed in.
(define gnc:guid-type #f)
(let ()
  (define entity-types (vector 'gnc-id-none
                               'gnc-id-null
                               'gnc-id-group
                               'gnc-id-account
                               'gnc-id-trans
                               'gnc-id-split))
  (set! gnc:guid-type
        (lambda (guid)
          (vector-ref entity-types (gnc:guid-type-helper guid)))))
