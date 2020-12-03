;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash engine business-core))

(eval-when (compile load eval expand)
  (load-extension "libgnucash-guile" "gnc_guile_bindings_init"))

(use-modules (sw_engine))
(use-modules (srfi srfi-1))

(export gnc:owner-get-address)
(export gnc:owner-get-name-dep)
(export gnc:owner-get-address-dep)
(export gnc:owner-get-name-and-address-dep)
(export gnc:owner-get-owner-id)
(export gnc:owner-from-split)
(export gnc:split->owner)

(define (gnc:owner-get-address owner)
  (let ((type (gncOwnerGetType owner)))
    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (let ((c (gncOwnerGetCustomer owner)))
	 (gncCustomerGetAddr c)))
      ((eqv? type GNC-OWNER-VENDOR)
       (let ((v (gncOwnerGetVendor owner)))
	 (gncVendorGetAddr v)))
      ((eqv? type GNC-OWNER-EMPLOYEE)
       (let ((e (gncOwnerGetEmployee owner)))
	 (gncEmployeeGetAddr e)))
      ((eqv? type GNC-OWNER-JOB)
       (gnc:owner-get-address (gncJobGetOwner
			       (gncOwnerGetJob owner))))
      (else '()))))

;
; The -dep functions return combined strings of the appropriate
; content.  When multiple "lines" are included, separate them
; by newlines.
;
; e.g.: return a string which is basically:
;    name \n Attn: contact \n addr1 \n addr2 \n addr3 \n addr4
;
; But only include the strings that really exist.
;

(define (gnc:owner-get-name-dep owner)
  (cond
   ((eqv? (gncOwnerGetType owner) GNC-OWNER-JOB)
    (gnc:owner-get-name-dep (gncJobGetOwner (gncOwnerGetJob owner))))
   (else (or (gncOwnerGetName owner) ""))))

(define (gnc:owner-get-address-dep owner)
  (define (addif elt)
    (if (and elt (> (string-length elt) 0))
        (list elt)
        '()))
  (let ((addr (gnc:owner-get-address owner)))
    (string-join
     (append
      (addif (gncAddressGetName  addr))
      (addif (gncAddressGetAddr1 addr))
      (addif (gncAddressGetAddr2 addr))
      (addif (gncAddressGetAddr3 addr))
      (addif (gncAddressGetAddr4 addr)))
     "\n")))

(define (gnc:owner-get-name-and-address-dep owner)
  (let ((name (gnc:owner-get-name-dep owner))
	(addr (gnc:owner-get-address-dep owner)))
    (if (> (string-length name) 0)
	(string-append name "\n" addr)
	addr)))

(define (gnc:owner-get-owner-id owner)
  (let ((type (gncOwnerGetType owner)))
    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (let ((c (gncOwnerGetCustomer owner)))
	 (gncCustomerGetID c)))
      ((eqv? type GNC-OWNER-VENDOR)
       (let ((v (gncOwnerGetVendor owner)))
	 (gncVendorGetID v)))
      ((eqv? type GNC-OWNER-EMPLOYEE)
       (let ((e (gncOwnerGetEmployee owner)))
	 (gncEmployeeGetID e)))
      ((eqv? type GNC-OWNER-JOB)
       (gnc:owner-get-owner-id (gncJobGetOwner (gncOwnerGetJob owner))))
      (else ""))))

;; this function aims to find a split's owner. various splits are
;; supported: (1) any splits in the invoice posted transaction, in
;; APAR or income/expense accounts (2) any splits from invoice's
;; payments, in APAR or asset/liability accounts. it returns either
;; the owner or '() if not found. in addition, if owner was found, the
;; result-owner argument is mutated to it.
(define (gnc:owner-from-split split result-owner)
  (define (notnull x) (and (not (null? x)) x))
  (issue-deprecation-warning
   "gnc:owner-from-split is deprecated in 4.x. use gnc:split->owner instead.")
  (let* ((trans (xaccSplitGetParent split))
	 (invoice (notnull (gncInvoiceGetInvoiceFromTxn trans)))
	 (temp (gncOwnerNew))
	 (owner (or (and invoice (gncInvoiceGetOwner invoice))
                    (any
                     (lambda (split)
                       (let* ((lot (xaccSplitGetLot split))
                              (invoice (notnull (gncInvoiceGetInvoiceFromLot lot))))
                         (or (and invoice (gncInvoiceGetOwner invoice))
                             (and (gncOwnerGetOwnerFromLot lot temp) temp))))
                     (xaccTransGetSplitList trans)))))
    (gncOwnerFree temp)
    (cond (owner (gncOwnerCopy (gncOwnerGetEndOwner owner) result-owner)
                 result-owner)
          (else  '()))))


;; optimized from above, and simpler: does not search all transaction
;; splits. It will allocate and memoize (cache) the owners because
;; gncOwnerGetOwnerFromLot is slow. after use, it must be called with
;; #f to free the owners.
(define gnc:split->owner
  (let ((ht (make-hash-table)))
    (lambda (split)
      (cond
       ((not split)
        (hash-for-each (lambda (k v) (gncOwnerFree v)) ht)
        (hash-clear! ht))
       ((hash-ref ht (gncSplitGetGUID split)) => identity)
       (else
        (let ((lot (xaccSplitGetLot split))
              (owner (gncOwnerNew)))
          (unless (gncOwnerGetOwnerFromLot lot owner)
            (gncOwnerCopy (gncOwnerGetEndOwner
                           (gncInvoiceGetOwner
                            (gncInvoiceGetInvoiceFromLot lot)))
                          owner))
          (hash-set! ht (gncSplitGetGUID split) owner)
          owner))))))
