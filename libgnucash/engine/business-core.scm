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

(define-module (gnucash business-core))
(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/engine" 0)

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
  (define (just-name name)
    (if name name ""))

  (let ((type (gncOwnerGetType owner)))
    (cond
      ((eqv? type GNC-OWNER-JOB)
       (gnc:owner-get-name-dep (gncJobGetOwner
				(gncOwnerGetJob owner))))
      (else (just-name (gncOwnerGetName owner))))))

(define (gnc:owner-get-address-dep owner)
  (define (add-if-exists lst new)
    (if (and new (> (string-length new) 0))
	(cons new lst)
	lst))
  (define (build-string lst)
    (cond
     ((null? lst) "")
     ((null? (cdr lst)) (car lst))
     (else (string-append (build-string (cdr lst)) "\n" (car lst)))))
  (let ((lst '())
	(addr (gnc:owner-get-address owner)))
; Added gncAddressGetName  <mikee@saxicola.co.uk>
    (set! lst (add-if-exists lst (gncAddressGetName  addr)))
    (set! lst (add-if-exists lst (gncAddressGetAddr1 addr)))
    (set! lst (add-if-exists lst (gncAddressGetAddr2 addr)))
    (set! lst (add-if-exists lst (gncAddressGetAddr3 addr)))
    (set! lst (add-if-exists lst (gncAddressGetAddr4 addr)))
    (build-string lst)))

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

(define (gnc:entry-type-percent-p type-val)
  (let ((type type-val))
    (equal? type GNC-AMT-TYPE-PERCENT)))

(define (gnc:owner-from-split split result-owner)
  (let* ((trans (xaccSplitGetParent split))
	 (invoice (gncInvoiceGetInvoiceFromTxn trans))
	 (temp-owner (gncOwnerNew))
	 (owner '()))

    (if (not (null? invoice))
	(set! owner (gncInvoiceGetOwner invoice))
	(let ((split-list (xaccTransGetSplitList trans)))
	  (define (check-splits splits)
	    (if (and splits (not (null? splits)))
		(let* ((split (car splits))
		       (lot (xaccSplitGetLot split)))
		  (if (not (null? lot))
		      (let* ((invoice (gncInvoiceGetInvoiceFromLot lot))
			     (owner? (gncOwnerGetOwnerFromLot
				      lot temp-owner)))
			(if (not (null? invoice))
			    (set! owner (gncInvoiceGetOwner invoice))
			    (if owner?
				(set! owner temp-owner)
				(check-splits (cdr splits)))))
		      (check-splits (cdr splits))))))
	  (check-splits split-list)))

    (if (not (null? owner))
	(begin
	  (gncOwnerCopy (gncOwnerGetEndOwner owner) result-owner)
	  (gncOwnerFree temp-owner)
	  result-owner)
	(begin
	  (gncOwnerFree temp-owner)
	  '()))))


(export gnc:owner-get-address)
(export gnc:owner-get-name-dep)
(export gnc:owner-get-address-dep)
(export gnc:owner-get-name-and-address-dep)
(export gnc:owner-get-owner-id)
(export gnc:entry-type-percent-p)
(export gnc:owner-from-split)
