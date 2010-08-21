;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  business-reports.scm
;;  load the business report definitions
;;
;;  Copyright (c) 2002 Derek Atkins <derek@ihtfp.com>
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report business-reports))
(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/standard-reports" 0)
(gnc:module-load "gnucash/business-utils" 0)

;; to define gnc-build-url
(gnc:module-load "gnucash/html" 0)
;; this defines URL-TYPE-OWNERREPORT
(gnc:module-load "gnucash/business-gnome" 0)

(define gnc:menuname-business-reports (N_ "_Business"))

(define (guid-ref idstr type guid)
  (gnc-build-url type (string-append idstr guid) ""))

(define (gnc:customer-anchor-text customer)
  (guid-ref "customer=" URL-TYPE-CUSTOMER (gncCustomerReturnGUID customer)))

(define (gnc:job-anchor-text job)
  (guid-ref "job=" URL-TYPE-JOB (gncJobReturnGUID job)))

(define (gnc:vendor-anchor-text vendor)
  (guid-ref "vendor=" URL-TYPE-VENDOR (gncVendorReturnGUID vendor)))

(define (gnc:employee-anchor-text employee)
  (guid-ref "employee=" URL-TYPE-EMPLOYEE (gncEmployeeReturnGUID employee)))

(define (gnc:invoice-anchor-text invoice)
  (guid-ref "invoice=" URL-TYPE-INVOICE (gncInvoiceReturnGUID invoice)))

(define (gnc:owner-anchor-text owner)
  (let ((type (gncOwnerGetType (gncOwnerGetEndOwner owner))))
    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (gnc:customer-anchor-text (gncOwnerGetCustomer owner)))

      ((eqv? type GNC-OWNER-VENDOR)
       (gnc:vendor-anchor-text (gncOwnerGetVendor owner)))

      ((eqv? type GNC-OWNER-EMPLOYEE)
       (gnc:employee-anchor-text (gncOwnerGetEmployee owner)))

      ((eqv? type GNC-OWNER-JOB)
       (gnc:job-anchor-text (gncOwnerGetJob owner)))

      (else
       ""))))

(define (gnc:owner-report-text owner acc)
  (let* ((end-owner (gncOwnerGetEndOwner owner))
	 (type (gncOwnerGetType end-owner))
	 (ref #f))

    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (set! ref "owner=c:"))

      ((eqv? type GNC-OWNER-VENDOR)
       (set! ref "owner=v:"))

      ((eqv? type GNC-OWNER-EMPLOYEE)
       (set! ref "owner=e:"))

      (else (set! ref "unknown-type=")))

    (if ref
	(begin
	  (set! ref (string-append ref (gncOwnerReturnGUID end-owner)))
	  (if (not (null? acc))
	      (set! ref (string-append ref "&acct="
				       (gncAccountGetGUID acc))))
	  (gnc-build-url URL-TYPE-OWNERREPORT ref ""))
	ref)))

(export gnc:menuname-business-reports)

(use-modules (gnucash report fancy-invoice))
(use-modules (gnucash report invoice))
(use-modules (gnucash report easy-invoice))
(use-modules (gnucash report taxinvoice))
(use-modules (gnucash report owner-report))
(use-modules (gnucash report job-report))
(use-modules (gnucash report payables))
(use-modules (gnucash report receivables))

(define gnc:invoice-report-create gnc:invoice-report-create-internal)

(export gnc:invoice-report-create
	gnc:customer-anchor-text gnc:job-anchor-text gnc:vendor-anchor-text
	gnc:invoice-anchor-text gnc:owner-anchor-text gnc:owner-report-text)
(re-export gnc:owner-report-create)
