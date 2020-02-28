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
(use-modules (gnucash app-utils))
(use-modules (gnucash report report-system))
(use-modules (gnucash report standard-reports))
(use-modules (srfi srfi-8))

;; to define gnc-build-url
(gnc:module-load "gnucash/html" 0)

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
         (type (gncOwnerGetType end-owner)))
    (gnc-build-url
     URL-TYPE-OWNERREPORT
     (string-append
      (cond ((eqv? type GNC-OWNER-CUSTOMER) "owner=c:")
            ((eqv? type GNC-OWNER-VENDOR) "owner=v:")
            ((eqv? type GNC-OWNER-EMPLOYEE) "owner=e:")
            (else "unknown-type="))
      (gncOwnerReturnGUID end-owner)
      (if (null? acc) "" (string-append "&acct=" (gncAccountGetGUID acc))))
     "")))

;; Creates a new report instance for the given invoice. The given
;; report-template-id must refer to an existing report template, which
;; is then used to instantiate the new report instance.
(define (gnc:invoice-report-create invoice report-template-id)
    (if (gnc:find-report-template report-template-id)
        ;; We found the report template id, so instantiate a report
        ;; and set the invoice option accordingly.
        (let* ((options (gnc:make-report-options report-template-id))
               (invoice-op (gnc:lookup-option options gnc:pagename-general gnc:optname-invoice-number)))

          (gnc:option-set-value invoice-op invoice)
          (gnc:make-report report-template-id options))
        ;; Invalid report-template-id, so let's return zero as an invalid report id.
        0
        ))

(use-modules (gnucash report invoice))
(use-modules (gnucash report taxinvoice))
(use-modules (gnucash report receipt))
(use-modules (gnucash report owner-report))
(use-modules (gnucash report job-report))
(use-modules (gnucash report lot-viewer))
(use-modules (gnucash report new-aging))
(use-modules (gnucash report new-owner-report))
(use-modules (gnucash report payables))
(use-modules (gnucash report receivables))
(use-modules (gnucash report customer-summary))
(use-modules (gnucash report balsheet-eg))

(define (gnc:payables-report-create account title show-zeros?)
  (payables-report-create-internal account title show-zeros?))

(define (gnc:receivables-report-create account title show-zeros?)
  (receivables-report-create-internal account title show-zeros?))

(define* (gnc:owner-report-create owner account #:key currency)
  ; Figure out an account to use if nothing exists here.
  (if (null? account)
      (set! account (find-first-account-for-owner owner #:currency currency)))
  (owner-report-create owner account))

(export gnc:invoice-report-create
	gnc:customer-anchor-text gnc:job-anchor-text gnc:vendor-anchor-text
	gnc:invoice-anchor-text gnc:owner-anchor-text gnc:owner-report-text
	gnc:payables-report-create gnc:receivables-report-create
	gnc:owner-report-create)
