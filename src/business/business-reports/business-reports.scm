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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report business-reports))
(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/standard-reports" 0)
(gnc:module-load "gnucash/business-utils" 0)

;; this defines gnc:url-type-ownerreport and pulls in gnome-utils
;; to define gnc:html-build-url
(gnc:module-load "gnucash/business-gnome" 0)

(define gnc:menuname-business-reports (N_ "Business Reports"))

(define (guid-ref idstr type guid)
  (gnc:html-build-url type (string-append idstr guid) #f))

(define (gnc:customer-anchor-text customer)
  (guid-ref "customer=" gnc:url-type-customer (gnc:customer-get-guid customer)))

(define (gnc:job-anchor-text job)
  (guid-ref "job=" gnc:url-type-job (gnc:job-get-guid job)))

(define (gnc:vendor-anchor-text vendor)
  (guid-ref "vendor=" gnc:url-type-vendor (gnc:vendor-get-guid vendor)))

(define (gnc:employee-anchor-text employee)
  (guid-ref "employee=" gnc:url-type-employee (gnc:employee-get-guid employee)))

(define (gnc:invoice-anchor-text invoice)
  (guid-ref "invoice=" gnc:url-type-invoice (gnc:invoice-get-guid invoice)))

(define (gnc:owner-anchor-text owner)
  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
	       (gnc:owner-get-type (gnc:owner-get-end-owner owner)) #f)))
    (case type
      ((gnc-owner-customer)
       (gnc:customer-anchor-text (gnc:owner-get-customer owner)))

      ((gnc-owner-vendor)
       (gnc:vendor-anchor-text (gnc:owner-get-vendor owner)))

      ((gnc-owner-employee)
       (gnc:employee-anchor-text (gnc:owner-get-employee owner)))

      ((gnc-owner-job)
       (gnc:job-anchor-text (gnc:owner-get-job owner)))

      (else
       ""))))

(define (gnc:owner-report-text owner acc)
  (let* ((end-owner (gnc:owner-get-end-owner owner))
	 (type (gw:enum-<gnc:GncOwnerType>-val->sym
	       (gnc:owner-get-type end-owner) #f))
	 (ref #f))

    (case type
      ((gnc-owner-customer)
       (set! ref "owner=c:"))

      ((gnc-owner-vendor)
       (set! ref "owner=v:"))

      ((gnc-owner-employee)
       (set! ref "owner=e:"))

      (else (set! ref "unknown-type=")))

    (if ref
	(begin
	  (set! ref (string-append ref (gnc:owner-get-guid end-owner)))
	  (if acc
	      (set! ref (string-append ref "&acct="
				       (gnc:account-get-guid acc))))
	  (gnc:html-build-url gnc:url-type-ownerreport ref #f))
	ref)))

(export gnc:menuname-business-reports)

(use-modules (gnucash report fancy-invoice))
(use-modules (gnucash report invoice))
(use-modules (gnucash report easy-invoice))
(use-modules (gnucash report owner-report))
(use-modules (gnucash report payables))
(use-modules (gnucash report receivables))

(define gnc:invoice-report-create gnc:invoice-report-create-internal)

(export gnc:invoice-report-create gnc:owner-report-create
	gnc:customer-anchor-text gnc:job-anchor-text gnc:vendor-anchor-text
	gnc:invoice-anchor-text gnc:owner-anchor-text gnc:owner-report-text)
