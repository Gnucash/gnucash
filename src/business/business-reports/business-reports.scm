;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  business-reports.scm
;;  load the business report definitions
;;
;;  Copyright (c) 2002 Derek Atkins <derek@ihtfp.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report business-reports))
(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/report/standard-reports" 0)
(gnc:module-load "gnucash/business-gnome" 0)

(define gnc:menuname-business-reports (N_ "Business Reports"))

(define (guid-ref type guid)
  (gnc:html-build-url type (string-append "guid=" guid) #f))

(define (gnc:customer-anchor-text customer)
  (guid-ref gnc:url-type-customer (gnc:customer-get-guid customer)))

(define (gnc:job-anchor-text job)
  (guid-ref gnc:url-type-job (gnc:job-get-guid job)))

(define (gnc:vendor-anchor-text vendor)
  (guid-ref gnc:url-type-vendor (gnc:vendor-get-guid vendor)))

(define (gnc:invoice-anchor-text invoice)
  (guid-ref gnc:url-type-invoice (gnc:invoice-get-guid invoice)))

(define (gnc:owner-anchor-text owner)
  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
	       (gnc:owner-get-type (gnc:owner-get-end-owner owner)) #f)))
    (case type
      ((gnc-owner-customer)
       (gnc:customer-anchor-text (gnc:owner-get-customer owner)))

      ((gnc-owner-vendor)
       (gnc:vendor-anchor-text (gnc:owner-get-vendor owner)))

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
       (set! ref "owner=v:")))

    (if ref
	(begin
	  (set! ref (string-append ref (gnc:owner-get-guid end-owner)))
	  (if acc
	      (set! ref (string-append ref "&acct="
				       (gnc:account-get-guid acc))))
	  (gnc:html-build-url gnc:url-type-ownerreport ref #f))
	ref)))

(export gnc:menuname-business-reports)

(use-modules (gnucash report invoice))
(use-modules (gnucash report owner-report))
(use-modules (gnucash report payables))
(use-modules (gnucash report receivables))

(define gnc:invoice-report-create gnc:invoice-report-create-internal)

(export gnc:invoice-report-create gnc:owner-report-create
	gnc:customer-anchor-text gnc:job-anchor-text gnc:vendor-anchor-text
	gnc:invoice-anchor-text gnc:owner-anchor-text gnc:owner-report-text)
