(define-module (gnucash business-core))
(use-modules (g-wrapped gw-business-core))
(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/engine" 0)

; return a string which is basically:
;    name \n Attn: contact \n addr1 \n addr2 \n addr3 \n addr4
;
; But only include the strings that really exist.
;
(define (name-and-addr name addr)

  (define (add-if-exists lst new)
    (if (and new (> (string-length new) 0))
	(cons new lst)
	lst))

  (define (build-string lst)
    (cond
     ((null? lst) "")
     ((null? (cdr lst)) (car lst))
     (else (string-append (build-string (cdr lst)) "\n" (car lst)))))

  (let ((lst '()))

    (set! lst (add-if-exists lst name))
    (set! lst (add-if-exists lst (gnc:address-get-name addr)))
    (set! lst (add-if-exists lst (gnc:address-get-addr1 addr)))
    (set! lst (add-if-exists lst (gnc:address-get-addr2 addr)))
    (set! lst (add-if-exists lst (gnc:address-get-addr3 addr)))
    (set! lst (add-if-exists lst (gnc:address-get-addr4 addr)))

    (build-string lst)))

(define (gnc:owner-get-name owner)
  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
	       (gnc:owner-get-type owner) #f)))
    (case type
      ((gnc-owner-customer)
       (let ((c (gnc:owner-get-customer owner)))
	 (gnc:customer-get-name c)))
      ((gnc-owner-vendor)
       (let ((v (gnc:owner-get-vendor owner)))
	  (gnc:vendor-get-name v)))
      ((gnc-owner-job)
       (gnc:owner-get-name (gnc:job-get-owner
			    (gnc:owner-get-job owner))))
      (else ""))))

(define (gnc:owner-get-address owner)
  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
	       (gnc:owner-get-type owner) #f)))
    (case type
      ((gnc-owner-customer)
       (let ((c (gnc:owner-get-customer owner)))
	 (gnc:customer-get-addr c)))
      ((gnc-owner-vendor)
       (let ((v (gnc:owner-get-vendor owner)))
	 (gnc:vendor-get-addr v)))
      ((gnc-owner-job)
       (gnc:owner-get-address (gnc:job-get-owner
			       (gnc:owner-get-job owner))))
      (else ""))))

(define (gnc:owner-get-address-dep owner)
  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
	       (gnc:owner-get-type owner) #f)))
    (case type
      ((gnc-owner-customer)
       (let ((c (gnc:owner-get-customer owner)))
	 (name-and-addr
	  (gnc:customer-get-name c)
	  (gnc:customer-get-addr c))))
      ((gnc-owner-vendor)
       (let ((v (gnc:owner-get-vendor owner)))
	 (name-and-addr
	  (gnc:vendor-get-name v)
	  (gnc:vendor-get-addr v))))
      ((gnc-owner-job)
       (gnc:owner-get-address-dep (gnc:job-get-owner
				   (gnc:owner-get-job owner))))
      (else ""))))

(define (gnc:owner-get-owner-id owner)
  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
	       (gnc:owner-get-type owner) #f)))
    (case type
      ((gnc-owner-customer)
       (let ((c (gnc:owner-get-customer owner)))
	 (gnc:customer-get-id)))
      ((gnc-owner-vendor)
       (let ((v (gnc:owner-get-vendor owner)))
	 (gnc:vendor-get-id)))
      ((gnc-owner-job)
       (gnc:owner-get-id (gnc:job-get-owner (gnc:owner-get-job owner))))
      (else ""))))

(define (gnc:entry-type-percent-p type-val)
  (let ((type (gw:enum-<gnc:GncAmountType>-val->sym type #f)))
    (equal? type 'gnc-amount-type-percent)))

(define (gnc:owner-from-split split result-owner)
  (let* ((trans (gnc:split-get-parent split))
	 (invoice (gnc:invoice-get-invoice-from-txn trans))
	 (temp-owner (gnc:owner-create))
	 (owner #f))

    (if invoice
	(set! owner (gnc:invoice-get-owner invoice))
	(let ((split-list (gnc:transaction-get-splits trans)))
	  (define (check-splits splits)
	    (let* ((split (car splits))
		   (lot (gnc:split-get-lot split)))
	      (if lot
		  (let* ((invoice (gnc:invoice-get-invoice-from-lot lot))
			 (owner? (gnc:owner-get-owner-from-lot
				  lot temp-owner)))
		    (if invoice
			(set! owner (gnc:invoice-get-owner invoice))
			(if owner?
			    (set! owner temp-owner)
			    (check-splits (cdr splits)))))
		  (check-splits (cdr splits)))))
	  (check-splits split-list)))

    (gnc:owner-copy-into-owner (gnc:owner-get-end-owner owner) result-owner)
    (gnc:owner-destroy temp-owner)
    result-owner))


(export gnc:owner-get-name)
(export gnc:owner-get-address)
(export gnc:owner-get-address-dep)
(export gnc:owner-get-owner-id)
(export gnc:entry-type-percent-p)
(export gnc:owner-from-split)
