;; Scheme code for supporting options for the business modules
;;
;; Created by:	Derek Atkins <derek@ihtfp.com>
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


;; Internally, values are always a guid. Externally, both guids and
;; invoice pointers may be used to set the value of the option. The
;; option always returns a single invoice pointer.

(define (gnc:make-invoice-option
	 section
	 name
	 sort-tag
	 documentation-string
	 default-getter
	 value-validator)

  (define (convert-to-guid item)
    (if (string? item)
        item
        (gnc:invoice-get-guid item)))

  (define (convert-to-invoice item)
    (if (string? item)
        (gnc:invoice-lookup item (gnc:get-current-book))
        item))

  (let* ((option (convert-to-guid (default-getter)))
         (option-set #f)
         (getter (lambda () (convert-to-invoice
			     (if option-set
				 option
				 (default-getter)))))
         (value->string (lambda ()
                          (string-append
                           "'" (gnc:value->string (if option-set option #f)))))
         (validator
          (if (not value-validator)
              (lambda (invoice) (list #t invoice))
              (lambda (invoice)
                (value-validator (convert-to-invoice invoice))))))
    (gnc:make-option
     section name sort-tag 'invoice documentation-string getter
     (lambda (invoice)
       (if (not invoice) (set! invoice (default-getter)))
       (set! invoice (convert-to-invoice invoice))
       (let* ((result (validator invoice))
	      (valid (car result))
	      (value (cadr result)))
	 (if valid
	     (begin
	       (set! option (convert-to-guid value))
	       (set! option-set #t))
	     (gnc:error "Illegal invoice value set"))))
     (lambda () (convert-to-invoice (default-getter)))
     (gnc:restore-form-generator value->string)
     (lambda (f p) (gnc:kvp-frame-set-slot-path f value p))
     (lambda (f p)
       (let ((v (gnc:kvp-frame-get-slot-path f p)))
	 (if (and v (string? v))
	     (set! value v))))
     validator
     #f #f #f #f)))

;; Internally, values are always a guid. Externally, both guids and
;; customer pointers may be used to set the value of the option. The
;; option always returns a single customer pointer.

(define (gnc:make-customer-option
	 section
	 name
	 sort-tag
	 documentation-string
	 default-getter
	 value-validator)

  (define (convert-to-guid item)
    (if (string? item)
        item
        (gnc:customer-get-guid item)))

  (define (convert-to-customer item)
    (if (string? item)
        (gnc:customer-lookup item (gnc:get-current-book))
        item))

  (let* ((option (convert-to-guid (default-getter)))
         (option-set #f)
         (getter (lambda () (convert-to-customer
			     (if option-set
				 option
				 (default-getter)))))
         (value->string (lambda ()
                          (string-append
                           "'" (gnc:value->string (if option-set option #f)))))
         (validator
          (if (not value-validator)
              (lambda (customer) (list #t customer))
              (lambda (customer)
                (value-validator (convert-to-customer customer))))))
    (gnc:make-option
     section name sort-tag 'customer documentation-string getter
     (lambda (customer)
       (if (not customer) (set! customer (default-getter)))
       (set! customer (convert-to-customer customer))
       (let* ((result (validator customer))
	      (valid (car result))
	      (value (cadr result)))
	 (if valid
	     (begin
	       (set! option (convert-to-guid value))
	       (set! option-set #t))
	     (gnc:error "Illegal customer value set"))))
     (lambda () (convert-to-customer (default-getter)))
     (gnc:restore-form-generator value->string)
     (lambda (f p) (gnc:kvp-frame-set-slot-path f value p))
     (lambda (f p)
       (let ((v (gnc:kvp-frame-get-slot-path f p)))
	 (if (and v (string? v))
	     (set! value v))))
     validator
     #f #f #f #f)))

;; Internally, values are always a guid. Externally, both guids and
;; vendor pointers may be used to set the value of the option. The
;; option always returns a single vendor pointer.

(define (gnc:make-vendor-option
	 section
	 name
	 sort-tag
	 documentation-string
	 default-getter
	 value-validator)

  (define (convert-to-guid item)
    (if (string? item)
        item
        (gnc:vendor-get-guid item)))

  (define (convert-to-vendor item)
    (if (string? item)
        (gnc:vendor-lookup item (gnc:get-current-book))
        item))

  (let* ((option (convert-to-guid (default-getter)))
         (option-set #f)
         (getter (lambda () (convert-to-vendor
			     (if option-set
				 option
				 (default-getter)))))
         (value->string (lambda ()
                          (string-append
                           "'" (gnc:value->string (if option-set option #f)))))
         (validator
          (if (not value-validator)
              (lambda (vendor) (list #t vendor))
              (lambda (vendor)
                (value-validator (convert-to-vendor vendor))))))
    (gnc:make-option
     section name sort-tag 'vendor documentation-string getter
     (lambda (vendor)
       (if (not vendor) (set! vendor (default-getter)))
       (set! vendor (convert-to-vendor vendor))
       (let* ((result (validator vendor))
	      (valid (car result))
	      (value (cadr result)))
	 (if valid
	     (begin
	       (set! option (convert-to-guid value))
	       (set! option-set #t))
	     (gnc:error "Illegal vendor value set"))))
     (lambda () (convert-to-vendor (default-getter)))
     (gnc:restore-form-generator value->string)
     (lambda (f p) (gnc:kvp-frame-set-slot-path f value p))
     (lambda (f p)
       (let ((v (gnc:kvp-frame-get-slot-path f p)))
	 (if (and v (string? v))
	     (set! value v))))
     validator
     #f #f #f #f)))

;; Internally, values are always a type/guid pair. Externally, both
;; type/guid pairs and owner pointers may be used to set the value of
;; the option. The option always returns a single owner pointer.

(define (gnc:make-owner-option
	 section
	 name
	 sort-tag
	 documentation-string
	 default-getter
	 value-validator
	 owner-type)

  (let ((option-value (gnc:owner-create)))

    (define (convert-to-pair item)
      (if (pair? item)
	  item
	  (cons (gw:enum-<gnc:GncOwnerType>-val->sym
		 (gnc:owner-get-type item) #f)
		(gnc:owner-get-guid item))))

    (define (convert-to-owner pair)
      (if (pair? pair)
	  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym (car pair) #f)))
	    (case type
	      ((gnc-owner-customer)
	       (gnc:owner-init-customer
		option-value
		(gnc:customer-lookup (cdr pair) (gnc:get-current-book)))
	       option-value)

	       ((gnc-owner-vendor)
		(gnc:owner-init-vendor
		 option-value
		 (gnc:vendor-lookup (cdr pair) (gnc:get-current-book)))
		option-value)

	       ((gnc-owner-job)
		(gnc:owner-init-job
		 option-value
		 (gnc:job-lookup (cdr pair) (gnc:get-current-book)))
		option-value)

	       (else #f)))
	  pair))

    (let* ((option (convert-to-pair (default-getter)))
	   (option-set #f)
	   (getter (lambda () (convert-to-owner
			       (if option-set
				   option
				   (default-getter)))))
	   (value->string (lambda ()
			    (string-append
			     "'" (gnc:value->string
				  (if option-set option #f)))))
	   (validator
	    (if (not value-validator)
		(lambda (owner)
		  (let ((type (gw:enum-<gnc:GncOwnerType>-val->sym
			       (if (pair? owner)
				   (car owner)
				   (gnc:owner-get-type owner)) #f)))
		    (if (equal? type owner-type)
			(list #t owner)
			(list #f "Owner-Type Mismatch"))))
		(lambda (owner)
		  (value-validator (convert-to-owner owner))))))

      (gnc:make-option
       section name sort-tag 'owner documentation-string getter
       (lambda (owner)
	 (if (not owner) (set! owner (default-getter)))
	 (set! owner (convert-to-owner owner))
	 (let* ((result (validator owner))
		(valid (car result))
		(value (cadr result)))
	   (if valid
	       (begin
		 (set! option (convert-to-pair value))
		 (set! option-set #t))
	       (gnc:error "Illegal owner value set"))))
       (lambda () (convert-to-owner (default-getter)))
       (gnc:restore-form-generator value->string)
       (lambda (f p)
	 (gnc:kvp-frame-set-slot-path f (symbol->string (car value))
				      (append p '("type")))
	 (gnc:kvp-frame-set-slot-path f (cdr value)
				      (append p '("value"))))
       (lambda (f p)
	 (let ((t (gnc:kvp-frame-get-slot-path f (append p '("type"))))
	       (v (gnc:kvp-frame-get-slot-path f (append p '("value")))))
	   (if (and t v (string? t) (string? v))
	       (set! value (cons (string->symbol t) v)))))
       validator
       owner-type #f #f #f))))

(export gnc:make-invoice-option)
(export gnc:make-customer-option)
(export gnc:make-vendor-option)
(export gnc:make-owner-option)
