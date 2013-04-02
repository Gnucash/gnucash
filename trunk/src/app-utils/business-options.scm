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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org


;; Internally, values are always a guid. Externally, both guids and
;; invoice pointers may be used to set the value of the option. The
;; option always returns a single invoice pointer.

(use-modules (gnucash main))

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
        (gncInvoiceReturnGUID item)))

  (define (convert-to-invoice item)
    (if (string? item)
        (gncInvoiceLookupFlip item (gnc-get-current-book))
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
     (lambda (invoice) ;; setter
       (if (null? invoice) (set! invoice (default-getter)))
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
     (lambda (f p) (kvp-frame-set-slot-path-gslist f option p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
	 (if (and v (string? v))
	     (begin
	       (set! option v)
	       (set! option-set #t)))))
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
        (gncCustomerReturnGUID item)))

  (define (convert-to-customer item)
    (if (string? item)
        (gncCustomerLookupFlip item (gnc-get-current-book))
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
       (if (null? customer) (set! customer (default-getter)))
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
     (lambda (f p) (kvp-frame-set-slot-path-gslist f option p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
	 (if (and v (string? v))
	     (begin
	       (set! option v)
	       (set! option-set #t)))))
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
        (gncVendorReturnGUID item)))

  (define (convert-to-vendor item)
    (if (string? item)
        (gncVendorLookupFlip item (gnc-get-current-book))
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
       (if (null? vendor) (set! vendor (default-getter)))
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
     (lambda (f p) (kvp-frame-set-slot-path-gslist f option p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
	 (if (and v (string? v))
	     (begin
	       (set! option v)
	       (set! option-set #t)))))
     validator
     #f #f #f #f)))

;; Internally, values are always a guid. Externally, both guids and
;; employee pointers may be used to set the value of the option. The
;; option always returns a single employee pointer.

(define (gnc:make-employee-option
	 section
	 name
	 sort-tag
	 documentation-string
	 default-getter
	 value-validator)

  (define (convert-to-guid item)
    (if (string? item)
        item
        (gncEmployeeReturnGUID item)))

  (define (convert-to-employee item)
    (if (string? item)
        (gncEmployeeLookupFlip item (gnc-get-current-book))
        item))

  (let* ((option (convert-to-guid (default-getter)))
         (option-set #f)
         (getter (lambda () (convert-to-employee
			     (if option-set
				 option
				 (default-getter)))))
         (value->string (lambda ()
                          (string-append
                           "'" (gnc:value->string (if option-set option #f)))))
         (validator
          (if (not value-validator)
              (lambda (employee) (list #t employee))
              (lambda (employee)
                (value-validator (convert-to-employee employee))))))
    (gnc:make-option
     section name sort-tag 'employee documentation-string getter
     (lambda (employee)
       (if (null? employee) (set! employee (default-getter)))
       (set! employee (convert-to-employee employee))
       (let* ((result (validator employee))
	      (valid (car result))
	      (value (cadr result)))
	 (if valid
	     (begin
	       (set! option (convert-to-guid value))
	       (set! option-set #t))
	     (gnc:error "Illegal employee value set"))))
     (lambda () (convert-to-employee (default-getter)))
     (gnc:restore-form-generator value->string)
     (lambda (f p) (kvp-frame-set-slot-path-gslist f option p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
	 (if (and v (string? v))
	     (begin
	       (set! option v)
	       (set! option-set #t)))))
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

  (let ((option-value (gncOwnerNew)))

    (define (convert-to-pair item)
      (if (pair? item)
	  item
	  (cons (gncOwnerGetType item)
		(gncOwnerReturnGUID item))))

    (define (convert-to-owner pair)
      (if (pair? pair)
	  (let ((type (car pair)))
	    (cond
	      ((eqv? type GNC-OWNER-CUSTOMER)
	       (gncOwnerInitCustomer
		option-value
		(gncCustomerLookupFlip (cdr pair) (gnc-get-current-book)))
	       option-value)

	       ((eqv? type GNC-OWNER-VENDOR)
		(gncOwnerInitVendor
		 option-value
		 (gncVendorLookupFlip (cdr pair) (gnc-get-current-book)))
		option-value)

	       ((eqv? type GNC-OWNER-EMPLOYEE)
		(gncOwnerInitEmployee
		 option-value
		 (gncEmployeeLookupFlip (cdr pair) (gnc-get-current-book)))
		option-value)

	       ((eqv? type GNC-OWNER-JOB)
		(gncOwnerInitJob
		 option-value
		 (gncJobLookupFlip (cdr pair) (gnc-get-current-book)))
		option-value)

	       (else '())))
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
		  (let ((type (if (pair? owner)
                                  (car owner)
                                  (gncOwnerGetType owner))))
		    (if (equal? type owner-type)
			(list #t owner)
			(list #f "Owner-Type Mismatch"))))
		(lambda (owner)
		  (value-validator (convert-to-owner owner))))))

      (gnc:make-option
       section name sort-tag 'owner documentation-string getter
       (lambda (owner)
	 (if (null? owner) (set! owner (default-getter)))
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
	 (kvp-frame-set-slot-path-gslist f (symbol->string (car option))
				      (append p '("type")))
	 (kvp-frame-set-slot-path-gslist f (cdr option)
				      (append p '("value"))))
       (lambda (f p)
	 (let ((t (kvp-frame-get-slot-path-gslist f (append p '("type"))))
	       (v (kvp-frame-get-slot-path-gslist f (append p '("value")))))
	   (if (and t v (string? t) (string? v))
	       (begin
		 (set! option (cons (string->symbol t) v))
		 (set! option-set #t)))))
       validator
       owner-type #f #f #f))))


;; Internally, values are always a guid. Externally, both guids and
;; taxtable pointers may be used to set the value of the option. The
;; option always returns a single taxtable pointer.

(define (gnc:make-taxtable-option
	 section
	 name
	 sort-tag
	 documentation-string
	 default-getter
	 value-validator)

  (define (convert-to-guid item)
    (if (string? item)
        item
        (gncTaxTableReturnGUID item)))

  (define (convert-to-taxtable item)
    (if (string? item)
        (gncTaxTableLookupFlip item (gnc-get-current-book))
        item))

  (let* ((option (convert-to-guid (default-getter)))
         (option-set #f)
         (getter (lambda () (convert-to-taxtable
			     (if option-set
				 option
				 (default-getter)))))
         (value->string (lambda ()
                          (string-append
                           "'" (gnc:value->string (if option-set option #f)))))
         (validator
          (if (not value-validator)
              (lambda (taxtable) (list #t taxtable))
              (lambda (taxtable)
                (value-validator (convert-to-taxtable taxtable))))))
    (gnc:make-option
     section name sort-tag 'taxtable documentation-string getter
     (lambda (taxtable)
       (if (null? taxtable) (set! taxtable (default-getter)))
       (set! taxtable (convert-to-taxtable taxtable))
       (let* ((result (validator taxtable))
	      (valid (car result))
	      (value (cadr result)))
	 (if valid
	     (begin
	       (set! option (convert-to-guid value))
	       (set! option-set #t))
	     (gnc:error "Illegal taxtable value set"))))
     (lambda () (convert-to-taxtable (default-getter)))
     (gnc:restore-form-generator value->string)
     (lambda (f p) (kvp-frame-set-slot-path-gslist f option p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
	 (if (and v (string? v))
	     (begin
	       (set! option v)
	       (set! option-set #t)))))
     validator
     #f #f #f #f)))

;; This defines an option to set a counter value. This is a slightly
;; different kind of option: Unlike all other options, the values edited
;; by this option are not saved in the "options"/<section> kvm slot, but
;; in the "counters" slot. This is mostly due to backwards compatibility
;; and partly because counters are a bit different from other options
;; anyway.
;;
;; This is implemented by overriding the scm->kvp and kvp->scm methods
;; to ignore the kvp path passed and replace it with a hardcoded
;; "counters".
(define (gnc:make-counter-option
         section
         name
	 key
         sort-tag
         documentation-string
         default-value)
  (let ((option (gnc:make-number-range-option section name sort-tag documentation-string default-value 0 999999999 0 1)))
     (gnc:set-option-scm->kvp option (lambda (f p) (kvp-frame-set-slot-path-gslist f (inexact->exact ((gnc:option-getter option))) (list "counters" key))))
     (gnc:set-option-kvp->scm option (lambda (f p)
                               (let ((v (kvp-frame-get-slot-path-gslist f (list "counters" key))))
                                 (if (and v (integer? v))
                                     ((gnc:option-setter option) v)))))
     option))

;; This defines an option to set a counter format, which has the same
;; exception as gnc:make-counter-option above.
(define (gnc:make-counter-format-option
         section
         name
	 key
         sort-tag
         documentation-string
         default-value)
  (let ((option (gnc:make-string-option section name sort-tag documentation-string default-value)))
     (gnc:set-option-scm->kvp option (lambda (f p) (kvp-frame-set-slot-path-gslist f ((gnc:option-getter option)) (list "counter_formats" key))))
     (gnc:set-option-kvp->scm option (lambda (f p)
                               (let ((v (kvp-frame-get-slot-path-gslist f (list "counter_formats" key))))
                                 (if (and v (string? v))
                                     ((gnc:option-setter option) v)))))
     option))

(export gnc:make-invoice-option)
(export gnc:make-customer-option)
(export gnc:make-vendor-option)
(export gnc:make-employee-option)
(export gnc:make-owner-option)
(export gnc:make-taxtable-option)
(export gnc:make-counter-option)
(export gnc:make-counter-format-option)
