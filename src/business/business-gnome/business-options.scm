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
     validator
     #f #f #f #f)))

(export gnc:make-invoice-option)
(export gnc:make-customer-option)
(export gnc:make-vendor-option)
