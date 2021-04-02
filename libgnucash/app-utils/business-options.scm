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

(define-module (gnucash app-utils business-options))

(eval-when (compile load eval expand)
  (load-extension "libgnc-app-utils" "scm_init_sw_app_utils_module"))

(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash app-utils options))
(use-modules (sw_app_utils))

(export gnc:*business-label*)
(export gnc:*company-name*)
(export gnc:*company-addy*)
(export gnc:*company-id*)
(export gnc:*company-phone*)
(export gnc:*company-fax*)
(export gnc:*company-url*)
(export gnc:*company-email*)
(export gnc:*company-contact*)
(export gnc:*fancy-date-label*)
(export gnc:*fancy-date-format*)
(export gnc:*tax-label*)
(export gnc:*tax-nr-label*)
(export gnc:company-info)
(export gnc:fancy-date-info)
(export gnc:*option-section-budgeting*)
(export gnc:*option-name-auto-readonly-days*)
(export gnc:*option-name-num-field-source*)
(export gnc:*kvp-option-path*)
(export gnc:options-fancy-date)
(export gnc:*option-name-default-budget*)

(define gnc:*kvp-option-path* (list KVP-OPTION-PATH))
(define gnc:*option-name-auto-readonly-days* OPTION-NAME-AUTO-READONLY-DAYS)
(define gnc:*option-name-num-field-source* OPTION-NAME-NUM-FIELD-SOURCE)

(define gnc:*option-section-budgeting* OPTION-SECTION-BUDGETING)
(define gnc:*option-name-default-budget* OPTION-NAME-DEFAULT-BUDGET)

(define gnc:*business-label* (N_ "Business"))
(define gnc:*company-name* (N_ "Company Name"))
(define gnc:*company-addy* (N_ "Company Address"))
(define gnc:*company-id* (N_ "Company ID"))
(define gnc:*company-phone* (N_ "Company Phone Number"))
(define gnc:*company-fax* (N_ "Company Fax Number"))
(define gnc:*company-url* (N_ "Company Website URL"))
(define gnc:*company-email* (N_ "Company Email Address"))
(define gnc:*company-contact* (N_ "Company Contact Person"))
(define gnc:*fancy-date-label* (N_ "Fancy Date Format"))
(define gnc:*fancy-date-format* (N_ "custom"))
(define gnc:*tax-label* (N_ "Tax"))
(define gnc:*tax-nr-label* (N_ "Tax Number"))


(define (gnc:options-fancy-date book)
  (let ((date-format (gnc:fancy-date-info book gnc:*fancy-date-format*)))
    (if (boolean? date-format) ;; date-format does not exist
        (qof-date-format-get-string (qof-date-format-get))
       date-format)))

(define (gnc:company-info book key)
  ;; Access company info from key-value pairs for current book
 (gnc:option-get-value book gnc:*business-label* key))

(define (gnc:fancy-date-info book key)
  ;; Access fancy date info from key-value pairs for current book
 (gnc:option-get-value book gnc:*business-label* (list gnc:*fancy-date-label* key)))

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
     (lambda (b p) (qof-book-set-option b option p))
     (lambda (b p)
       (let ((v (qof-book-get-option b p)))
	 (if (and v (string? v))
	     (begin
	       (set! option v)
	       (set! option-set #t)))))
     validator
     #f #f #f #f)))

;; Internally, values are always a guid. Externally, both guids and
;; customer pointers may be used to set the value of the option. The
;; option always returns a single customer pointer.
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
       (lambda (b p)
	 (qof-book-set-option b (symbol->string (car option))
				      (append p '("type")))
	 (qof-book-set-option b (cdr option)
				      (append p '("value"))))
       (lambda (b p)
	 (let ((t (qof-book-get-option b (append p '("type"))))
	       (v (qof-book-get-option b (append p '("value")))))
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
     (lambda (b p) (qof-book-set-option b option p))
     (lambda (b p)
       (let ((v (qof-book-get-option b p)))
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
  (let ((option (gnc:make-number-range-option
                 section name sort-tag documentation-string
                 default-value 0 999999999 0 1)))
    (gnc:set-option-scm->kvp
     option
     (lambda (b p)
       (qof-book-set-option
        b (inexact->exact ((gnc:option-getter option)))
        (list "counters" key))))
    (gnc:set-option-kvp->scm
     option
     (lambda (b p)
       (let ((v (qof-book-get-option b (list "counters" key))))
         (if (and v (integer? v))
             ((gnc:option-setter option) v)))))
    option))

;; This defines an option to set a counter format, which has the same
;; exception as gnc:make-counter-option above.
;; Note this function uses a hack to make sure there never is a default value
;; (default-value is set to #f and value subsequently set to whatever was passed as default-value)
;; This hack was introduced to fix https://bugs.gnucash.org/show_bug.cgi?id=687504
(define (gnc:make-counter-format-option
         section
         name
         key
         sort-tag
         documentation-string
         default-value)
  (let ((option (gnc:make-string-option
                 section name sort-tag documentation-string #f)))
    (gnc:option-set-value option default-value)
    (gnc:set-option-scm->kvp
     option
     (lambda (b p)
       (let ((value ((gnc:option-getter option)))
             (path (string-concatenate (list "counter_formats/" key))))
         (qof-book-set-string-option b path value))))
    (gnc:set-option-kvp->scm
     option
     (lambda (b p)
       (let* ((path (string-concatenate (list "counter_formats/" key)))
              (v (qof-book-get-string-option b path)))
         (if (and v (string? v))
             ((gnc:option-setter option) v)))))
    option))

(export gnc:make-invoice-option)
(export gnc:make-owner-option)
(export gnc:make-taxtable-option)
(export gnc:make-counter-option)
(export gnc:make-counter-format-option)
