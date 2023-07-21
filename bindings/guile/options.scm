;; Scheme code for supporting options
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

(define-module (gnucash options))

(eval-when (compile load eval expand)
  (load-extension "libgnucash-guile" "scm_init_sw_app_utils_module"))

(use-modules (gnucash core-utils))
(use-modules (sw_engine))
(use-modules (sw_app_utils))
(use-modules (gnucash utilities))
(use-modules (srfi srfi-1))
(use-modules (ice-9 regex))
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

;; Conditionally extract the GncOptionDBPtr& from a passed in options:
; If it's a procedure then it's the object returned by gnc:new-options;
; otherwise it is assumed to be a GncOptionDBPtr&.
(define-public (gnc:optiondb options)
  (if (procedure? options) (options 'get) options))

(define-public (gnc:lookup-option options section name)
  (if options
      (gnc-lookup-option (gnc:optiondb options) section name)
      #f))

(define-public (gnc:option-setter option)
  (issue-deprecation-warning "gnc:option-setter is deprecated. Option values are set and retrieved by gnc-set-option and gnc-optiondb-lookup-value.")
  (lambda (value)
    (GncOption-set-value option value)
    ))

(define-public (gnc:option-set-value option value)
    (issue-deprecation-warning "gnc:option-set-value and indeed all direct option access is deprecated. Use gnc-set-option instead.")
    (GncOption-set-value option value))

(define-public (gnc:option-set-default-value option value)
    (issue-deprecation-warning "gnc:option-set-default-value and indeed all direct option access is deprecated. Use gnc-set-option instead.")
    (GncOption-set-default-value option value))

(define-public (gnc:option-section option)
  (GncOption-get-section option))

(define-public (gnc:option-name option)
  (GncOption-get-name option))

(define-public (gnc:option-default-value option)
  (GncOption-get-default-value option))

(define-public (gnc:option-value option)
    (issue-deprecation-warning "gnc:option-value and indeed all direct option access is deprecated. Use gnc-optiondb-lookup-value instead.")
    (GncOption-get-value option))

(define-public (gnc:color->html color)
  ;; HTML doesn't like alpha values.
  (let ((html-color (if (> (string-length color) 6)
                       (substring color 0 6)
                       color)))
    (format #f "#~a" html-color)))

(define-public (gnc:color-option->html opt)
  (gnc:color->html (GncOption-get-value opt)))

(define-public (gnc:color-option->hex-string opt)
  (format #f "~a" (GncOption-get-value opt)))

(define-public (gnc:book-get-option-value book category key)
  (define acc (if (pair? key) cons list))
  (qof-book-get-option book (acc category key)))

(define-public (gnc:option-make-internal! options section name)
  (let ((option (gnc-lookup-option (gnc:optiondb options) section name)))
    (and option (GncOption-make-internal option))))

(define-public (gnc:option-type option)
  (GncOption-get-type option))

;; Create the database and return a dispatch function.
(define-public (gnc:new-options)
  (let ((optiondb (gnc-new-optiondb)))
    (define (dispatch key)
      optiondb)
    dispatch))

(define-public (gnc:options-set-default-section options section)
  (GncOptionDBPtr-set-default-section (gnc:optiondb options) section))

(define-public (gnc:options-for-each func optdb)
  (gnc-optiondb-foreach (gnc:optiondb optdb) func))

;; Copies all values from src-options to dest-options, that is, it
;; copies the values of all options from src which exist in dest to
;; there.
(define-public (gnc:options-copy-values src-options dest-options)
  (if 
   dest-options
   (gnc:options-for-each 
    (lambda (src-option) 
      (let ((dest-option (gnc-lookup-option (gnc:optiondb dest-options)
                                            (gnc:option-section src-option)
                                            (gnc:option-name src-option))))
        (if dest-option
            (GncOption-set-value dest-option 
                                 (GncOption-get-value src-option)))))
    src-options)))

;; Get scheme commands to set changed options, used to write a file that will
;; restore a customized report or stylesheet.
(define-public (gnc:value->string value)
  (format #f "~s" value))

(define-public (gnc:generate-restore-forms options toplevel-name)
  (define (section-op section-name)
    (display
     (string-append "\n; Section: " section-name "\n\n")))

  (define (gnc:option-is-budget? option)
    (GncOption-is-budget-option option))

  (define (option-op option)
    (let ((value (GncOption-get-value option))
          (default-value (GncOption-get-default-value option)))
      (if (not (equal? value default-value))
          (display (string-append
                      "(let ((option (gnc:lookup-option " toplevel-name "\n"
                      "                                 "
                      (gnc:value->string (gnc:option-section option)) "\n"
                      "                                 "
                      (gnc:value->string (gnc:option-name option)) ")))\n"
                      "  ("
                      (cond
                       ((gnc:option-is-budget? option)
                       (let* ((budget (GncOption-get-value option))
                              (guid (gncBudgetGetGUID budget))
                              (guid-string (gnc:value->string guid)))
                              (if (string? guid-string)
                                  (string-append
                                   "(lambda (option) "
                                   "(if option ((gnc:option-setter option) "
                                   "(gnc-budget-lookup " guid-string
                                   " (gnc-get-current-book)))))"
                                   )
                                  ("Failed to get GUID for budget option."))))
                       (else
                        (string-append
                         "(lambda (o) (if o (gnc:option-set-value o "
                         (GncOption-save-scm-value option) ")))"
                        )))
                      " option))\n\n")))))

  (define (generate-forms)
      (gnc-optiondb-foreach2 (gnc:optiondb options) section-op option-op))

  (with-output-to-string generate-forms))


;; The following implement the old API that separated creation from registration.
(define-public (gnc:register-option optdb opt)
  (issue-deprecation-warning "gnc:register-option is deprecated. Use gnc-register-foo-option instead.")
  (GncOptionDBPtr-register-option (gnc:optiondb optdb)
                                  (GncOption-get-section opt) opt))

(define-public (gnc:unregister-option optdb section name)
  (GncOptionDBPtr-unregister-option (gnc:optiondb optdb) section name))

(define-public (gnc:make-string-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-string-option is deprecated. Make and register the option in one command with gnc-register-string-option.")
  (gnc-make-string-option section name key docstring default (GncOptionUIType-STRING)))
(define-public (gnc:make-text-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-text-option is deprecated. Make and register the option in one command with gnc-register-text-option.")
  (gnc-make-string-option section name key docstring default (GncOptionUIType-TEXT)))
(define-public (gnc:make-font-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-font-option is deprecated. Make and register the option in one command with gnc-register-font-option.")
  (gnc-make-string-option section name key docstring default (GncOptionUIType-FONT)))
(define-public (gnc:make-color-option section name key docstring colors range use-alpha)
  (issue-deprecation-warning "gnc:make-color-option is deprecated. Make and register the option in one command with gnc-register-color-option.")
  (let ((color-str (if use-alpha ;; It's always false...
                      (format #f "~x~x~x~x" (car colors) (cadr colors) (caddr colors) (cadddr colors))
                      (format #f "~x~x~x" (car colors) (cadr colors) (caddr colors)))))
  (gnc-make-string-option section name key docstring color-str (GncOptionUIType-COLOR))))
(define-public (gnc:make-budget-option section name key docstring)
  (issue-deprecation-warning "gnc:make-budget-option is deprecated. Make and register the option in one command with gnc-register-color-option.")
  (let ((option (gnc-make-qofinstance-option section name key docstring #f (GncOptionUIType-BUDGET))))
    (gnc:option-set-default-value option
                          (gnc-budget-get-default (gnc-get-current-book)))
    option))
(define-public (gnc:make-commodity-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-commodity-option is deprecated. Make and register the option in one command with gnc-register-commodity-option.")
  (gnc-make-commodity-option section name key docstring default))
(define-public (gnc:make-simple-boolean-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-simple-boolean-option is deprecated. Make and register the option in one command with gnc-register-simple-boolean-option.")
  (gnc-make-bool-option section name key docstring default (GncOptionUIType-BOOLEAN)))
(define-public (gnc:make-complex-boolean-option section name key docstring default setter-cb widget-changed-cb)
  (issue-deprecation-warning "gnc:make-complex-boolean-option is deprecated and its functionality removed. Make and register a simple-boolean in one command with gnc-register-simple-boolean-option and figure out some other way to change widget sensitivity.")
  (gnc-make-bool-option section name key docstring default (GncOptionUIType-BOOLEAN)))
(define-public (gnc:make-pixmap-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-pixmap-option is deprecated. Make and register the option in one command with gnc-register-pixmap-option.")
  (gnc-make-string-option section name key docstring default (GncOptionUIType-PIXMAP)))
;; gnc:make-account-list-option's getter and validator parameters are functions. 
(define-public (gnc:make-account-list-option section name key docstring default validator multi)
  (issue-deprecation-warning "gnc:make-account-list-option is deprecated. Make and register the option in one command with gnc-register-account-list-option.")
  (gnc-make-account-list-option section name key docstring (default)))
(define-public (gnc:make-account-list-limited-option section name key docstring default validator multi permitted)
  (issue-deprecation-warning "gnc:make-account-list-limited-option is deprecated. Make and register the option in one command with gnc-register-account-list-limited-option.")
  (gnc-make-account-list-limited-option section name key docstring (default) permitted))
(define-public (gnc:make-account-sel-limited-option section name key docstring default validator permitted)
  (issue-deprecation-warning "gnc:make-account-sel-limited-option is deprecated. Make and register the option in one command with gnc-register-account-sel-limited-option.")
  (let ((defval (if default (default) '())))
    (gnc-make-account-sel-limited-option section name key docstring defval permitted)))
(define-public (gnc:make-account-sel-option section name key docstring default validator)
  (let ((defval (if default (default) '())))
  (gnc-make-account-sel-limited-option section name key docstring defval '())))
(define-public (gnc:make-multichoice-option section name key docstring default multichoice)
  (issue-deprecation-warning "gnc:make-multichoice-option is deprecated. Make and register the option in one command with gnc-register-multichoice-option.")
  (let ((defval (cond ((symbol? default)
                       (symbol->string default))
                      ((number? default)
                       (number->string default))
                     (else default))))
  (gnc-make-multichoice-option section name key docstring defval multichoice)))
(define-public (gnc:make-multichoice-callback-option section name key docstring default multichoice setter-cb widget-changed-cb)
  (issue-deprecation-warning "gnc:make-multichoice-callback-option is deprecated in favor of a not-yet-written but more sensible way to conditionally enable and disable option widgets.")
  (gnc:make-multichoice-option section name key docstring default multichoice))

(define-public (gnc:make-list-option section name key docstring default multichoice)
  (issue-deprecation-warning "gnc:make-list-option is deprecated. Make and register the option in one command with gnc-register-list-option.")
  (let ((indexes (if default (map (lambda (def-item)
                                    (list-index (lambda (choice)
                                                  (eq? def-item
                                                       (vector-ref choice 0)))
                                                multichoice))
                                  default) 0)))
    (gnc-make-list-option section name key docstring indexes multichoice)))
(define-public (gnc:make-radiobutton-option section name key docstring default multichoice)
  (gnc:warn "gnc:make-radiobutton-option is no longer available. Using gnc:make-multichoice-option instead.")
  (gnc:make-multichoice-option section name key docstring default multichoice))
(define-public (gnc:make-number-range-option section name key docstring default min max dec-places step)
  (issue-deprecation-warning "gnc:make-number-range-option is deprecated. Make and register the option in one command with gnc-register-number-range-option.")
  (gnc-make-range-value-option section name key docstring default min max step))
(define-public (gnc:make-number-plot-size-option section name key docstring default min max dec-places step)
  (issue-deprecation-warning "gnc:make-number-plot-size-option is deprecated. Make and register the option in one command with gnc-register-plot-size-range-option.")
  ;; Ignore what the call asks for, only 10-100% makes sense.
  (gnc-make-plot-size-option section name key docstring 100 10 2000 1))
(define-public (gnc:make-query-option section name default)
  (issue-deprecation-warning "gnc:make-query-option is deprecated. Make and register the option in one command with gnc-register-query-option.")
  (let ((defv (if (list? default) default (gnc-query2scm default))))
    (gnc-make-query-option section name "" "query" defv (GncOptionUIType-INTERNAL))))
(define-public (gnc:make-internal-option section name default)
  (issue-deprecation-warning "gnc:make-internal-option is deprecated. Make and register the option in one command with gnc-register-internal-option.")
  (let ((type (GncOptionUIType-INTERNAL))
        (key "_")
        (desc "internal"))
    (cond
     ((boolean? default) (gnc-make-bool-option section name key desc default
                                               (GncOptionUIType-INTERNAL)))
     ((string? default) (gnc-make-string-option section name key desc default
                                                (GncOptionUIType-INTERNAL)))
     ((procedure? default)
        (format #t "gnc:make-internal-option passed procedure resolving to ~a~%" (default))
        (gnc-make-bool-option section name key desc #f (GncOptionUIType-INTERNAL)))
     (else
       (format #t "gnc:make-internal-option passed something unknown that looks like ~a~%" default)
       (gnc-make-bool-option section name key desc #f (GncOptionUIType-INTERNAL))))))

(define-public (gnc:make-owner-option section name key docstring getter validator owner-type)
  (issue-deprecation-warning "gnc:make-owner-option is deprecated. Make and register the option in one command with gnc-register-owner-option.")
  (let* ((ui-type (cond
                  ((eqv? owner-type GNC-OWNER-CUSTOMER) (GncOptionUIType-CUSTOMER))
                  ((eqv? owner-type GNC-OWNER-VENDOR) (GncOptionUIType-VENDOR))
                  ((eqv? owner-type GNC-OWNER-EMPLOYEE) (GncOptionUIType-EMPLOYEE))
                  ((eqv? owner-type GNC-OWNER-JOB) (GncOptionUIType-JOB))
                  (else (GncOptionUIType-INTERNAL))))

         (guid (gncOwnerReturnGUID (getter)))
         (book (gnc-get-current-book))
         (defval (cond
                  ((eqv? owner-type GNC-OWNER-CUSTOMER) (gncCustomerLookupFlip guid book))
                  ((eqv? owner-type GNC-OWNER-VENDOR) (gncVendorLookupFlip guid book))
                  ((eqv? owner-type GNC-OWNER-EMPLOYEE) (gncEmployeeLookupFlip guid book))
                  ((eqv? owner-type GNC-OWNER-JOB) (gncJobLookupFlip guid book)))))

    (gnc-make-gncowner-option section name key docstring defval ui-type)))
(define-public (gnc:make-invoice-option section name key docstring getter validator)
  (issue-deprecation-warning "gnc:make-invoice-option is deprecated. Make and register the option in one command with gnc-register-ionvoice-option.")
  (let ((defval (if getter (getter) #f)))
    (gnc-make-qofinstance-option section name key docstring defval (GncOptionUIType-INVOICE))))
(define-public (gnc:make-taxtable-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-taxtable-option is deprecated. Make and register the option in one command with gnc-register-taxtable-option.")
  (gnc-make-qofinstance-option section name key docstring default (GncOptionUIType-TAX-TABLE)))
(define-public (gnc:make-counter-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-number-range-option is deprecated. Make and register the option in one command with gnc-register-number-range-option.")
  (gnc-make-range-value-option section name key docstring default 0.0 999999999.0 1.0))

(define-public (gnc:make-counter-format-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-counter-format-option is deprecated. Make and register the option in one command with gnc-register-counter-format-option.")
  (gnc-make-string-option section name key docstring default (GncOptionUIType-STRING)))
(define-public (gnc:make-date-format-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-date-format-option is deprecated. Make and register the option in one command with gnc-register-date-format-option.")
  (gnc-make-string-option section name key docstring default (GncOptionUIType-DATE-FORMAT)))
(define-public (gnc:make-currency-option section name key docstring default)
  (issue-deprecation-warning "gnc:make-currency-option is deprecated. Make and register the option in one command with gnc-register-currency-option.")
  (gnc-make-currency-option section name key docstring default))
(define-public (gnc:make-date-option section name key docstring getter showtime
                                     subtype relative-date-list)
  (let ((default (getter))
        (both (if (eq? subtype 'both) #t #f)))
    (gnc-make-date-option section name key docstring default relative-date-list both)))

(define-public (gnc:options-make-end-date! options pagename optname sort-tag docstring)
  (gnc-register-end-date-option (gnc:optiondb options) pagename optname sort-tag docstring))

(define-public (gnc:options-make-date-interval! options pagename name-from info-from name-to info-to sort-tag)
  (gnc-register-start-date-option (gnc:optiondb options) pagename name-from
                                  (string-append sort-tag "a") info-from)
  (gnc-register-end-date-option (gnc:optiondb options) pagename name-to
                                (string-append sort-tag "b") info-to))
(define-public (gnc:date-option-absolute-time option-value)
  (if (pair? option-value)
          (if (eq? (car option-value) 'absolute)
              (cdr option-value)
              (gnc-relative-date-to-time64 (cdr option-value)))
          option-value))

(define-public (gnc:register-multichoice-callback-option options section name key docstring default multichoice widget-changed-cb)
  (let ((defval (cond ((symbol? default)
                       (symbol->string default))
                      ((number? default)
                       (number->string default))
                     (else default))))
  (gnc-register-multichoice-callback-option (gnc:optiondb options) section name key docstring defval multichoice widget-changed-cb)))

;; Scheme code for supporting options for the business modules
;;
;; Created by:	Derek Atkins <derek@ihtfp.com>
;;

;; Internally, values are always a guid. Externally, both guids and
;; invoice pointers may be used to set the value of the option. The
;; option always returns a single invoice pointer.

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
 (gnc:book-get-option-value book gnc:*business-label* key))

(define (gnc:fancy-date-info book key)
  ;; Access fancy date info from key-value pairs for current book
 (gnc:book-get-option-value book gnc:*business-label* (list gnc:*fancy-date-label* key)))



(define (gnc:options-fancy-date book)
  (let ((date-format (gnc:fancy-date-info book gnc:*fancy-date-format*)))
    (if (boolean? date-format) ;; date-format does not exist
        (qof-date-format-get-string (qof-date-format-get))
       date-format)))
