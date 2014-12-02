;; Business Preferences
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

(use-modules (gnucash gettext))

(define gnc:*option-section-counters* (N_ "Counters"))

;; This defines all available counter types to show options for. This a
;; list that contains a sublist for each counter type, containing: The
;; (untranslated) counter name, the format label, the previous number
;; label, the format help text and the previous number help text.
(define counter-types
  (list (list "gncCustomer"     (N_ "Customer number format")
                                (N_ "Customer number")
                                (N_ "The format string to use for generating customer numbers. This is a printf-style format string.")
                                (N_ "The previous customer number generated. This number will be incremented to generate the next customer number."))
        (list "gncEmployee"     (N_ "Employee number format")
                                (N_ "Employee number")
                                (N_ "The format string to use for generating employee numbers. This is a printf-style format string.")
                                (N_ "The previous employee number generated. This number will be incremented to generate the next employee number."))
        (list "gncInvoice"      (N_ "Invoice number format")
                                (N_ "Invoice number")
                                (N_ "The format string to use for generating invoice numbers. This is a printf-style format string.")
                                (N_ "The previous invoice number generated. This number will be incremented to generate the next invoice number."))
        (list "gncBill"         (N_ "Bill number format")
                                (N_ "Bill number")
                                (N_ "The format string to use for generating bill numbers. This is a printf-style format string.")
                                (N_ "The previous bill number generated. This number will be incremented to generate the next bill number."))
        (list "gncExpVoucher"   (N_ "Expense voucher number format")
                                (N_ "Expense voucher number")
                                (N_ "The format string to use for generating expense voucher numbers. This is a printf-style format string.")
                                (N_ "The previous expense voucher number generated. This number will be incremented to generate the next voucher number."))
        (list "gncJob"          (N_ "Job number format")
                                (N_ "Job number")
                                (N_ "The format string to use for generating job numbers. This is a printf-style format string.")
                                (N_ "The previous job number generated. This number will be incremented to generate the next job number."))
        (list "gncOrder"        (N_ "Order number format")
                                (N_ "Order number")
                                (N_ "The format string to use for generating order numbers. This is a printf-style format string.")
                                (N_ "The previous order number generated. This number will be incremented to generate the next order number."))
        (list "gncVendor"       (N_ "Vendor number format")
                                (N_ "Vendor number")
                                (N_ "The format string to use for generating vendor numbers. This is a printf-style format string.")
                                (N_ "The previous vendor number generated. This number will be incremented to generate the next vendor number."))
))

(define (book-options-generator options)
  (define (reg-option new-option)
    (gnc:register-option options new-option))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-name*
    "a" (N_ "The name of your business.") ""))

  (reg-option
   (gnc:make-text-option
    gnc:*business-label* gnc:*company-addy*
    "b1" (N_ "The address of your business.") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-contact*
    "b2" (N_ "The contact person to print on invoices.") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-phone*
    "c1" (N_ "The phone number of your business.") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-fax*
    "c2" (N_ "The fax number of your business.") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-email*
    "c3" (N_ "The email address of your business.") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-url*
    "c4" (N_ "The URL address of your website.") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-id*
    "c5" (N_ "The ID for your company (eg 'Tax-ID: 00-000000).")
    ""))
 
  (reg-option
   (gnc:make-taxtable-option
    gnc:*business-label* (N_ "Default Customer TaxTable")
    "e" (N_ "The default tax table to apply to customers.")
    (lambda () '()) #f))

  (reg-option
   (gnc:make-taxtable-option
    gnc:*business-label* (N_ "Default Vendor TaxTable")
    "f" (N_ "The default tax table to apply to vendors.")
    (lambda () '()) #f))

  (reg-option
   (gnc:make-dateformat-option
    gnc:*business-label* (N_ "Fancy Date Format")
    "g" (N_ "The default date format used for fancy printed dates.")
    #f))

  ;; Accounts tab

  (reg-option 
   (gnc:make-simple-boolean-option
    gnc:*option-section-accounts* gnc:*option-name-trading-accounts*
    "a" (N_ "Check to have trading accounts used for transactions involving more than one currency or commodity.")
    #f))

  (reg-option
   (gnc:make-number-range-option
	gnc:*option-section-accounts* gnc:*option-name-auto-readonly-days*
	"b" (N_ "Choose the number of days after which transactions will be read-only and cannot be edited anymore. This threshold is marked by a red line in the account register windows. If zero, all transactions can be edited and none are read-only.")
	0 ;; default
	0 ;; lower bound
	3650 ;; upper bound
	0 ;; number of decimals
	1 ;; step size
	))

  (reg-option 
   (gnc:make-simple-boolean-option
    gnc:*option-section-accounts* gnc:*option-name-num-field-source*
    "c" (N_ "Check to have split action field used in registers for 'Num' field in place of transaction number; transaction number shown as 'T-Num' on second line of register. Has corresponding effect on business features, reporting and imports/exports.")
    #f))

  ;; Budgeting Tab

  (reg-option
   (gnc:make-budget-option
    gnc:*option-section-budgeting* gnc:*option-name-default-budget*
    "a" (N_ "Budget to be used when none has been otherwise specified.")))

  ;; Counters Tab
  (map (lambda (vals)
               (let* (
		      ; Unpack the list of strings for this counter type
                      (key (car vals))
                      (sort-string key)
                      (format-label (cadr vals))
                      (number-label (caddr vals))
                      (format-description (cadddr vals))
                      (number-description (cadddr (cdr vals)))
                     )
                     (begin
		      ; For each counter-type we create an option for
		      ; the last used number and the format string to
		      ; use.
                      (reg-option
                       (gnc:make-counter-option
                        gnc:*option-section-counters* number-label key
                        (string-append sort-string "a") number-description 0))
                      (reg-option
                       (gnc:make-counter-format-option
                        gnc:*option-section-counters* format-label key
                        (string-append sort-string "b") format-description ""))
                      )
               )
       )
       ;; Make counter and format option for each defined counter
       counter-types
  )
)


(gnc-register-kvp-option-generator QOF-ID-BOOK-SCM book-options-generator)
