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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "invoice_reg_width" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "bill_reg_width" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "voucher_reg_width" 0))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  gnc:*business-label* (N_ "Number of Rows")
  "a" (N_ "Default number of register rows to display in Invoices.")
   10.0 ;; default
    1.0 ;; lower bound
  200.0 ;; upper bound
    0.0 ;; number of decimals
    1.0 ;; step size
  ))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  gnc:*business-label* (N_ "Invoice Tax Included?")
  "f1" (N_ (string-append
	   "Whether tax is included by default in entries on Invoices. "
	   "This setting is inherited by new customers and vendors"))
  #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  gnc:*business-label* (N_ "Bill Tax Included?")
  "f2" (N_ (string-append
	   "Whether tax is included by default in entries on Bills. "
	   "This setting is inherited by new customers and vendors"))
  #f))

(gnc:register-configuration-option
 (gnc:make-complex-boolean-option
  gnc:*business-label* (N_ "Notify Bills Due?")
  "g" (N_ "Whether to display the list of Bills Due at startup.")
  #t #f
  (lambda (x) (gnc:set-option-selectable-by-name gnc:*business-label*
						 "Bills Due Days"
						 x))
  ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  gnc:*business-label* (N_ "Bills Due Days")
  "h" (N_ "How many days in the future to warn about Bills coming due.")
    7.0 ;; default
    1.0 ;; lower bound
  180.0 ;; upper bound
    0.0 ;; number of decimals
    1.0 ;; step size
  ))


(define (book-options-generator options)
  (define (reg-option new-option)
    (gnc:register-option options new-option))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-name*
    "a" (N_ "The name of your business") ""))

  (reg-option
   (gnc:make-text-option
    gnc:*business-label* gnc:*company-addy*
    "b" (N_ "The address of your business") ""))

  (reg-option
   (gnc:make-taxtable-option
    gnc:*business-label* (N_ "Default Customer TaxTable")
    "e" (N_ "The default tax table to apply to customers.")
    (lambda () #f) #f))

  (reg-option
   (gnc:make-taxtable-option
    gnc:*business-label* (N_ "Default Vendor TaxTable")
    "f" (N_ "The default tax table to apply to vendors.")
    (lambda () #f) #f))
)

(gnc:register-kvp-option-generator gnc:id-book book-options-generator)
