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
    "b1" (N_ "The address of your business") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-contact*
    "b2" (N_ "The contact person to print on invoices") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-phone*
    "c1" (N_ "The phone number of your business") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-fax*
    "c2" (N_ "The fax number of your business") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-email*
    "c3" (N_ "The email address of your business") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-url*
    "c4" (N_ "The URL address of your website") ""))

  (reg-option
   (gnc:make-string-option
    gnc:*business-label* gnc:*company-id*
    "c5" (N_ "The ID for your company (eg 'Tax-ID: 00-000000)")
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
    "g" (N_ "The default date format used for fancy printed dates")
    #f))
    
  (reg-option 
   (gnc:make-simple-boolean-option
    gnc:*book-label* gnc:*trading-accounts*
    "a" (N_ "True if trading accounts should be used for transactions involving more than one commodity")
    #f))
)

(gnc-register-kvp-option-generator QOF-ID-BOOK-SCM book-options-generator)
