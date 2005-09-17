;; Preferences
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

(require 'sort)
(require 'hash-table)
(use-modules (g-wrapped gw-core-utils))

;; (define gnc:*double-entry-restriction*
;;   (gnc:make-config-var
;;    "Determines how the splits in a transaction will be balanced. 
;;  The following values have significance:
;; 
;;    #f        anything goes
;; 
;;    'force    The sum of all splits in a transaction will be
;;              forced to be zero, even if this requires the
;;              creation of additional splits.  Note that a split
;;              whose value is zero (e.g. a stock price) can exist
;;              by itself. Otherwise, all splits must come in at 
;;              least pairs.
;; 
;;    'collect  splits without parents will be forced into a
;;              lost & found account.  (Not implemented)"
;;    (lambda (var value)
;;      (cond
;;       ((eq? value #f)
;;        (_gnc_set_force_double_entry_ 0)
;;        (list value))
;;       ((eq? value 'force)
;;        (_gnc_set_force_double_entry_ 1)
;;        (list value))
;;       ((eq? value 'collect)
;;        (gnc:warn
;;         "gnc:*double-entry-restriction* -- 'collect not supported yet.  "
;;         "Ignoring.")
;;        #f)
;;       (else
;;        (gnc:warn
;;         "gnc:*double-entry-restriction* -- " value " not supported.  Ignoring.")
;;        #f)))
;;    eq?
;;    #f))

(define gnc:*options-entries* (gnc:new-options))

(define (gnc:register-configuration-option new-option)
  (gnc:register-option gnc:*options-entries* new-option))

(define (gnc:lookup-global-option section name)
  (gnc:lookup-option gnc:*options-entries* section name))

(define (gnc:send-global-options) gnc:*options-entries*)

(define (gnc:global-options-clear-changes)
  (gnc:options-clear-changes gnc:*options-entries*))

;; save-all-options: this is the actual hook that gets called at
;; shutdown.  right now, we put all the options in the same file so
;; it's important to make sure it happens in this order.  later the
;; hook should probably revert back to just save-global-options.
(define (gnc:save-all-options)
  (gnc:save-global-options)
  (gnc:hook-run-danglers gnc:*save-options-hook*))

(define (gnc:save-global-options)
  (gnc:make-home-dir)
  (gnc:save-options gnc:*options-entries*
                    (symbol->string 'gnc:*options-entries*)
                    gnc:current-config-auto
                    (string-append
                     "(gnc:config-file-format-version 1)\n\n"
                     ";"
                     (_ "GnuCash Configuration Options")
                     "\n")
                    #t))

(define (gnc:config-file-format-version version) #t)


;;;;;; Create default options and config vars

(define gnc:*debit-strings*
  (list (cons 'NO_TYPE   (N_ "Funds In"))
        (cons 'BANK      (N_ "Deposit"))
        (cons 'CASH      (N_ "Receive"))
        (cons 'CREDIT    (N_ "Payment"))
        (cons 'ASSET     (N_ "Increase"))
        (cons 'LIABILITY (N_ "Decrease"))
        (cons 'STOCK     (N_ "Buy"))
        (cons 'MUTUAL    (N_ "Buy"))
        (cons 'CURRENCY  (N_ "Buy"))
        (cons 'INCOME    (N_ "Charge"))
        (cons 'EXPENSE   (N_ "Expense"))
	(cons 'PAYABLE   (N_ "Payment"))
	(cons 'RECEIVABLE (N_ "Invoice"))
        (cons 'EQUITY    (N_ "Decrease"))))

(define gnc:*credit-strings*
  (list (cons 'NO_TYPE   (N_ "Funds Out"))
        (cons 'BANK      (N_ "Withdrawal"))
        (cons 'CASH      (N_ "Spend"))
        (cons 'CREDIT    (N_ "Charge"))
        (cons 'ASSET     (N_ "Decrease"))
        (cons 'LIABILITY (N_ "Increase"))
        (cons 'STOCK     (N_ "Sell"))
        (cons 'MUTUAL    (N_ "Sell"))
        (cons 'CURRENCY  (N_ "Sell"))
        (cons 'INCOME    (N_ "Income"))
        (cons 'EXPENSE   (N_ "Rebate"))
	(cons 'PAYABLE   (N_ "Bill"))
	(cons 'RECEIVABLE (N_ "Payment"))
        (cons 'EQUITY    (N_ "Increase"))))

(define (gnc:get-debit-string type)
  (_ (assoc-ref gnc:*debit-strings* type)))

(define (gnc:get-credit-string type)
  (_ (assoc-ref gnc:*credit-strings* type)))

;; International options
(gnc:register-configuration-option
 (gnc:make-multichoice-option
  (N_ "International") (N_ "Date Format")
  "a" (N_ "Date Format Display") 'locale
  (list (list->vector (list 'us
                            (N_ "US (12/31/2001)")
                            (N_ "US-style: mm/dd/yyyy")))
        (list->vector (list 'uk
                            (N_ "UK (31/12/2001)")
                            (N_ "UK-style dd/mm/yyyy")))
        (list->vector (list 'ce
                            (N_ "Europe (31.12.2001)")
                            (N_ "Continental Europe: dd.mm.yyyy")))
        (list->vector (list 'iso
                            (N_ "ISO (2001-12-31)")
                            (N_ "ISO Standard: yyyy-mm-dd")))
        (list->vector (list 'locale
                            (N_ "Locale")
                            (N_ "Default system locale format"))))))

(gnc:register-configuration-option
 (gnc:make-currency-option
  (N_ "International") (N_ "New Account Default Currency")
  "b1" (N_ "Default currency for new accounts")
  (gnc:locale-default-iso-currency-code)))

(gnc:register-configuration-option
 (gnc:make-currency-option
  (N_ "International") (N_ "Default Report Currency")
  "b2" (N_ "Default currency for reports")
  (gnc:locale-default-iso-currency-code)))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "International") (N_ "Use 24-hour time format")
  "c" (N_ "Use a 24 hour (instead of a 12 hour) time format.") #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "International") (N_ "Enable EURO support")
  "d" (N_ "Enables support for the European Union EURO currency") 
  (gnc:is-euro-currency-code (gnc:locale-default-iso-currency-code))))

(gnc:register-configuration-option
 (gnc:make-dateformat-option
  (N_ "International") (N_ "Fancy Date Format")
  "e" (N_ "The default date format used for fancy printed dates") #f))


;;; Summarybar Options

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Summarybar") (N_ "Show grand total")
  "a" (N_ "Show a grand total of all accounts converted to the default report currency")
  #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Summarybar") (N_ "Show non currency commodities")
  "b" (N_ "Show non currency commodities")
  #t))

(gnc:options-make-date-interval!
 gnc:*options-entries* (N_ "Summarybar")
 (N_ "Start date") (N_ "Start date for profit/loss calculation")
 (N_ "End date") (N_ "End date for profit/loss and date for net assets calculation")
 "c")

;;; Reconcile Options

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Reconcile") (N_ "Automatic interest transfer")
  "a" (N_ "Prior to reconciling an account which charges or pays interest, \
prompt the user to enter a transaction for the interest charge or payment.
Currently only enabled for Bank, Credit, Mutual, Asset, Receivable, Payable, and Liability accounts.")
  #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Reconcile") (N_ "Automatic credit card payments")
  "b" (N_ "After reconciling a credit card statement, prompt the user \
to enter a credit card payment")
  #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Reconcile") (N_ "Check off cleared transactions")
  "c" (N_ "Automatically check off cleared transactions when reconciling")
  #t))


;;; User Info Options

;(gnc:register-configuration-option
; (gnc:make-string-option
;  (N_ "User Info") (N_ "User Name")
;  "b" (N_ "The name of the user. This is used in some reports.") ""))

;(gnc:register-configuration-option
; (gnc:make-text-option
;  (N_ "User Info") (N_ "User Address")
;  "c" (N_ "The address of the user. This is used in some reports.") ""))


;;; General Options

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "General") (N_ "Show Advanced Settings")
  "a" (N_ "Allow modification of less commonly used settings.") #f))

;; QIF Import options. 

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  (N_ "Online Banking & Importing") (N_ "QIF Verbose documentation")
  "a" (N_ "Show some documentation-only pages in QIF Import druid")
  #t))


;; Network/security options 
;;(gnc:register-configuration-option
;; (gnc:make-simple-boolean-option
;;  (N_ "Network") (N_ "Allow http network access")
;;  "a" (N_ "Enable GnuCash's HTTP client support.")
;;  #t))
;;
;;(gnc:register-configuration-option
;; (gnc:make-simple-boolean-option
;;  (N_ "Network") (N_ "Allow https connections using OpenSSL")
;;  "b" (N_ "Enable secure HTTP connections using OpenSSL")
;;  #t))
;;
;;(gnc:register-configuration-option
;; (gnc:make-simple-boolean-option
;;  (N_ "Network") (N_ "Enable GnuCash Network")
;;  "c" (N_ "The GnuCash Network server provides support and other services")
;;  #t))
;;
;;(gnc:register-configuration-option 
;; (gnc:make-string-option
;;  (N_ "Network") (N_ "GnuCash Network server") 
;;  "d" (N_ "Host to connect to for user registration and support services")
;;  "www.gnucash.org"))


;;; Advanced Options

(gnc:register-configuration-option
 (gnc:make-number-range-option
  (N_ "_+Advanced") (N_ "New Search Limit")
  "j" (N_ "Default to 'new search' if fewer than this number of items is returned.")
    1.0 ;; default
    1.0 ;; lower bound
  100.0 ;; upper bound
    0.0 ;; number of decimals
    1.0 ;; step size
  ))


;;; Internal options -- Section names that start with "__" are not
;;; displayed in option dialogs.

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__exp_parser" "defined_variables" '()))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gnc_network" "uid" ""))
