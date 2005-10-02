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

;;; Internal options -- Section names that start with "__" are not
;;; displayed in option dialogs.

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__exp_parser" "defined_variables" '()))
