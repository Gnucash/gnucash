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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org

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
(use-modules (gnucash gettext))

;; Old-school config files depend on this API
 (define (gnc:config-file-format-version version) #t)

;;;;;; Create config vars

(define gnc:*debit-strings*
  (list (cons ACCT-TYPE-NONE       (N_ "Funds In"))
        (cons ACCT-TYPE-BANK       (N_ "Deposit"))
        (cons ACCT-TYPE-CASH       (N_ "Receive"))
        (cons ACCT-TYPE-CREDIT     (N_ "Payment"))
        (cons ACCT-TYPE-ASSET      (N_ "Increase"))
        (cons ACCT-TYPE-LIABILITY  (N_ "Decrease"))
        (cons ACCT-TYPE-STOCK      (N_ "Buy"))
        (cons ACCT-TYPE-MUTUAL     (N_ "Buy"))
        (cons ACCT-TYPE-CURRENCY   (N_ "Buy"))
        (cons ACCT-TYPE-INCOME     (N_ "Charge"))
        (cons ACCT-TYPE-EXPENSE    (N_ "Expense"))
        (cons ACCT-TYPE-PAYABLE    (N_ "Payment"))
        (cons ACCT-TYPE-RECEIVABLE (N_ "Invoice"))
        (cons ACCT-TYPE-TRADING    (N_ "Decrease"))
        (cons ACCT-TYPE-EQUITY     (N_ "Decrease"))))

(define gnc:*credit-strings*
  (list (cons ACCT-TYPE-NONE       (N_ "Funds Out"))
        (cons ACCT-TYPE-BANK       (N_ "Withdrawal"))
        (cons ACCT-TYPE-CASH       (N_ "Spend"))
        (cons ACCT-TYPE-CREDIT     (N_ "Charge"))
        (cons ACCT-TYPE-ASSET      (N_ "Decrease"))
        (cons ACCT-TYPE-LIABILITY  (N_ "Increase"))
        (cons ACCT-TYPE-STOCK      (N_ "Sell"))
        (cons ACCT-TYPE-MUTUAL     (N_ "Sell"))
        (cons ACCT-TYPE-CURRENCY   (N_ "Sell"))
        (cons ACCT-TYPE-INCOME     (N_ "Income"))
        (cons ACCT-TYPE-EXPENSE    (N_ "Rebate"))
        (cons ACCT-TYPE-PAYABLE    (N_ "Bill"))
        (cons ACCT-TYPE-RECEIVABLE (N_ "Payment"))
        (cons ACCT-TYPE-TRADING    (N_ "Increase"))
        (cons ACCT-TYPE-EQUITY     (N_ "Increase"))))

(define (gnc:get-debit-string type)
  (_ (assoc-ref gnc:*debit-strings* type)))

(define (gnc:get-credit-string type)
  (_ (assoc-ref gnc:*credit-strings* type)))
