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

(gnc:support "prefs.scm")

(define gnc:*options-entries* (gnc:new-options))

(define (gnc:register-configuration-option new-option)
  (gnc:register-option gnc:*options-entries* new-option))

(define (gnc:lookup-global-option section name)
  (gnc:lookup-option gnc:*options-entries* section name))

(define (gnc:send-global-options) gnc:*options-entries*)

(define (gnc:global-options-clear-changes)
  (gnc:options-clear-changes gnc:*options-entries*))

(define (gnc:save-global-options)
  (gnc:make-home-dir)
  (gnc:save-options gnc:*options-entries*
                    (symbol->string 'gnc:*options-entries*)
                    (build-path (getenv "HOME") ".gnucash" "config.auto")
                    (string-append
                     "(gnc:config-file-format-version 1)\n\n"
                     "; GnuCash Configuration Options\n")))

(define (gnc:config-file-format-version version) #t)


;;;;;; Create default options and config vars

(define gnc:*debit-strings*
  (list '(NO_TYPE   . "Funds In")
        '(BANK      . "Deposit")
        '(CASH      . "Receive")
        '(CREDIT    . "Payment")
        '(ASSET     . "Increase")
        '(LIABILITY . "Debit")
        '(STOCK     . "Buy")
        '(MUTUAL    . "Buy")
        '(CURRENCY  . "Buy")
        '(INCOME    . "Charge")
        '(EXPENSE   . "Expense")
        '(EQUITY    . "Debit")))

(define gnc:*credit-strings*
  (list '(NO_TYPE   . "Funds Out")
        '(BANK      . "Withdrawal")
        '(CASH      . "Spend")
        '(CREDIT    . "Charge")
        '(ASSET     . "Decrease")
        '(LIABILITY . "Credit")
        '(STOCK     . "Sell")
        '(MUTUAL    . "Sell")
        '(CURRENCY  . "Sell")
        '(INCOME    . "Income")
        '(EXPENSE   . "Rebate")
        '(EQUITY    . "Credit")))

(if (gnc:debugging?)
    (let ((thunk (lambda (pair)
                   (gnc:register-translatable-strings (cdr pair)))))
      (map thunk gnc:*debit-strings*)
      (map thunk gnc:*credit-strings*)))

(define (gnc:get-debit-string type)
  (gnc:_ (assoc-ref gnc:*debit-strings* type)))

(define (gnc:get-credit-string type)
  (gnc:_ (assoc-ref gnc:*credit-strings* type)))


;; Main Window options

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Main Window" "Double click expands parent accounts"
  "a" "Double clicking on an account with children expands \
the account instead of opening a register." #f))

(gnc:register-configuration-option
 (gnc:make-list-option
  "Main Window" "Account types to display"
  "b" ""
  (list 'bank 'cash 'credit 'asset 'liability 'stock
        'mutual 'currency 'income 'expense 'equity)
  (list #(bank "Bank" "")
        #(cash "Cash" "")
        #(credit "Credit" "")
        #(asset "Asset" "")
        #(liability "Liability" "")
        #(stock "Stock" "")
        #(mutual "Mutual Fund" "")
        #(currency "Currency" "")
        #(income "Income" "")
        #(expense "Expense" "")
        #(equity "Equity" ""))))

(gnc:register-configuration-option
 (gnc:make-list-option
  "Main Window" "Account fields to display"
  "c" ""
  (list 'description 'total)
  (list #(type "Type" "")
        #(code "Code" "")
        #(description "Description" "")
        #(notes "Notes" "")
        #(currency "Currency" "")
        #(security "Security" "")
        #(balance "Balance" "")
        #(total "Total" ""))))


;; International options

(gnc:register-configuration-option
 (gnc:make-multichoice-option
  "International" "Date Format"
  "a" "Date Format Display" 'us
  (list #(us     "US"     "US-style: mm/dd/yyyy")
        #(uk     "UK"     "UK-style dd/mm/yyyy")
	#(ce     "Europe" "Continental Europe: dd.mm.yyyy")
	#(iso    "ISO"    "ISO Standard: yyyy-mm-dd")
	#(locale "Locale" "Default system locale format"))))

(gnc:register-configuration-option
 (gnc:make-currency-option
  "International" "Default Currency"
  "b" "Default currency for new accounts"
  (gnc:locale-default-currency)))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "International" "Use 24-hour time format"
  "c" "Use a 24 hour (instead of a 12 hour) time format." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "International" "Enable EURO support"
  "d" "Enables support for the European Union EURO currency" 
  (gnc:is-euro-currency
   (gnc:locale-default-currency))))


;;; Register options

(gnc:register-configuration-option
 (gnc:make-multichoice-option
  "Register" "Default Register Style"
  "a" "Default style for register windows"
  'ledger
  (list #(ledger "Ledger" "Show transactions on one or two lines")
        #(auto_ledger "Auto Ledger"
                      "Show transactions on one or two lines and expand the current transaction")
        #(journal "Journal"
                  "Show expanded transactions with all splits"))))

(gnc:register-configuration-option     
 (gnc:make-simple-boolean-option
  "Register" "Double Line Mode"
  "aa" "Show two lines of information for each transaction" #f))

(gnc:register-configuration-option     
 (gnc:make-simple-boolean-option
  "Register" "Auto-Raise Lists"
  "b" "Automatically raise the list of accounts or actions during input." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Register" "Show All Transactions"
  "c" "By default, show every transaction in an account." #t))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  "Register" "Number of Rows"
  "d" "Default number of register rows to display."
   15.0 ;; default
    1.0 ;; lower bound
  200.0 ;; upper bound
    0.0 ;; number of decimals
    1.0 ;; step size
  ))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Register" "Show Vertical Borders"
  "e" "By default, show vertical borders on the cells." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Register" "Show Horizontal Borders"
  "f" "By default, show horizontal borders on the cells." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Register" "'Enter' moves to blank transaction"
  "g" "If selected, move to the blank transaction after the user presses \
'Enter'. Otherwise, move down one row." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Register" "Confirm before changing reconciled"
  "h" "If selected, use a dialog to confirm a change to a reconciled \
transaction." #t))

(gnc:register-configuration-option
 (gnc:make-font-option
  "Register" "Register font"
  "i" "The font to use in the register" (gnc:register-default-font)))

(gnc:register-configuration-option
 (gnc:make-font-option
  "Register" "Register hint font"
  "j" "The font used to show hints in the register"
  (gnc:register-default-hint-font)))


;; Register Color options

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Header color"
  "a" "The header background color"
  (list #x96 #xb2 #x84 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Primary color"
  "b" "The default background color for register rows"
  (list #xbf #xde #xba 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Secondary color"
  "c" "The default secondary background color for register rows"
  (list #xf6 #xff #xdb 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Primary active color"
  "d" "The background color for the current register row"
  (list #xff #xf7 #xba 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Secondary active color"
  "e" "The secondary background color for the current register row"
  (list #xff #xf0 #x99 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Split color"
  "f" "The default background color for split rows in the register"
  (list #xff #xfa #xd9 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Split active color"
  "g" "The background color for the current split row in the register"
  (list #xff #xf2 #xab 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Register Colors" "Double mode colors alternate with transactions"
  "h" "Alternate the primary and secondary colors with each transaction, not each row"
  #f))


;;; Reconcile Options

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Reconcile" "Automatic credit card payments"
  "a" "After reconciling a credit card statement, prompt the user to enter a credit card payment"
  #t))


;;; General Options

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "General" "Save Window Geometry"
  "a" "Save window sizes and positions." #t))

(gnc:register-configuration-option
 (gnc:make-multichoice-option
  "General" "Toolbar Buttons"
  "b" "Choose whether to display icons, text, or both for toolbar buttons"
  'icons_and_text
  (list #(icons_and_text "Icons and Text" "Show both icons and text")
        #(icons_only "Icons only" "Show icons only")
        #(text_only "Text only" "Show text only"))))

(gnc:register-configuration-option
 (gnc:make-multichoice-option
  "General" "Account Separator"
  "c" "The character used to separate fully-qualified account names"
  'colon
  (list #(colon ": (Colon)" "Income:Salary:Taxable")
        #(slash "/ (Slash)" "Income/Salary/Taxable")
        #(backslash "\\ (Backslash)" "Income\\Salary\\Taxable")
        #(dash "- (Dash)" "Income-Salary-Taxable")
        #(period ". (Period)" "Income.Salary.Taxable"))))

(gnc:register-configuration-option
 (gnc:make-multichoice-option
  "General" "Reversed-balance account types"
  "d" "The types of accounts for which balances are sign-reversed"
 'credit
  (list #(income-expense "Income & Expense" "Reverse Income and Expense Accounts")
        #(credit "Credit Accounts" "Reverse Credit Card, Liability, Equity, and Income Accounts")
        #(none "None" "Don't reverse any accounts"))))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "General" "Use accounting labels"
  "e" "Only use 'debit' and 'credit' instead of informal synonyms" #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "General" "Display \"Tip of the Day\""
  "f" "Display hints for using GnuCash at startup" #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "General" "Display negative amounts in red"
  "g" "Display negative amounts in red" #t))

; this option also changes the next option so that its
; selectability matches the state of this option.
(gnc:register-configuration-option
 (gnc:make-complex-boolean-option
  "General" "Automatic Decimal Point"
  "h" 
  "Automatically insert a decimal point into values that are entered without one." 
  #f #f
  (lambda (x) (gnc:set-option-selectable-by-name "General"
                                                 "Auto Decimal Places"
                                                 x))))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  "General" "Auto Decimal Places"
  "i" "How many automatic decimal places will be filled in."
    ;; current range is 1-8 with default from the locale
    ( gnc:locale-decimal-places )  ;; default
    1.0 ;; lower bound
    8.0 ;; upper bound
    0.0 ;; number of decimals used for this range calculation
    1.0 ;; step size
  ))


;;; Configuation variables

(define gnc:*arg-show-version*
  (gnc:make-config-var
   "Show version."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*arg-show-usage*
  (gnc:make-config-var
   "Generate an argument summary."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*arg-show-help*
  (gnc:make-config-var
   "Generate an argument summary."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*arg-no-file*
  (gnc:make-config-var
   "Don't load any file, including autoloading the last file."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*config-dir*
  (gnc:make-config-var
   "Configuration directory."
   (lambda (var value) (if (string? value) (list value) #f))
   string=?
   gnc:_config-dir-default_))

(define gnc:*share-dir*
  (gnc:make-config-var
   "Shared files directory."
   (lambda (var value) (if (string? value) (list value) #f))
   string=?
   gnc:_share-dir-default_))

;; Convert the temporary startup value into a config var.
(let ((current-value gnc:*debugging?*))
  (set! 
   gnc:*debugging?*
   (gnc:make-config-var
    "Enable debugging code."
    (lambda (var value) (if (boolean? value) (list value) #f))
    eq?
    #f))
  (gnc:config-var-value-set! gnc:*debugging?* #f current-value))

(define gnc:*loglevel*
  (gnc:make-config-var
   "Logging level from 0 (least logging) to 5 (most logging)."
   (lambda (var value) (if (exact? value) (list value) #f))
   eq?
   #f))

;; Convert the temporary startup value into a config var.
(let ((current-load-path gnc:*load-path*))
  (set!
   gnc:*load-path*
   (gnc:make-config-var
    "A list of strings indicating the load path for (gnc:load name).
Each element must be a string representing a directory or a symbol
where 'default expands to the default path, and 'current expands to
the current value of the path."
    (lambda (var value)
      (let ((result (gnc:_expand-load-path_ value)))
        (if (list? result)
            (list result)
            #f)))
    equal?
    '(default)))
  (gnc:config-var-value-set! gnc:*load-path* #f current-load-path))

(define gnc:*doc-path*

  (gnc:make-config-var
   "A list of strings indicating where to look for html and parsed-html files
Each element must be a string representing a directory or a symbol
where 'default expands to the default path, and 'current expands to
the current value of the path."
   (lambda (var value)
     (let ((result (gnc:_expand-doc-path_ value)))
       (if (list? result)
           (list result)
           #f)))
   equal?
   '(default)))


;;; Internal options -- Section names that start with "__" are not
;;; displayed in option dialogs.

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "account_win_width" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "account_win_height" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "main_win_width" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "main_win_height" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "reg_win_width" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "reg_stock_win_width" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "reg_column_widths" '()))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__exp_parser" "defined_variables" '()))


;; This needs to be after all the global options definitions
(if (gnc:debugging?)
    (gnc:options-register-translatable-strings gnc:*options-entries*))
