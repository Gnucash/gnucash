;; Preferences...
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
        '(ASSET     . "Appreciation")
        '(LIABILITY . "Debit")
        '(STOCK     . "Bought")
        '(MUTUAL    . "Bought")
        '(CURRENCY  . "Bought")
        '(INCOME    . "Charge")
        '(EXPENSE   . "Expense")
        '(EQUITY    . "Debit")))

(define gnc:*credit-strings*
  (list '(NO_TYPE   . "Funds Out")
        '(BANK      . "Withdrawal")
        '(CASH      . "Spend")
        '(CREDIT    . "Charge")
        '(ASSET     . "Depreciation")
        '(LIABILITY . "Credit")
        '(STOCK     . "Sold")
        '(MUTUAL    . "Sold")
        '(CURRENCY  . "Sold")
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
  (list #(us "US" "US-style: mm/dd/yyyy")
        #(uk "UK" "UK-style dd/mm/yyyy")
	#(ce "Europe" "Continental Europe: dd.mm.yyyy")
	#(iso "ISO" "ISO Standard: yyyy-mm-dd"))))
;	#(locale "Locale" "Take from system locale"))))

(gnc:register-configuration-option
 (gnc:make-currency-option
  "International" "Default Currency"
  "b" "Default Currency For New Accounts"
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


;; Register options

(gnc:register-configuration-option
 (gnc:make-multichoice-option
  "Register" "Default Register Mode"
  "a" "Choose the default mode for register windows"
  'single_line
  (list #(single_line "Single Line" "Show transactions on single lines")
        #(double_line "Double Line"
                      "Show transactions on two lines with more information")
        #(multi_line  "Multi Line" "Show transactions on multiple lines with one line for each split in the transaction")
        #(auto_single "Auto Single" "Single line mode with a multi-line cursor")
        #(auto_double "Auto Double" "Double line mode with a multi-line cursor")
        )))

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


;; Register Color options

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Header background"
  "a" "The header background color"
  (list #x96 #xb2 #x84 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Single mode default even row background"
  "b" "The default background color for even rows in single mode"
  (list #xf6 #xff #xdb 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Single mode default odd row background"
  "bb" "The default background color for odd rows in single mode"
  (list #xbf #xde #xba 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Single mode active background"
  "c" "The background color for the active transaction in single mode"
  (list #xff #xf7 #xba 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Double mode default even row background"
  "d" "The default background color for even rows in double mode"
  (list #xf6 #xff #xdb 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Double mode default odd row background"
  "e" "The default background color for odd rows in double mode"
  (list #xbf #xde #xba 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Register Colors" "Double mode colors alternate with transactions"
  "ee" "Alternate the even and odd colors with each transaction, not each row"
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Double mode active background"
  "f" "The background color for the active transaction in double mode"
  (list #xff #xf7 #xba 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Multi mode default transaction background"
  "g" "The default background color for transactions in multi-line mode and the auto modes"
  (list #xbf #xde #xba 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Multi mode active transaction background"
  "h" "The background color for an active transaction in multi-line mode and the auto modes"
  (list #xff #xf0 #x99 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Multi mode default split background"
  "i" "The default background color for splits in multi-line mode and the auto modes"
  (list #xff #xfa #xd9 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Multi mode active split background"
  "j" "The background color for an active split in multi-line mode and the auto modes"
  (list #xff #xf2 #xab 0)
  255
  #f))


;; General Options

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
  'default
  (list #(default "Income & Expense" "Reverse Income and Expense Accounts")
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

;(gnc:register-configuration-option
; (gnc:make-number-range-option
;  "General" "Default precision"
;  "f" "Default number of decimal places to display"
;   15.0 ;; default
;    1.0 ;; lower bound
;  200.0 ;; upper bound
;    0.0 ;; number of decimals
;    1.0 ;; step size
;  ))


;; Configuation variables

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


;; Internal options -- Section names that start with "__" are not
;; displayed in option dialogs.

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "account_add_win_width" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "account_add_win_height" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "account_edit_win_width" 0))

(gnc:register-configuration-option
 (gnc:make-internal-option
  "__gui" "account_edit_win_height" 0))

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


;; This needs to be after all the global options definitions
(if (gnc:debugging?)
    (gnc:options-register-translatable-strings gnc:*options-entries*))
