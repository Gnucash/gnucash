;;;; Preferences...

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

;; Account Types options

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show bank accounts"
  "a" "Show bank accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show cash accounts"
  "b" "Show cash accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show credit accounts"
  "c" "Show credit accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show asset accounts"
  "d" "Show asset accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show liability accounts"
  "e" "Show liability accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show stock accounts"
  "f" "Show stock accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show mutual fund accounts"
  "g" "Show mutual fund accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show currency accounts"
  "h" "Show currency accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show income accounts"
  "i" "Show income accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show expense accounts"
  "j" "Show expense accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show equity accounts"
  "k" "Show equity accounts in the account tree." #t))


;; Account Fields options

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account name"
  "a" "Show the account name column in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account type"
  "b" "Show the account type column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account code"
  "c" "Show the account code column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account description"
  "d" "Show the account description column in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account notes"
  "e" "Show the account notes column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account currency"
  "f" "Show the account currency column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account security"
  "g" "Show the account security column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account balance"
  "h" "Show the account balance column in the account tree." #t))


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

;; hack alert - we should probably get the default new account currency
;; from the locale
;; I haven't figured out if I can do this in scheme or need a C hook yet
(gnc:register-configuration-option
 (gnc:make-string-option
  "International" "Default Currency"
  "b" "Default Currency For New Accounts" "USD"))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "International" "Use 24-hour time format"
  "c" "Use a 24 hour (instead of a 12 hour) time format." #f))


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
        #(auto_single "Auto Single" "Single line mode with multi-line cursor")
        #(auto_double "Auto Double" "Double line mode with multi-line cursor")
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


;; Register Color options

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Header background"
  "a" "The header background color"
  (list #xff #xff #xff 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Single mode default even row background"
  "b" "The default background color for even rows in single mode"
  (list #xcc #xcc #xff 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Single mode default odd row background"
  "bb" "The default background color for odd rows in single mode"
  (list #xcc #xcc #xff 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Single mode active background"
  "c" "The background color for the active transaction in single mode"
  (list #xff #xdd #xdd 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Double mode default even row background"
  "d" "The default background color for even rows in double mode"
  (list #xcc #xcc #xff 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Double mode default odd row background"
  "e" "The default background color for odd rows in double mode"
  (list #xff #xff #xff 0)
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
  (list #xff #xdd #xdd 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Multi mode default transaction background"
  "g" "The default background color for transactions in multi-line mode and the auto modes"
  (list #xcc #xcc #xff 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Multi mode active transaction background"
  "h" "The background color for an active transaction in multi-line mode and the auto modes"
  (list #xff #xdd #xdd 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Multi mode default split background"
  "i" "The default background color for splits in multi-line mode and the auto modes"
  (list #xff #xff #xff 0)
  255
  #f))

(gnc:register-configuration-option
 (gnc:make-color-option
  "Register Colors" "Multi mode active split background"
  "j" "The background color for an active split in multi-line mode and the auto modes"
  (list #xff #xff #xdd 0)
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
 (gnc:make-number-range-option
  "__gui" "account_add_win_width" #f #f
        0.0 ;; default
        0.0 ;; lower bound
    32000.0 ;; upper bound
        0.0 ;; number of decimals
        1.0 ;; step size
        ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  "__gui" "account_add_win_height" #f #f
        0.0 ;; default
        0.0 ;; lower bound
    32000.0 ;; upper bound
        0.0 ;; number of decimals
        1.0 ;; step size
        ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  "__gui" "account_edit_win_width" #f #f
        0.0 ;; default
        0.0 ;; lower bound
    32000.0 ;; upper bound
        0.0 ;; number of decimals
        1.0 ;; step size
        ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  "__gui" "account_edit_win_height" #f #f
        0.0 ;; default
        0.0 ;; lower bound
    32000.0 ;; upper bound
        0.0 ;; number of decimals
        1.0 ;; step size
        ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  "__gui" "main_win_width" #f #f
        0.0 ;; default
        0.0 ;; lower bound
    32000.0 ;; upper bound
        0.0 ;; number of decimals
        1.0 ;; step size
        ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  "__gui" "main_win_height" #f #f
      400.0 ;; default
        0.0 ;; lower bound
    32000.0 ;; upper bound
        0.0 ;; number of decimals
        1.0 ;; step size
        ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  "__gui" "reg_win_width" #f #f
        0.0 ;; default
        0.0 ;; lower bound
    32000.0 ;; upper bound
        0.0 ;; number of decimals
        1.0 ;; step size
        ))

(gnc:register-configuration-option
 (gnc:make-number-range-option
  "__gui" "reg_stock_win_width" #f #f
        0.0 ;; default
        0.0 ;; lower bound
    32000.0 ;; upper bound
        0.0 ;; number of decimals
        1.0 ;; step size
        ))
