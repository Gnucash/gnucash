;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balsheet-eg.scm
;; by Chris Dennis  chris@starsoftanalysis.co.uk
;;
;; - eguile version of ...
;; balance-sheet.scm: balance sheet
;;
;; By Robert Merkel <rgmerk@mira.net>
;;
;; Heavily modified and Frankensteined by David Montenegro
;;   2004.06.12-2004.06.23 <sunrise2000@comcast.net>
;;
;; $Author: chris $ $Date: 2009/07/02 10:16:02 $ $Revision: 1.44 $
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report balsheet-eg))
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (gnucash report eguile-gnc))
(use-modules (gnucash report eguile-utilities))

(use-modules (ice-9 regex))  ; for regular expressions
(cond-expand
  (guile-2
      (use-modules (ice-9 local-eval)))  ; for the-environment
  (else ))
(use-modules (srfi srfi-13)) ; for extra string functions

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/html" 0)

(define debugging? #f)

;;; these could go into a separate module..........
;;;
;; Useful routines to use in the template
(define (escape-html s1)
  ;; convert string s1 to escape HTML special characters < > and &
  ;; i.e. convert them to &lt; &gt; and &amp; respectively.
  ;; Maybe there's a way to do this in one go... (but order is important)
  (set! s1 (regexp-substitute/global #f "&" s1 'pre "&amp;" 'post))
  (set! s1 (regexp-substitute/global #f "<" s1 'pre "&lt;" 'post))
  (regexp-substitute/global #f ">" s1 'pre "&gt;" 'post))

(define (nl->br str)
  ;; replace newlines with <br>
  (regexp-substitute/global #f "\n" str 'pre "<br />" 'post))

(define (nbsp str)
  ;; replace spaces with &nbsp; (non-breaking spaces)
  ;; (yes, I know <nobr> is non-standard, but webkit splits e.g. "-£40.00" between
  ;; the '-' and the '£' without it.)
  (string-append "<nobr>" (regexp-substitute/global #f " " str 'pre "&nbsp;" 'post) "</nobr>"))

(define (dump x) (escape-html (object->string x)))
(define (ddump x) (display (dump x)))

(define (string-repeat s n)
  ;; return a string made of n copies of string s
  ;; (there's probably a better way)
  (let ((s2 ""))
    (do ((i 1 (1+ i))) ((> i n))
      (set! s2 (string-append s2 s)))
    s2))

(define (debug . args)
  (if debugging?
    (for arg in args do
        (if (string? arg)
          (display (string-append arg " "))
          (display (string-append (dump arg) " "))))
      ))

(define (hrule cols) ; in fact just puts in an empty row for spacing
  (display "<tr valign=\"center\"><td colspan=\"")
  (display cols)
  (display "\">&nbsp;</td></tr>\n"))

(define (empty-cells n)
  ;; Display n empty table cells
  (display (string-repeat "<td class=\"empty\"></td>" n)))

(define (indent-cells n)
  ;; Display n empty table cells with width attribute for indenting
  ;; (the &nbsp;s are just there in case CSS isn't working)
  (display (string-repeat "<td min-width=\"32\" class=\"indent\">&nbsp;&nbsp;</td>" n)))

;; 'Safe' versions of cdr and cadr that don't crash
;; if the list is empty  (is there a better way?)
(define (safe-cdr l)
  (if (null? l)
    '()
    (cdr l)))
(define (safe-cadr l)
  (if (null? l)
    '()
    (if (null? (cdr l))
      '()
      (cadr l))))

(define (gnc:company-info key) ; this should be in business-utils.scm soon
  ;; Access company info from key-value pairs for current book
  (kvp-frame-get-slot-path-gslist
    (qof-book-get-slots (gnc-get-current-book))
    (append gnc:*kvp-option-path* (list gnc:*business-label* key))))

(define (add-to-cc cc com num neg?)
  ; add a numeric and commodity to a commodity-collector,
  ; changing sign if required
  (if neg?
    (cc 'add com (gnc-numeric-neg num))
    (cc 'add com num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Define an account record for cacheing information about all the accounts
(define (accrec-printer accrec port)
  ;; accrec printer.  This is for debugging reports, so it uses
  ;; HTML for pretty-printing
  (set-current-output-port port)
  (display "accrec:- ")
  (display " account: ")     (display (dump (accrec-account accrec)))
  (display " code: ")        (display (accrec-code accrec))
  (display " placeholder: ") (display (dump (accrec-placeholder? accrec)))
  (display " namelink: ")    (display (accrec-namelink accrec))
  (display " commodity: ")   (if (accrec-commodity accrec)
                               (display (gnc-commodity-get-mnemonic (accrec-commodity accrec)))
                               (display "#f"))
  (display " balance-num: ") (if (accrec-balance-num accrec)
                               (display (gnc-numeric-to-double (accrec-balance-num accrec)))
                               ;(display (gnc:monetary->string (accrec-balance-mny accrec)))
                               ;(display (format-monetary (accrec-balance-num accrec))) ; not this -- too fancy
                               (display "#f"))
  (display " depth: ")       (display (accrec-depth accrec))
  (display " treedepth: ")   (display (accrec-treedepth accrec))
  (display " non-zero?: ")   (display (accrec-non-zero? accrec))
  (display " summary?: ")    (display (accrec-summary? accrec))
  (display " subtotal-cc: ") (if (accrec-subtotal-cc accrec)
                               ;(display (get-comm-coll-total (accrec-subtotal-cc accrec) #f))
                               ;(display (format-comm-coll (accrec-subtotal-cc accrec)))
                               (display
                                 (string-concatenate
                                   (map-in-order
                                     (lambda (mny)
                                       (string-append (gnc:monetary->string mny) " "))
                                     ((accrec-subtotal-cc accrec) 'format gnc:make-gnc-monetary #f))))
                               (display "#f"))
  (display " sublist: ")     (if (accrec-sublist accrec)
                               (begin
                                 (display "\n<ul>")
                                 (for sub-accrec in (accrec-sublist accrec) do
                                     (display "\n<li>")
                                     (accrec-printer sub-accrec port)
                                     (display "</li>"))
                                 (display "</ul>"))
                               (display "#f")))
(define accrectype (make-record-type "accrecc"
                                     '(account
                                        code
                                        placeholder?
                                        namelink ; a/c name, as link if required
                                        commodity
                                        balance-num ; excluding sublist
                                        depth
                                        treedepth
                                        non-zero?  ; #t if this or any sub-a/cs are non zero
                                        summary?   ; #t if subaccounts summarised here
                                        subtotal-cc ; of sublist plus this a/c
                                        sublist)
                                     accrec-printer))
(define newaccrec-full (record-constructor accrectype))                ; requires all the fields
(define newaccrec-empty (record-constructor accrectype '()))        ; all fields default to #f
(define newaccrec (record-constructor accrectype '(account         ; most-likely-to-be-needed fields
                                                    code
                                                    placeholder?
                                                    namelink
                                                    commodity
                                                    balance-num
                                                    depth
                                                    treedepth)))
(define (newaccrec-clean)
  ;; Create a new accrec with 'clean' empty values, e.g. strings are "", not #f
  (newaccrec-full #f         ; account
                  ""         ; code
                  #f         ; placeholder?
                  ""         ; namelink
                  (gnc-default-currency)         ; commodity
                  (gnc-numeric-zero) ; balance-num
                  0         ; depth
                  0         ; treedepth
                  #f         ; non-zero?
                  #f        ; summary?
                  (gnc:make-commodity-collector) ; subtotal-cc
                  #f        ;'()        ; sublist
                  ))
(define accrec? (record-predicate accrectype))
(define accrec-account      (record-accessor accrectype 'account))
(define accrec-code         (record-accessor accrectype 'code))
(define accrec-placeholder? (record-accessor accrectype 'placeholder?))
(define accrec-namelink     (record-accessor accrectype 'namelink))
(define accrec-commodity    (record-accessor accrectype 'commodity))
(define accrec-balance-num  (record-accessor accrectype 'balance-num))
(define (accrec-balance-mny accrec)
  (gnc:make-gnc-monetary (accrec-commodity accrec) (accrec-balance-num accrec)))
(define accrec-depth        (record-accessor accrectype 'depth))
(define accrec-treedepth    (record-accessor accrectype 'treedepth))
(define accrec-non-zero?    (record-accessor accrectype 'non-zero?))
(define accrec-summary?     (record-accessor accrectype 'summary?))
(define accrec-subtotal-cc  (record-accessor accrectype 'subtotal-cc))
(define accrec-sublist      (record-accessor accrectype 'sublist))
(define accrec-set-account!      (record-modifier accrectype 'account))
(define accrec-set-code!         (record-modifier accrectype 'code))
(define accrec-set-placeholder?! (record-modifier accrectype 'placeholder?))
(define accrec-set-namelink!     (record-modifier accrectype 'namelink))
(define accrec-set-commodity!    (record-modifier accrectype 'commodity))
(define accrec-set-balance-num!  (record-modifier accrectype 'balance-num))
(define (accrec-set-balance-mny! accrec mny)
  (accrec-set-commodity!   accrec (gnc:gnc-monetary-commodity mny))
  (accrec-set-balance-num! accrec (gnc:gnc-monetary-amount    mny)))
(define accrec-set-depth!        (record-modifier accrectype 'depth))
(define accrec-set-treedepth!    (record-modifier accrectype 'treedepth))
(define accrec-set-non-zero?!    (record-modifier accrectype 'non-zero?))
(define accrec-set-summary?!     (record-modifier accrectype 'summary?))
(define accrec-set-subtotal-cc!  (record-modifier accrectype 'subtotal-cc))
(define accrec-set-sublist!      (record-modifier accrectype 'sublist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All the options stuff starts here

(define reportname (_ "Balance Sheet (eguile)"))

;; define all option's names and help text so that they are properly
;; defined in *one* place.
(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report."))

(define optname-date    (N_ "Balance Sheet Date"))
(define optname-columns (N_ "1- or 2-column report"))
(define opthelp-columns
  (N_ "The balance sheet can be displayed with either 1 or 2 columns. 'auto' means that the layout will be adjusted to fit the width of the page."))

(define optname-depth-limit (N_ "Levels of Subaccounts"))
(define opthelp-depth-limit (N_ "Maximum number of levels in the account tree displayed."))
(define optname-flatten?    (N_ "Flatten list to depth limit"))
(define opthelp-flatten?
  (N_ "Displays accounts which exceed the depth limit at the depth limit."))

(define optname-omit-zb-accts (N_ "Exclude accounts with zero total balances"))
(define opthelp-omit-zb-accts
  (N_ "Exclude non-top-level accounts with zero balance and no non-zero sub-accounts."))

(define optname-account-links (N_ "Display accounts as hyperlinks"))
(define opthelp-account-links (N_ "Shows each account in the table as a hyperlink to its register window."))

(define optname-neg-format (N_ "Negative amount format"))
(define opthelp-neg-format
  (N_ "The formatting to use for negative amounts: with a leading sign, or enclosing brackets."))

(define optname-font-family    (N_ "Font family"))
(define opthelp-font-family    (N_ "Font definition in CSS font-family format."))
(define optname-font-size      (N_ "Font size"))
(define opthelp-font-size      (N_ "Font size in CSS font-size format (e.g. \"medium\" or \"10pt\")."))
(define optname-template-file  (N_ "Template file"))
(define opthelp-template-file
  (N_ "The file name of the eguile template part of this report. This file must be in your .gnucash directory, or else in its proper place within the GnuCash installation directories."))
(define optname-css-file  (N_ "CSS stylesheet file"))
(define opthelp-css-file
  (N_ "The file name of the CSS stylesheet to use with this report. If specified, this file should be in your .gnucash directory, or else in its proper place within the GnuCash installation directories."))
(define optname-extra-notes (N_ "Extra Notes"))
(define opthelp-extra-notes (N_ "Notes added at end of invoice -- may contain HTML markup."))

(define optname-report-commodity (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-foreign (N_ "Show Foreign Currencies"))
(define opthelp-show-foreign
  (N_ "Display any foreign currency amount in an account."))

(define accounts-page    gnc:pagename-accounts)
(define commodities-page (N_ "Commodities"))
(define display-page     gnc:pagename-display)
(define general-page     gnc:pagename-general)
(define notes-page       (N_ "Notes"))

;; options generator
(define (balsheet-options-generator)
  (let* ((options (gnc:new-options))
         (add-option
           (lambda (new-option)
             (gnc:register-option options new-option))))

    ;; Accounts options
    (add-option (gnc:make-simple-boolean-option accounts-page optname-omit-zb-accts
                                                "a" opthelp-omit-zb-accts #f))
    (add-option (gnc:make-simple-boolean-option accounts-page optname-account-links
                                                "b" opthelp-account-links #t))
    (gnc:options-add-account-levels!  options accounts-page optname-depth-limit
                                      "c" opthelp-depth-limit 'all)
    (add-option (gnc:make-simple-boolean-option accounts-page optname-flatten?
                                                "d" opthelp-flatten? #f))

    ;; Commodity options
    (gnc:options-add-currency! options commodities-page optname-report-commodity "a")
    (gnc:options-add-price-source!  options commodities-page
                                    optname-price-source "b" 'average-cost)
    (add-option (gnc:make-simple-boolean-option commodities-page optname-show-foreign
                                                "c" opthelp-show-foreign #t))

    ;; Display options
    (add-option (gnc:make-multichoice-option
                  display-page optname-columns
                  "a" opthelp-columns 'onecol
                  (list (vector 'autocols
                                (N_ "Auto")
                                (N_ "Adjust the layout to fit the width of the screen or page."))
                        (vector 'onecol
                                (N_ "One")
                                (N_ "Display liabilities and equity below assets."))
                        (vector 'twocols
                                (N_ "Two")
                                (N_ "Display assets on the left, liabilities and equity on the right.")))))
    (add-option (gnc:make-multichoice-option
                  display-page optname-neg-format
                  "b" opthelp-neg-format 'negsign
                  (list (vector 'negsign
                                (N_ "Sign")
                                (N_ "Prefix negative amounts with a minus sign, e.g. -$10.00."))
                        (vector 'negbrackets
                                (N_ "Brackets")
                                (N_ "Surround negative amounts with brackets, e.g. ($100.00).")))))
    (add-option (gnc:make-string-option display-page optname-font-family "c"
                                        opthelp-font-family "sans"))
    (add-option (gnc:make-string-option display-page optname-font-size "d"
                                        opthelp-font-size "medium"))
    (add-option (gnc:make-string-option display-page optname-template-file "e"
                                        opthelp-template-file "balsheet-eg.eguile.scm"))
    (add-option (gnc:make-string-option display-page optname-css-file "f"
                                        opthelp-css-file "balsheet-eg.css"))

    ;; General options
    (add-option (gnc:make-string-option general-page optname-report-title
                                        "a" opthelp-report-title reportname))
    (gnc:options-add-report-date!  options general-page optname-date "b")

    ;; Notes options
    (add-option (gnc:make-text-option notes-page optname-extra-notes
                                      "a" opthelp-extra-notes
                                      (N_ "(Development version -- don't rely on the numbers on this report without double-checking them.<br>Change the 'Extra Notes' option to get rid of this message)")))

    ;; Set the accounts page as default option tab
    (gnc:options-set-default-section options general-page)

    options))

;; Create the report as a chunk of HTML, and return it
;; as a <html-doc> or plain HTML
(define (balsheet-renderer report-obj)

  (define (get-option pagename optname)
    (gnc:option-value
      (gnc:lookup-option
        (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting reportname)
  (let* (
         ;; get all options' values
         (opt-omit-zb-accts?   (get-option accounts-page    optname-omit-zb-accts))
         (opt-use-links?       (get-option accounts-page    optname-account-links))
         (opt-depth-limit      (get-option accounts-page    optname-depth-limit))
         (opt-flatten?         (get-option accounts-page    optname-flatten?))
         (opt-report-commodity (get-option commodities-page optname-report-commodity))
         (opt-price-source     (get-option commodities-page optname-price-source))
         (opt-show-foreign?    (get-option commodities-page optname-show-foreign))
         (opt-report-title     (get-option general-page     optname-report-title))
         (opt-date-tp          (gnc:timepair-end-day-time
                                 (gnc:date-option-absolute-time
                                   (get-option general-page optname-date))))
         (opt-date-secs        (gnc:timepair->secs          opt-date-tp))
         (opt-columns          (get-option display-page     optname-columns))
         (opt-font-family      (get-option display-page     optname-font-family))
         (opt-font-size        (get-option display-page     optname-font-size))
         (opt-template-file    (find-file
                                 (get-option display-page   optname-template-file)))
         (opt-css-file         (find-file
                                 (get-option display-page   optname-css-file)))
         (opt-flatten-depth    999); may get adjusted below
         (opt-neg-format       (get-option display-page     optname-neg-format))
         (opt-extra-notes      (get-option notes-page       optname-extra-notes))

         ;; non-option assignments
         ;;
         (accounts
           (gnc:filter-accountlist-type
             (list ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CREDIT
                   ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY
                   ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL ACCT-TYPE-CURRENCY
                   ACCT-TYPE-PAYABLE ACCT-TYPE-RECEIVABLE
                   ACCT-TYPE-EQUITY ACCT-TYPE-TRADING
                   ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE)
             (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
         ;; decompose the account list
         (split-up-accounts (gnc:decompose-accountlist accounts))
         (asset-accounts
           (assoc-ref split-up-accounts ACCT-TYPE-ASSET))
         (liability-accounts
           (assoc-ref split-up-accounts ACCT-TYPE-LIABILITY))
         (equity-accounts
           (assoc-ref split-up-accounts ACCT-TYPE-EQUITY))
         (trading-accounts
           (assoc-ref split-up-accounts ACCT-TYPE-TRADING))
         (income-expense-accounts
           (append (assoc-ref split-up-accounts ACCT-TYPE-INCOME)
                   (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE)))


         ;; exchange rates calculation parameters
         (exchange-fn
           (gnc:case-exchange-fn opt-price-source opt-report-commodity opt-date-tp))
         ; List of commodities (other than the local one) used
         ; so that exchange rate table can be displayed.
         ; xlist will become an association list of (comm . #t) pairs
         ; to avoid duplicates.
         (xlist '())

         (coyname (or (gnc:company-info gnc:*company-name*) ""))

         (css? (gnc-html-engine-supports-css))

         (html #f))

    ;; end of all the lets.  time for some real code

    ;; The following routines are defined inside
    ;; the renderer to make options available:

    ;; number formatting stuff
    (define (fmtnumber n)
      ;; format double n with as many decimal places as required
      (number->string (if (integer? n) (inexact->exact n) n)))
    (define (fmtnumeric n)
      ;; format gnc-numeric n for printing
      (fmtnumber (gnc-numeric-to-double n)))

    ;; HTML-specific formatting

    (define (negstyle item)
      ;; apply styling for negative amounts
      (string-append "<span class=\"negative\">" item "</span>"))

    (define (foreignstyle item)
      ;; apply styling for amount in foreign currency
      (if css?
        (string-append "<span class=\"foreign\">" item "</span>"))
        (string-append "<small><i>" item "</i></small>"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; accrec-related routines
    ;;;
    ;;; The accrec record stucture (defined above) and the following
    ;;; routines provide the basis for easy access to accounts
    ;;; from the main part of the report

    (define (one-depth-1 accrec)
      ; Return true if the accrec tree contains exactly 1 depth-1 account
      ; (expects a top-level accrec starting at depth 0)
      (and (accrec-sublist accrec)
           (= 1 (length (accrec-sublist accrec)))))

    (define (account-link account)
      ;; Return an HTML link to the given account,
      ;; e.g. <a href="gnc-register:acct-guid=abcdeaalsdfjkalsdk#">Account Name</a>
      (if opt-use-links?
        (string-append
          "<a href=\"gnc-register:acct-guid="
          (gncAccountGetGUID account)
          "\">"
          (xaccAccountGetName account)
          "</a>")
        (xaccAccountGetName account)))

    (define (excluded-acc? accrec)
      ;; Returns true if the account is to be excluded from the report
      ;; NOTE! The balance of excluded accounts will still be added
      ;;       into the report, so this only makes sense for zero-balance accounts
      (and (not (accrec-non-zero? accrec))
           (or
             ;; Reason 0: option to exclude zero-balance accounts (depth > 1 only)
             (and opt-omit-zb-accts?)
                  ;(> (accrec-depth accrec) 1))
             ;; Reason 1: zero Imbalance a/c
             (string-prefix? (_ "Imbalance") (xaccAccountGetName (accrec-account accrec)))
             ;; Reason 2: zero Orphan a/c
             (string-prefix? (_ "Orphan") (xaccAccountGetName (accrec-account accrec))))))

    (define (flattened-acc-depth acc)
      ;; Accounts deeper than required get moved to the requested depth
      (min (gnc-account-get-current-depth acc)
           opt-flatten-depth)) ; this is set to a large value if no flattening required

    (define (process-acc-list account-list neg?)
      ;; non-recursive wrapper around this:
      ;; Convert the account list to a tree structure for easier handling later
      (define (process-acc-list-r
                account-list       ; list of accounts to process
                curr-depth         ; set depth to 1 to start with
                neg?)
        (let ((tree '())          ; gets tree of accounts from this depth down
              (maxdepth 0)        ; gets max depth of all at this level
              (any-non-zero? #f)  ; becomes true if any at this level are non-zero
              (total-cc (gnc:make-commodity-collector)))        ; gets grand total of all accounts
          ; at this level and below
          ; loop until no more accounts, or next account is at higher level
          (while (and (not (null? account-list))
                      (>= (gnc-account-get-current-depth (car account-list)) curr-depth))
                 (let* ((account (car account-list))
                        (comm    (xaccAccountGetCommodity account))
                        (bal     (xaccAccountGetBalanceAsOfDate account opt-date-secs))
                        (depth   (flattened-acc-depth account))
                        (treedepth 1)
                        ; Next account only qualifies as 'deeper' if we're not flattening
                        (next-acc-deeper (and (not (null? (safe-cadr account-list)))
                                              (> (flattened-acc-depth (safe-cadr account-list)) depth)))
                        (newacc (newaccrec-clean)))
                   (accrec-set-account!      newacc account)
                   (accrec-set-code!         newacc (xaccAccountGetCode account))
                   (accrec-set-placeholder?! newacc (xaccAccountGetPlaceholder account))
                   (accrec-set-namelink!     newacc (account-link account))
                   (accrec-set-commodity!    newacc comm) ;(xaccAccountGetCommodity account))
                   (accrec-set-balance-num!  newacc
                                             (if neg?
                                               (gnc-numeric-neg bal)
                                               bal))
                   (accrec-set-depth!        newacc depth) ;(gnc-account-get-current-depth account))
                   (accrec-set-non-zero?!    newacc (not (gnc-numeric-zero-p bal)))

                   (if (>= depth opt-depth-limit)
                     (accrec-set-summary?! newacc #t))
                   (set! xlist (assoc-set! xlist comm #t)) ; even if not opt-show-foreign?

                   (accrec-set-subtotal-cc! newacc (gnc:make-commodity-collector))
                   (add-to-cc total-cc comm bal neg?)
                   (add-to-cc (accrec-subtotal-cc newacc) comm bal neg?)

                   (if next-acc-deeper
                     ; recurse to deal with deeper level accounts,
                     ; then store the resulting list
                     (let* ((result-v (process-acc-list-r
                                        (safe-cdr account-list) (1+ curr-depth) neg?))
                            (subtree (vector-ref result-v 0))
                            (subtotal-cc (vector-ref result-v 2))
                            (subtreedepth (vector-ref result-v 3))
                            (subnonzero?  (vector-ref result-v 4)))
                       (set! account-list (vector-ref result-v 1))
                       (if (not (null? subtree)); (it could be null if all sub-accounts were excluded)
                         (begin
                           ; add the sub-total from the recursion to the current level's total
                           (total-cc 'merge subtotal-cc neg?)
                           ((accrec-subtotal-cc newacc) 'merge subtotal-cc neg?)
                           (if (< curr-depth opt-depth-limit)
                             ; fix the subtree to the current tree
                             ; but only if not beyond the limit
                             (accrec-set-sublist! newacc subtree))
                           ; add the subtree's depth to this level's
                           (set! treedepth (1+ subtreedepth))
                           (if subnonzero?
                             (accrec-set-non-zero?! newacc #t)))))
                     (begin  ; else -- same level -- just pop the account off the list
                       (set! account-list (cdr account-list)))) ; end if next-acc-deeper

                   (if (> treedepth maxdepth)
                     (set! maxdepth treedepth))
                   ;(display " =D=maxdepth=")(ddump maxdepth)
                   (if (not (excluded-acc? newacc))
                     (set! tree (append tree (list newacc))))
                   (if (accrec-non-zero? newacc)
                     (set! any-non-zero? #t))
                   (accrec-set-treedepth!    newacc treedepth)
                   )); end of while
          ; next a/c (if any) is at higher level, so return what's
          ; left of the account list, and the accumulated total
          ;       0    1            2        3        4
          (vector tree account-list total-cc maxdepth any-non-zero?)
          )); end of p-a-l-r
      (let* ((result-v (process-acc-list-r account-list 1 neg?))
             (accrec0  (newaccrec-clean)))  ; accrec for depth 0
        ; Set up top-level 'depth-0' accrec with summary information
        (accrec-set-depth!       accrec0 0)
        (accrec-set-sublist!     accrec0 (vector-ref result-v 0))
        (accrec-set-subtotal-cc! accrec0 (vector-ref result-v 2))
        (accrec-set-treedepth!   accrec0 (min (vector-ref result-v 3) opt-depth-limit))
        ; Return the depth-0 accrec, with all other accounts linked to it
        accrec0))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; gnc-specific routines
    ;;; (if these are already defined elsewhere, I couldn't find them
    ;;;  -- please let me know.  CD)

    (define (gnc-monetary-neg? monetary)
      ; return true if the monetary value is negative
      (gnc-numeric-negative-p (gnc:gnc-monetary-amount monetary)))

    (define (neg-format mny-string neg?)
      ;; Given a monetary string, e.g. £123.00, applying formatting
      ;; for sign depending on option
      ;; And applies the CSS style for negatives too.  Is that right?
      (if neg?
        (if (equal? opt-neg-format 'negbrackets)
          (negstyle (nbsp (string-append "(" mny-string ")")))
          (negstyle (nbsp mny-string)))
        (nbsp mny-string)))

    (define (format-monetary mny)
      ;; Format the given gnc:monetary value according to opt-neg-format
      ;; If mny's currency isn't the same as that of the report,
      ;; convert it -- show both values if specified by option
      (let ((neg? (gnc-monetary-neg? mny))
            (comm (gnc:gnc-monetary-commodity mny))
            (answer ""))
        (if (and neg? (equal? opt-neg-format 'negbrackets))
          ; strip sign from amount -- (neg-format) will replace with brackets
          (set! mny (gnc:monetary-neg mny)))
        (if (not (gnc-commodity-equiv comm opt-report-commodity))
          (begin
            (if opt-show-foreign?
              (set! answer (string-append (foreignstyle (neg-format (gnc:monetary->string mny) neg?)) "&nbsp;")))
            (set! mny (exchange-fn mny opt-report-commodity))))
        ; main currency - converted if necessary
        (set! answer (string-append answer (neg-format (gnc:monetary->string mny) neg?)))
        answer))

    (define (format-comm-coll cc)
      ;; Format a commodity collector for display in the report.
      ;; Returns one commodity per line.
      (string-concatenate
        (map-in-order
          (lambda (mny)
            (string-append (format-monetary mny) "<br>"))
          (cc 'format gnc:make-gnc-monetary #f))))

    (define (format-comm-coll-total cc)
      ;; Format the total value of a commodity collector
      (format-monetary (gnc:sum-collector-commodity cc opt-report-commodity exchange-fn)))

    (define (fmtmoney2 mny)
      ;; format a monetary amount in the given currency/commodity
      ;; !! this takes a gnc-monetary
      (nbsp (gnc:monetary->string mny)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Adjust options for convenience
    (if (equal? opt-depth-limit 'all)
      (set! opt-depth-limit 999)) ; for easier comparisons
    (if opt-flatten?
      (set! opt-flatten-depth opt-depth-limit))

    ;; Run eguile to process the template
    (set! html (eguile-file-to-string opt-template-file (the-environment)))
    (gnc:debug "balsheet-eg.scm - generated html:") (gnc:debug html)
    (gnc:report-finished)
    (if css? ; return report as document or html, depending on version
      html
      (let ((document (gnc:make-html-document)))
        (gnc:html-document-add-object! document html)
        document))

    ))

(gnc:define-report
  'version 1
  'name reportname
  'report-guid "2e3751edeb7544e8a20fd19e9d08bb65"
  'menu-name (N_ "Balance Sheet using eguile-gnc")
  'menu-tip (N_ "Display a balance sheet (using eguile template)")
  'menu-path (list gnc:menuname-asset-liability)
  'options-generator balsheet-options-generator
  'renderer balsheet-renderer)

