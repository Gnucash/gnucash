;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cash-flow.scm: cash flow report 
;; 
;; By Herbert Thoma <herbie@hthoma.de>
;;
;; based on balance-sheet.scm by:
;; Robert Merkel <rgmerk@mira.net>
;; and pnl.scm by:
;; Christian Stimming <stimming@tu-harburg.de>
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

(define-module (gnucash report standard-reports cash-flow))
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (gnucash engine))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(export cash-flow-calc-money-in-out)

(define reportname (N_ "Cash Flow"))

;; define all option's names so that they are properly defined
;; in *one* place.
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))

(define optname-display-depth (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define optname-show-full-names (N_ "Show Full Account Names"))
(define optname-include-trading-accounts (N_ "Include Trading Accounts in report"))

;; options generator
(define (cash-flow-options-generator)
  (let ((options (gnc:new-options)))

    ;; date interval
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "a")

    ;; all about currencies
    (gnc:options-add-currency!
     options gnc:pagename-general
     optname-report-currency "b")

    (gnc:options-add-price-source!
     options gnc:pagename-general
     optname-price-source "c" 'pricedb-nearest)

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-rates
      "d" (N_ "Show the exchange rates used.") #f))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-full-names
      "e" (N_ "Show full account names (including parent accounts).") #t))

    ;; accounts to work on
    (gnc:options-add-account-selection!
     options gnc:pagename-accounts
     optname-display-depth optname-show-subaccounts
     optname-accounts "a" 2
     (lambda ()
       (gnc:filter-accountlist-type
        (list ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-ASSET
              ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)
        (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
     #f)

    ;; Trading accounts?
    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-include-trading-accounts
      "b" (N_ "Include transfers to and from Trading Accounts in the report.")  #f))

    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)

    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cash-flow-renderer
;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cash-flow-renderer report-obj)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting reportname)

  ;; get all option's values
  (let* ((display-depth (get-option gnc:pagename-accounts
                                    optname-display-depth))
         (show-subaccts? (get-option gnc:pagename-accounts
                                     optname-show-subaccounts))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))
         (include-trading-accounts (get-option gnc:pagename-accounts
                                               optname-include-trading-accounts))
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
         (show-rates? (get-option gnc:pagename-general
                                  optname-show-rates))
         (show-full-names? (get-option gnc:pagename-general
                                       optname-show-full-names))
         (from-date-t64 (gnc:time64-start-day-time
                         (gnc:date-option-absolute-time
                          (get-option gnc:pagename-general
                                      optname-from-date))))
         (to-date-t64 (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general
                                    optname-to-date))))

         ;; calculate the exchange rates
         (exchange-fn (gnc:case-exchange-fn
                       price-source report-currency to-date-t64))

         (doc (gnc:make-html-document))
         (table (gnc:make-html-table))

         ;;add subaccounts if requested
         (accounts (if show-subaccts?
                       (gnc:accounts-and-all-descendants accounts)
                       accounts))
         (accounts (sort accounts account-full-name<?)))

    (define (add-accounts-flow accounts accounts-alist)
      (let loop ((accounts accounts)
                 (odd-row? #t))
        (unless (null? accounts)
          (let* ((pair (assoc (car accounts) accounts-alist))
                 (acct (car pair)))
            (gnc:html-table-append-row/markup!
             table
             (if odd-row? "normal-row" "alternate-row")
             (list
              (gnc:make-html-text
               (gnc:html-markup-anchor
                (gnc:account-anchor-text acct)
                (if show-full-names?
                    (gnc-account-get-full-name acct)
                    (xaccAccountGetName acct))))
              (gnc:make-html-table-header-cell/markup
               "number-cell"
               (gnc:sum-collector-commodity
                (cadr pair) report-currency exchange-fn)))))
          (loop (cdr accounts)
                (not odd-row?)))))

    (gnc:html-document-set-title!
     doc (string-append
          (get-option gnc:pagename-general gnc:optname-reportname)
          " - "
          (format #f (_ "~a to ~a")
                  (qof-print-date from-date-t64) (qof-print-date to-date-t64))))

    (if (not (null? accounts))

        (let* ((tree-depth (if (equal? display-depth 'all)
                               (accounts-get-children-depth accounts)
                               display-depth))
               (account-disp-list
                (map
                 (lambda (account)
                   (gnc:html-markup/format
                    (if (and (= (gnc-account-get-current-depth account) tree-depth)
                             (pair? (gnc-account-get-children account)))
                        (if show-subaccts?
                            (_ "~a and subaccounts")
                            (_ "~a and selected subaccounts"))
                        "~a")
                    (gnc:html-markup-anchor
                     (gnc:account-anchor-text account)
                     (if show-full-names?
                         (gnc-account-get-full-name account)
                         (xaccAccountGetName account)))))
                 (filter
                  (lambda (account)
                    (<= (gnc-account-get-current-depth account) tree-depth))
                  accounts)))

               (commodity-list (gnc:accounts-get-commodities
                                accounts
                                report-currency))
               ;; Get an exchange function that will convert each transaction using the
               ;; nearest available exchange rate if that is what is specified
               (time-exchange-fn (gnc:case-exchange-time-fn
                                  price-source report-currency
                                  commodity-list to-date-t64
                                  0 0)))

          ;; Helper function to convert currencies
          (define (to-report-currency currency amount date)
            (gnc:gnc-monetary-amount
             (time-exchange-fn (gnc:make-gnc-monetary currency amount)
                               report-currency
                               date)))


          (let ((result (cash-flow-calc-money-in-out
                         (list (cons 'accounts accounts)
                               (cons 'to-date-t64 to-date-t64)
                               (cons 'from-date-t64 from-date-t64)
                               (cons 'report-currency report-currency)
                               (cons 'include-trading-accounts include-trading-accounts)
                               (cons 'to-report-currency to-report-currency)))))
            (let ((money-in-accounts (sort
                                      (cdr (assq 'money-in-accounts result))
                                      account-full-name<?))
                  (money-in-alist (cdr (assq 'money-in-alist result)))
                  (money-in-collector (cdr (assq 'money-in-collector result)))
                  (money-out-accounts (sort
                                       (cdr (assq 'money-out-accounts result))
                                       account-full-name<?))
                  (money-out-alist (cdr (assq 'money-out-alist result)))
                  (money-out-collector (cdr (assq 'money-out-collector result))))

              (gnc:html-document-add-object!
               doc
               (gnc:make-html-text (_ "Selected Accounts")))

              (gnc:html-document-add-object!
               doc
               (gnc:make-html-text
                (gnc:html-markup-ul
                 account-disp-list)))

              (gnc:html-table-append-ruler! table 2)

              (gnc:html-table-append-row/markup!
               table
               "primary-subheading"
               (list
                (_ "Money into selected accounts comes from")
                ""))

              (add-accounts-flow money-in-accounts money-in-alist)

              (gnc:html-table-append-row/markup!
               table
               "grand-total"
               (list
                (gnc:make-html-table-header-cell/markup "text-cell" (_ "Money In"))
                (gnc:make-html-table-header-cell/markup
                 "total-number-cell"
                 (gnc:sum-collector-commodity
                  money-in-collector report-currency exchange-fn))))

              (gnc:html-table-append-ruler! table 2)

              (gnc:html-table-append-row/markup!
               table
               "primary-subheading"
               (list
                (_ "Money out of selected accounts goes to")
                ""))

              (add-accounts-flow money-out-accounts money-out-alist)

              (gnc:html-table-append-row/markup!
               table
               "grand-total"
               (list
                (gnc:make-html-table-header-cell/markup "text-cell" (_ "Money Out"))
                (gnc:make-html-table-header-cell/markup
                 "total-number-cell"
                 (gnc:sum-collector-commodity
                  money-out-collector report-currency exchange-fn))))

              (gnc:html-table-append-ruler! table 2)

              (gnc:html-table-append-row/markup!
               table
               "grand-total"
               (list
                (gnc:make-html-table-header-cell/markup "text-cell" (_ "Difference"))
                (gnc:make-html-table-header-cell/markup
                 "total-number-cell"
                 (gnc:sum-collector-commodity
                  (gnc:collector- money-in-collector money-out-collector)
                  report-currency exchange-fn))))

              (gnc:html-document-add-object! doc table)


              ;; add currency information
              (if show-rates?
                  (gnc:html-document-add-object!
                   doc ;;(gnc:html-markup-p
                   (gnc:html-make-exchangerates
                    report-currency exchange-fn accounts))))))

        ;; error condition: no accounts specified

        (gnc:html-document-add-object!
         doc
         (gnc:html-make-no-account-warning
          reportname (gnc:report-id report-obj))))

    (gnc:report-finished)
    doc))


;; function to add inflow and outflow of money
(define (cash-flow-calc-money-in-out settings)
  (let* ((accounts (cdr (assq 'accounts settings)))
         (to-date-t64 (cdr (assq 'to-date-t64 settings)))
         (from-date-t64 (cdr (assq 'from-date-t64 settings)))
         (report-currency (cdr (assq 'report-currency settings)))
         (include-trading-accounts
          (cdr (assq 'include-trading-accounts settings)))
         (to-report-currency (cdr (assq 'to-report-currency settings)))
         (money-in '())
         (money-in-collector (gnc:make-commodity-collector))
         (money-out '())
         (money-out-collector (gnc:make-commodity-collector))
         (all-splits (gnc:account-get-trans-type-splits-interval
                      accounts '() from-date-t64 to-date-t64))
         (splits-to-do (length all-splits))
         (splits-seen-list '()))

    (let loop ((splits all-splits)
               (work-done 0))
      (unless (null? splits)
        (if (zero? (modulo work-done 100))
            (gnc:report-percent-done (* 85 (/ work-done splits-to-do))))
        (let* ((split (car splits))
               (parent (xaccSplitGetParent split)))
          (for-each
           (lambda (s)
             (let* ((s-account (xaccSplitGetAccount s))
                    (s-value (xaccSplitGetValue s))
                    (s-report-value (to-report-currency (xaccTransGetCurrency parent)
                                                        (abs s-value)
                                                        (xaccTransGetDate parent))))
               (cond
                ((null? s-account)
                 (format #t "WARNING: s-account is NULL for split: ~a\n"
                         (gncSplitGetGUID s)))
                ((or (and include-trading-accounts
                          (eqv? (xaccAccountGetType s-account)
                                ACCT-TYPE-TRADING))
                     (member s-account accounts)
                     (member s splits-seen-list))
                 #f)
                ((negative? s-value)
                 (let ((s-account-in-collector
                        (or (assoc-ref money-in s-account)
                            (let ((coll (gnc:make-commodity-collector)))
                              (set! money-in
                                (assoc-set! money-in s-account coll))
                              coll))))
                   (set! splits-seen-list (cons s splits-seen-list))
                   (money-in-collector 'add report-currency s-report-value)
                   (s-account-in-collector
                    'add report-currency s-report-value)))
                ((positive? s-value)
                 (let ((s-account-out-collector
                        (or (assoc-ref money-out s-account)
                            (let ((coll (gnc:make-commodity-collector)))
                              (set! money-out
                                (assoc-set! money-out s-account coll))
                              coll))))
                   (set! splits-seen-list (cons s splits-seen-list))
                   (money-out-collector 'add report-currency s-report-value)
                   (s-account-out-collector
                    'add report-currency s-report-value))))))
           (xaccTransGetSplitList parent)))
        (loop (cdr splits) (1+ work-done))))

    ;; Return an association list of results
    (list
     (cons 'money-in-accounts (map car money-in))
     (cons 'money-in-alist (map (lambda (p) (list (car p) (cdr p))) money-in))
     (cons 'money-in-collector money-in-collector)
     (cons 'money-out-accounts (map car money-out))
     (cons 'money-out-alist (map (lambda (p) (list (car p) (cdr p))) money-out))
     (cons 'money-out-collector money-out-collector))))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "f8748b813fab4220ba26e743aedf38da"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator cash-flow-options-generator
 'renderer cash-flow-renderer)
