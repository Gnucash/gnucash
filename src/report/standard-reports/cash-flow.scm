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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report cash-flow))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc:html-build-url

;; first define all option's names so that they are properly defined
;; in *one* place.
(define optname-from-date (N_ "From"))
(define optname-to-date (N_ "To"))

(define optname-display-depth (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-rates (N_ "Show Exchange Rates"))

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
     optname-price-source "c" 'weighted-average)

    (gnc:register-option 
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-rates
      "d" (N_ "Show the exchange rates used") #t))

    ;; accounts to work on
    (gnc:options-add-account-selection! 
     options gnc:pagename-accounts
     optname-display-depth optname-show-subaccounts
     optname-accounts "a" 2
     (lambda ()
       (gnc:filter-accountlist-type 
        '(bank cash credit asset liability stock mutual-fund currency
               payable receivable equity)
        (gnc:group-get-subaccounts (gnc:get-current-group)))))
    
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

  ;; get all option's values
  (let* ((accounts (get-option gnc:pagename-accounts
                               optname-accounts))	 
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
         (show-rates? (get-option gnc:pagename-general 
                                  optname-show-rates))
         (from-date-tp (gnc:timepair-start-day-time 
                        (gnc:date-option-absolute-time
                         (get-option gnc:pagename-general
                                     optname-from-date))))
         (to-date-tp (gnc:timepair-end-day-time 
                      (gnc:date-option-absolute-time
                       (get-option gnc:pagename-general
                                   optname-to-date))))

         ;; calculate the exchange rates
         (exchange-fn (gnc:case-exchange-fn
                       price-source report-currency to-date-tp))

         (doc (gnc:make-html-document))
         (table (gnc:make-html-table))
         (txt (gnc:make-html-text)))

    ;; is account in list of accounts?
    (define (same-account? a1 a2)
      (string=? (gnc:account-get-guid a1) (gnc:account-get-guid a2)))

    (define account-in-list?
      (lambda (account accounts)
        (cond
          ((null? accounts) #f)
          ((same-account? (car accounts) account) #t)
          (else (account-in-list? account (cdr accounts))))))

    (define account-in-alist
      (lambda (account alist)
        (cond
           ((null? alist) #f)
           ((same-account? (caar alist) account) (car alist))
           (else (account-in-alist account (cdr alist))))))

    

    (gnc:html-document-set-title! 
     doc (sprintf #f "%s - %s to %s for"
		  (get-option gnc:pagename-general gnc:optname-reportname)
                  (gnc:print-date from-date-tp) (gnc:print-date to-date-tp)))

    (if (not (null? accounts))

        (let* ((money-in-accounts '())
               (money-in-alist '())
               (money-in-collector (gnc:make-commodity-collector))

               (money-out-accounts '())
               (money-out-alist '())
               (money-out-collector (gnc:make-commodity-collector))

               (money-diff-collector (gnc:make-commodity-collector)))

          ;; function to add inflow and outflow of money
          (define (calc-money-in-out accounts)

            (define (calc-money-in-out-internal accounts-internal)
              (if (not (null? accounts-internal))
                (let* ((current (car accounts-internal))
                       (rest (cdr accounts-internal))
                       (name (gnc:account-get-name current))
                       (curr-commodity (gnc:account-get-commodity current))
                      )

                      ;(gnc:debug "---" name "---")

                      (for-each
                        (lambda (split)
                          (let ((parent (gnc:split-get-parent split)))
                            (if (and (gnc:timepair-le (gnc:transaction-get-date-posted parent) to-date-tp)
                                     (gnc:timepair-ge (gnc:transaction-get-date-posted parent) from-date-tp))
                              (for-each
                                (lambda (s)
                                  (let* ((s-account (gnc:split-get-account s))
                                         (s-amount (gnc:split-get-amount s))
                                         (s-value (gnc:split-get-value s))
                                         (s-commodity (gnc:account-get-commodity s-account)))
                                    ;(gnc:debug (gnc:account-get-name s-account))
                                    (if (not (account-in-list? s-account accounts))
                                      (if (gnc:numeric-negative-p (gnc:split-get-value s))
                                          (let ((pair (account-in-alist s-account money-in-alist)))
                                            ;(gnc:debug "in:" (gnc:commodity-get-printname s-commodity)
                                            ;                 (gnc:numeric-to-double s-amount) 
                                            ;                 (gnc:commodity-get-printname curr-commodity)
                                            ;                 (gnc:numeric-to-double s-value))
                                            (money-in-collector 'add curr-commodity (gnc:numeric-neg s-value))
                                            (if (not pair)
                                                (begin
                                                  (set! pair (list s-account (gnc:make-commodity-collector)))
                                                  (set! money-in-alist (cons pair money-in-alist))
                                                  (set! money-in-accounts (cons s-account money-in-accounts))
                                                  ;(gnc:debug money-in-alist)
                                                )
                                            )
                                            (let ((s-account-in-collector (cadr pair)))
                                              (s-account-in-collector 'add curr-commodity (gnc:numeric-neg s-value)))
                                          )
                                          (let ((pair (account-in-alist s-account money-out-alist)))
                                            ;(gnc:debug "out:" (gnc:commodity-get-printname s-commodity)
                                            ;                  (gnc:numeric-to-double s-amount) 
                                            ;                  (gnc:commodity-get-printname curr-commodity)
                                            ;                  (gnc:numeric-to-double s-value))
                                            (money-out-collector 'add curr-commodity s-value)
                                            (if (not pair)
                                                (begin
                                                  (set! pair (list s-account (gnc:make-commodity-collector)))
                                                  (set! money-out-alist (cons pair money-out-alist))
                                                  (set! money-out-accounts (cons s-account money-out-accounts))
                                                  ;(gnc:debug money-out-alist)
                                                )
                                            )
                                            (let ((s-account-out-collector (cadr pair)))
                                              (s-account-out-collector 'add curr-commodity s-value))
                                          )
                                      )
                                    )
                                  )
                                )
                                (gnc:transaction-get-splits parent)
                              )
                            )
                          )
                        )
                        (gnc:account-get-split-list current)
                      )

                      (calc-money-in-out-internal rest))))

            (calc-money-in-out-internal accounts))


          (calc-money-in-out accounts)

          (money-diff-collector 'merge money-in-collector #f)
          (money-diff-collector 'minusmerge money-out-collector #f)


          (gnc:html-document-add-object!
           doc
           (gnc:make-html-text
            (gnc:html-markup-ul
             (map 
              (lambda (acct)
                (gnc:html-markup-anchor 
                 (gnc:html-build-url gnc:url-type-register
                                      (string-append "account=" 
                                                     (gnc:account-get-full-name
                                                      acct))
                                       #f)
                  (gnc:account-get-name acct)))
               accounts))))

          (gnc:html-table-append-ruler! table 2)

          (gnc:html-table-append-row/markup!
           table
           "primary-subheading"
           (list
             (_ "Money In")
             ""))

          (for-each
            (lambda (pair)
              (gnc:html-table-append-row/markup!
               table
               "normal-row"
               (list
                (gnc:html-account-anchor (car pair))
                (gnc:make-html-table-header-cell/markup
                 "number-cell" (gnc:sum-collector-commodity (cadr pair) report-currency exchange-fn))))
            )
            money-in-alist
          )

          (gnc:html-table-append-row/markup!
           table
           "grand-total"
           (list
             (_ "Money In")
             (gnc:make-html-table-header-cell/markup
              "total-number-cell" (gnc:sum-collector-commodity money-in-collector report-currency exchange-fn))))

          (gnc:html-table-append-ruler! table 2)

          (gnc:html-table-append-row/markup!
           table
           "primary-subheading"
           (list
             (_ "Money Out")
             ""))

          (for-each
            (lambda (pair)
              (gnc:html-table-append-row/markup!
               table
               "normal-row"
               (list
                (gnc:html-account-anchor (car pair))
                (gnc:make-html-table-header-cell/markup
                 "number-cell" (gnc:sum-collector-commodity (cadr pair) report-currency exchange-fn))))
            )
            money-out-alist
          )

          (gnc:html-table-append-row/markup!
           table
           "grand-total"
           (list
             (_ "Money Out")
             (gnc:make-html-table-header-cell/markup
              "total-number-cell" (gnc:sum-collector-commodity money-out-collector report-currency exchange-fn))))

          (gnc:html-table-append-ruler! table 2)

          (gnc:html-table-append-row/markup!
           table
           "grand-total"
           (list
             (_ "Difference")
             (gnc:make-html-table-header-cell/markup
              "total-number-cell" (gnc:sum-collector-commodity money-diff-collector report-currency exchange-fn))))

          (gnc:html-document-add-object! doc table)


          ;; add currency information
          (if show-rates?
              (gnc:html-document-add-object! 
               doc ;;(gnc:html-markup-p
               (gnc:html-make-exchangerates 
                report-currency exchange-fn accounts))))

        
        
        ;; error condition: no accounts specified
        
        (gnc:html-document-add-object! 
         doc 
         (gnc:html-make-no-account-warning 
	  (_ "Cash Flow") (gnc:report-id report-obj))))
    doc))

(gnc:define-report 
 'version 1
 'name (N_ "Cash Flow")
 'menu-path (list gnc:menuname-income-expense)
 'options-generator cash-flow-options-generator
 'renderer cash-flow-renderer)
