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

;; Balance and Profit/Loss Reports
;; 
;; A lot of currency handling extensions by 
;; Christian Stimming <stimming@uclink.berkeley.edu> on 10/09/2000.

(gnc:support "report/balance-and-pnl.scm")
(gnc:depend "html-generator.scm")
(gnc:depend "text-export.scm")
(gnc:depend "report-utilities.scm")
(gnc:depend "options.scm")
(gnc:depend "currencies.scm")


;; Just a private scope.
(let 
    ((l0-collector (make-currency-collector))
     (l1-collector (make-currency-collector))
     (l2-collector (make-currency-collector))
     (default-exchange-rate 0) ;; if there is no user-specified exchange rate
     (currency-pref-options 
      '(("Currency 1" "USD")
	("Currency 2" "EUR")
	("Currency 3" "DEM")
	("Currency 4" "GBP")
	("Currency 5" "FRF")))
     (currency-option-value-prefix "Exchange rate for "))
  
  (define string-db (gnc:make-string-database))

  (define (register-common-options option-registerer)
    (begin 
      (option-registerer
       (gnc:make-date-option
	"Report" "To"
	"a" "Calculate balance sheet up to this date"
	(lambda ()
	  (let ((bdtime (localtime (current-time))))
	    (set-tm:sec bdtime 59)
	    (set-tm:min bdtime 59)
	    (set-tm:hour bdtime 23)
	    (let ((time (car (mktime bdtime))))
	      (cons 'absolute (cons time 0)))))
	#f 'absolute #f))

;; doesn't seem to work -- see at the very main loop
;      ;; accounts to do report on
;      (option-registerer
;       (gnc:make-account-list-option
;	"Report" "Account"
;	"c" "Do the report on these accounts"
;        (lambda ()
;          (let ((current-accounts (gnc:get-current-accounts))
;                (num-accounts
;                 (gnc:group-get-num-accounts (gnc:get-current-group))))
;            (cond ((not (null? current-accounts)) current-accounts)
;                  (else
;                   (let ((acctlist '()))
;                     (gnc:for-loop
;                      (lambda(x)
;                        (set! acctlist
;                              (append!
;                               acctlist
;                               (list (gnc:group-get-account
;                                      (gnc:get-current-group) x)))))
;                      0 num-accounts 1)
;                     acctlist)))))	
;	#f #t))
      
      (option-registerer
       (gnc:make-simple-boolean-option
	"Display" "Type"
	"b" "Display the account type?" #t))

;      (option-registerer
;       (gnc:make-simple-boolean-option
;	"Display" "Num"
;	"b" "Display the account number?" #t))

      (option-registerer
       (gnc:make-simple-boolean-option
	"Display" "Foreign Currency"
	"b" "Display the account's foreign currency amount?" #t))

      (option-registerer
       (gnc:make-currency-option 
	"Currencies" "Report's currency" 
	"AA" "All other currencies will get converted to this currency."
	(gnc:locale-default-currency)))
      
      (option-registerer
       (gnc:make-simple-boolean-option
	"Currencies" "Other currencies' total"
	"AB" "Show the total amount of other currencies?" #f))
      
      (for-each 
       (lambda(x)(begin (option-registerer
			 (gnc:make-currency-option 
			  "Currencies" (car x) 
			  (string-append (car x) "a") 
			  "Choose foreign currency to specify an exchange rate for"
			  (cadr x)))
			(option-registerer
			 (gnc:make-string-option
			  "Currencies" 
			  (string-append currency-option-value-prefix (car x)) 
			  (string-append (car x) "b") 
			  "Choose exchange rate for above currency"
			  (number->string default-exchange-rate)))))
       currency-pref-options)))

  (define (balsht-options-generator)
    (define gnc:*balsht-report-options* (gnc:new-options))
    (define (gnc:register-balsht-option new-option)
      (gnc:register-option gnc:*balsht-report-options* new-option)) 

    ;; The lazy way :-] Common options for both reports in one.
    (register-common-options gnc:register-balsht-option)
    
    (gnc:options-set-default-section gnc:*balsht-report-options*
				     "Report")

    gnc:*balsht-report-options*)

  (define  (pnl-options-generator)
    (define gnc:*pnl-report-options* (gnc:new-options))
    (define (gnc:register-pnl-option new-option)
      (gnc:register-option gnc:*pnl-report-options* new-option))

    (gnc:register-pnl-option
     (gnc:make-date-option
      "Report" "From"
      "a" "Start of reporting period"
      (lambda ()
        (let ((bdtime (localtime (current-time))))
          (set-tm:sec bdtime 0)
          (set-tm:min bdtime 0)
          (set-tm:hour bdtime 0)
          (set-tm:mday bdtime 1)
          (set-tm:mon bdtime 0)
          (let ((time (car (mktime bdtime))))
            (cons 'absolute (cons time 0)))))
      #f 'absolute #f)) 

    (register-common-options gnc:register-pnl-option)

    (gnc:options-set-default-section gnc:*pnl-report-options*
				     "Report")

    gnc:*pnl-report-options*)

  (define (render-level-2-account 
	   level-2-account l2-currency-collector 
	   balance-currency exchange-alist row-aligner)
    (let ((account-name (string-append NBSP NBSP NBSP NBSP 
                                       (gnc:account-get-full-name
                                        level-2-account)))
          (type-name (gnc:account-get-type-string
                      (gnc:account-get-type level-2-account))))
      (l2-currency-collector 
       'format
       (lambda (currency value)
	 (let ((tacc account-name)
	       (ttype type-name))
	   (set! account-name "")
	   (set! type-name "")
	   (row-aligner
	    (append
	     (list tacc ttype)
	     (if (equal? currency balance-currency)
		 (list NBSP
		       (gnc:amount->formatted-currency-string
			value balance-currency #f))
		 (list (gnc:amount->formatted-currency-string
			value currency #f) 
		       (gnc:amount->formatted-currency-string
			(* value
			   (let ((pair (assoc currency exchange-alist)))
			     (if (not pair) default-exchange-rate (cadr pair))))
			balance-currency #f)))
	     (list NBSP NBSP)))))
       #f)))
  
  (define (render-level-1-account 
	   l1-account l1-currency-collector 
	   balance-currency exchange-alist row-aligner)
    (let ((account-name (gnc:account-get-full-name l1-account))
          (type-name (gnc:account-get-type-string
                      (gnc:account-get-type l1-account))))
      (l1-currency-collector 
       'format
       (lambda (currency value)
	 (let ((tacc account-name)
	       (ttype type-name))
	   (set! account-name "")
	   (set! type-name "")
	   (row-aligner
	    (append 
	     (list tacc ttype NBSP NBSP) 
	     (if (equal? currency balance-currency)
		 (list NBSP 
		       (gnc:amount->formatted-currency-string
			value balance-currency #f))
		 (list (gnc:amount->formatted-currency-string
			value currency #f)
		       (gnc:amount->formatted-currency-string
			(* value
			   (let ((pair (assoc currency exchange-alist)))
			     (if (not pair) default-exchange-rate (cadr pair))))
			balance-currency #f)))))))
       #f)))
  
  (define (render-total l0-currency-collector 
			balance-currency exchange-alist 
			other-currency-total? show-fcur?
			row-aligner)
    (let ((account-name (string-html-strong (string-db 'lookup 'net)))
	  (exchanged-total 0))
      (append
       (l0-currency-collector 
	'format
	(lambda (currency value)
	  (if (equal? currency balance-currency)
	      (begin 
		(set! exchanged-total (+ exchanged-total value))
		'())
	      (begin 
		(set! exchanged-total 
		      (+ exchanged-total 
			 (* value
			    (let ((pair (assoc currency exchange-alist)))
			      (if (not pair) default-exchange-rate 
				  (cadr pair))))))
		(if (and other-currency-total? show-fcur?)
		    (row-aligner
		     (list 
		      NBSP NBSP NBSP NBSP
		      (gnc:amount->formatted-currency-string
		       value currency #f)
		      NBSP))
		    '()))))
	#f)
       (row-aligner
	(list account-name NBSP NBSP NBSP NBSP 
	      (gnc:amount->formatted-currency-string
	       exchanged-total balance-currency #f))))))
  
  (define blank-line
    (html-table-row '()))

  (define (is-it-on-balance-sheet? type balance?)
    (eq? 
     (not (member type '(INCOME EXPENSE)))
     (not balance?)))

  (define (generate-balance-sheet-or-pnl report-name
					 report-description
					 options
					 balance-sheet?)

    (let* ((from-option (gnc:lookup-option options "Report" "From"))
           (from-value (if from-option (gnc:date-option-absolute-time (gnc:option-value from-option)) #f))
           (to-value (gnc:timepair-end-day-time
                      (gnc:date-option-absolute-time (gnc:option-value
                       (gnc:lookup-option options "Report" "To")))))
	   (report-currency
	    (gnc:option-value 
	     (gnc:lookup-option options "Currencies" 
				"Report's currency")))
	   (show-currency-total? 
	    (gnc:option-value
	     (gnc:lookup-option options "Currencies" 
				"Other currencies' total")))
	   (exchange-alist
	    (map
	     (lambda(x)
	       (list 
		(gnc:option-value 
		 (gnc:lookup-option options "Currencies" (car x)))
		(let ((y (string->number 
			  (gnc:option-value
			   (gnc:lookup-option 
			    options "Currencies" 
			    (string-append 
			     currency-option-value-prefix (car x)))))))
		  (if (not y) 0 y))))
	     currency-pref-options))
;	   (accounts (gnc:option-value 
;		      (gnc:lookup-option options "Report" "Account")))
	   (show-type? (gnc:option-value
			(gnc:lookup-option options "Display" "Type")))
	   (show-fcur? (gnc:option-value
			(gnc:lookup-option options "Display" 
					   "Foreign Currency")))
	   (report-row-align (lambda(x)       
			       (html-table-row-align 
				(append 
				 (list (car x))
				 (if show-type? (list (cadr x)) '()) 
				 (if show-fcur? (list (caddr x)) '())
				 (list (cadddr x))
				 (if show-fcur? (list (cadddr (cdr x))) '())
				 (list (cadddr (cddr x))))
				(append '("left") 
					(if show-type? '("center") '())
					(if show-fcur? '("right") '())
					'("right")
					(if show-fcur? '("right") '())
					'("right"))))))
    
      (define (handle-level-1-account account options)
	(let ((type (gnc:account-type->symbol (gnc:account-get-type account))))
          (if (is-it-on-balance-sheet? type balance-sheet?)
              ;; Ignore
              '()
              (let* ((children (gnc:account-get-children account))
                     (num-children (gnc:group-get-num-accounts children))

                     (childrens-output (gnc:group-map-accounts
                                        (lambda (x)
                                          (handle-level-2-account x options))
                                        children))

                     (account-balance (if balance-sheet?
                                          (gnc:account-get-balance-at-date
                                           account
                                           to-value #f)
                                          (gnc:account-get-balance-interval
                                           account
                                           from-value
                                           to-value #f))))

                (if (not balance-sheet?)
                    (set! account-balance (- account-balance)))
                (l1-collector 'add (gnc:account-get-currency account)
				   account-balance)
                (l1-collector 'merge l2-collector #f)
                (l0-collector 'merge l1-collector #f)
                (let ((level-1-output
                       (render-level-1-account 
			account l1-collector report-currency 
			exchange-alist report-row-align)))
                  (l1-collector 'reset #f #f)
                  (l2-collector 'reset #f #f)
                  (if (null? childrens-output)
                      level-1-output
                      (list blank-line
                            level-1-output
                            childrens-output
                            blank-line)))))))

    (define (handle-level-2-account account options)
      (let
	  ((type (gnc:account-type->symbol (gnc:account-get-type account)))
	   (balance (make-currency-collector))
	   (rawbal
	    (if balance-sheet?
		(gnc:account-get-balance-at-date account to-value #f)
		(gnc:account-get-balance-interval 
		 account 
		 from-value
		 to-value #f))))
	(balance 'add 
                 (gnc:account-get-currency account)
		 (if balance-sheet? 
		     rawbal
		     (- rawbal)))
	(if (is-it-on-balance-sheet? type balance-sheet?)
	    ;; Ignore
	    '()
	    ;; add in balances for any sub-sub groups
	    (let ((grandchildren (gnc:account-get-children account)))
	      (if (not (pointer-token-null? grandchildren))
		  (balance (if balance-sheet? 'merge 'minusmerge) 
			   (if balance-sheet? 
			       (gnc:group-get-curr-balance-at-date grandchildren 
								   to-value)
			       (gnc:group-get-curr-balance-interval grandchildren
								    from-value
								    to-value))
			   #f))
	      (l2-collector 'merge balance #f)
              (render-level-2-account 
	       account balance report-currency exchange-alist
	       report-row-align)))))

    (let
	((current-group (gnc:get-current-group))
	 (output '()))

      ;; Now, the main body
      ;; Reset all the balance collectors
      (l0-collector 'reset #f #f)
      (l1-collector 'reset #f #f)
      (l2-collector 'reset #f #f)
      (set! report-name 
	    (if from-option 
		(list report-name " "
		      (strftime "%x" (localtime (car from-value))) 
		      " to " 
		      (strftime "%x" (localtime (car to-value))))
		(list report-name " "
		      (strftime "%x" (localtime (car to-value))))))
      
      (if (not (pointer-token-null? current-group))
	  (set! output
		(list
		 (gnc:group-map-accounts
;		 (map
		  (lambda (x) (handle-level-1-account x options))
;		  accounts)
;; obviously you can't just replace this "current-group" by 
;; the "accounts" list. Which is a pity. -- Christian
		  current-group)
		 (render-total l0-collector report-currency 
			       exchange-alist show-currency-total? show-fcur?
			       report-row-align))))

      (list
       "<html>"
       "<head>"
       "<title>" report-name "</title>"
       "</head>"

       (if balance-sheet?
           "<body bgcolor=#fffde6>"
           "<body bgcolor=#f6ffdb>")

       "<table cellpadding=1>"
       "<caption><b>" report-name "</b></caption>"
       "<tr>"
       "<th>" (string-db 'lookup 'account-name) "</th>"
       (if show-type? (string-append "<th align=center>" 
				     (string-db 'lookup 'type)  "</th>")
	   "")
       "<th "
       (if show-fcur? "colspan=2 " "")
       "align=right>" (string-db 'lookup 'subaccounts) "</th>"
       (if show-fcur? "<th></th>" "")
       "<th align=right>" (string-db 'lookup 'balance) "</th>"
       "</tr>"

       output

       "</table>"
       "</body>"
       "</html>"))))

  (string-db 'store 'net "Net")
  (string-db 'store 'type "Type")
  (string-db 'store 'account-name "Account Name")
  (string-db 'store 'subaccounts "(subaccounts)")
  (string-db 'store 'balance "Balance")
  (string-db 'store 'bal-title "Balance Sheet")
  (string-db 'store 'bal-desc "This page shows your net worth.")
  (string-db 'store 'pnl-title "Profit and Loss")
  (string-db 'store 'pnl-desc "This page shows your profits and losses.")

  (gnc:define-report
   'version 1
   'name "Balance sheet"
   'options-generator balsht-options-generator
   'renderer (lambda (options)
               (generate-balance-sheet-or-pnl
                (string-db 'lookup 'bal-title)
                (string-db 'lookup 'bal-desc)
                options
                #t)))

  (gnc:define-report
   'version 1
   'name "Profit and Loss"
   'options-generator pnl-options-generator
   'renderer (lambda (options)
               (generate-balance-sheet-or-pnl 
                (string-db 'lookup 'pnl-title)
                (string-db 'lookup 'pnl-desc)
                options
                #f))))
