;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transaction-report.scm : Report on all transactions in account(s)
;;
;; Original report by Robert Merkel <rgmerk@mira.net>
;; Contributions by Bryan Larsen <blarsen@ada-works.com>
;; More contributions for new report generation code by Robert Merkel
;; More contributions by Christian Stimming <stimming@tuhh.de>
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

(require 'record)
(gnc:support "report/transaction-report.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")

(let ((pagename-sorting (N_ "Sorting"))
      (optname-prime-sortkey (N_ "Primary Key"))
      (optname-prime-subtotal (N_ "Primary Subtotal"))
      (optname-prime-subtotal-by (N_ "Primary Subtotal by"))
      (optname-sec-sortkey (N_ "Secondary Key"))
      (optname-sec-subtotal (N_ "Secondary Subtotal"))
      (optname-sec-subtotal-by (N_ "Secondary Subtotal by")))

  (define-syntax addto!
    (syntax-rules ()
		  ((_ alist element) (set! alist (cons element alist)))))

  (define (split-account-full-name-same-p a b)
    (= (gnc:split-compare-account-full-names a b) 0))

  (define (split-account-code-same-p a b)
    (= (gnc:split-compare-account-codes a b) 0))

  (define (split-same-corr-account-full-name-p a b)
    (= (gnc:split-compare-other-account-full-names a b) 0))

  (define (split-same-corr-account-code-p a b)
    (= (gnc:split-compare-other-account-codes a b) 0))

  (define (timepair-same-year tp-a tp-b)
    (= (tm:year (gnc:timepair->date tp-a))
       (tm:year (gnc:timepair->date tp-b))))
  
  (define (timepair-same-month tp-a tp-b)
    (and (timepair-same-year tp-a tp-b) 
	 (= (tm:mon (gnc:timepair->date tp-a))
            (tm:mon (gnc:timepair->date tp-b)))))
  
  (define (split-same-month-p a b)
    (let ((tp-a (gnc:transaction-get-date-posted (gnc:split-get-parent a)))
	  (tp-b (gnc:transaction-get-date-posted (gnc:split-get-parent b))))
	  (timepair-same-month tp-a tp-b)))
    
  (define (split-same-year-p a b)
    (let ((tp-a (gnc:transaction-get-date-posted (gnc:split-get-parent a)))
	  (tp-b (gnc:transaction-get-date-posted (gnc:split-get-parent b))))
      (timepair-same-year tp-a tp-b)))
  
  (define (set-last-row-style! table tag . rest)
    (let ((arg-list 
	   (cons table 
		 (cons (- (gnc:html-table-num-rows table) 1)
                       (cons tag rest)))))
      (apply gnc:html-table-set-row-style! arg-list)))

  (define (add-subheading-row data table width subheading-style)
    (let ((heading-cell (gnc:make-html-table-cell data)))
      (gnc:html-table-cell-set-colspan! heading-cell width)
      (gnc:html-table-append-row!
       table
       (list heading-cell))
      (apply set-last-row-style!
	     (cons table (cons "tr" subheading-style)))))
  
  (define (render-account-full-name-subheading 
	   split table width subheading-style)
    (add-subheading-row (gnc:account-get-full-name 
			 (gnc:split-get-account split))
			table width subheading-style))

  (define (render-account-code-subheading split table width subheading-style)
    (add-subheading-row (gnc:account-get-code
			 (gnc:split-get-account split))
			table width subheading-style))
  
  (define (render-corresponding-account-name-subheading 
	   split table width subheading-style)
    (add-subheading-row (gnc:split-get-corr-account-full-name split)
			table width subheading-style))
  
  
  (define (render-corresponding-account-code-subheading 
             split table width subheading-style)
    (add-subheading-row (gnc:split-get-corr-account-code split)
			table width subheading-style))
  
  (define (render-month-subheading split table width subheading-style)
    (add-subheading-row (strftime "%B %Y" (gnc:timepair->date
					   (gnc:transaction-get-date-posted
					    (gnc:split-get-parent split))))
			table width subheading-style))
  
  (define (render-year-subheading split table width subheading-style)
    (add-subheading-row (strftime "%Y" (gnc:timepair->date
					(gnc:transaction-get-date-posted
					 (gnc:split-get-parent split))))
			table width subheading-style))

  (let ()
     
    (define account-types-to-reverse-assoc-list
      (list (cons 'none '())
	    (cons 'income-expense '(income expense))
	    (cons 'credit-accounts '(liability equity credit income))))


      (define comp-funcs-assoc-list
	(list (cons 'account-name  (vector 
				    'by-account-full-name 
				    split-account-full-name-same-p 
				    render-account-full-name-subheading))
	      (cons 'account-code  (vector 
				    'by-account-code 
				    split-account-code-same-p
				    render-account-code-subheading))
	      (cons 'date          (vector 'by-date #f #f))
	      (cons 'date-monthly
                    (vector 'by-date
                            split-same-month-p render-month-subheading))
	      (cons 'date-yearly
                    (vector 'by-date 
			    split-same-year-p 
			    render-year-subheading))
	      (cons 'corresponding-acc-name
                    (vector 'by-corr-account-full-name #f #f))
	      (cons 'corresponding-acc-code
                    (vector 'by-corr-account-code #f #f))
	      (cons 'corresponding-acc-name-subtotal 
		    (vector 'by-corr-account-full-name 
			    split-same-corr-account-full-name-p 
			    render-corresponding-account-name-subheading))
	      (cons 'corresponding-acc-code-subtotal 
		    (vector 
		     'by-corr-account-code 
		     split-same-corr-account-code-p 
		     render-corresponding-account-code-subheading))
	      (cons 'amount (vector 'by-amount #f #f))
	      (cons 'description (vector 'by-desc #f #f))
	      (cons 'number (vector 'by-num #f #f))
	      (cons 'memo   (vector 'by-memo #f #f))
	      (cons 'none   (vector 'by-none #f #f))))

      (define (used-date columns-used)
	(vector-ref columns-used 0))
      (define (used-num columns-used)
	(vector-ref columns-used 1))
      (define (used-description columns-used)
	(vector-ref columns-used 2))
      (define (used-account columns-used)
	(vector-ref columns-used 3))
      (define (used-other-account columns-used)
	(vector-ref columns-used 4))	
      (define (used-shares columns-used)
	(vector-ref columns-used 5))	
      (define (used-price columns-used)
	(vector-ref columns-used 6))	
      (define (used-amount-single columns-used)
	(vector-ref columns-used 7))	
      (define (used-amount-double-positive columns-used)
	(vector-ref columns-used 8))	
      (define (used-amount-double-negative columns-used)
	(vector-ref columns-used 9))	
      (define (used-running-balance columns-used)
	(vector-ref columns-used 10))
      (define (used-account-full-name columns-used)
	(vector-ref columns-used 11))

      (define columns-used-size 12)
      
      (define (num-columns-required columns-used)  
        (do ((i 0 (+ i 1)) 
             (col-req 0 col-req)) 
             ((>= i columns-used-size) col-req)
             (if (vector-ref columns-used i) (set! col-req (+ col-req 1)))))

      (define (build-column-used options)   
	(define (opt-val section name)
	  (gnc:option-value 
	   (gnc:lookup-option options section name)))
	(let ((column-list (make-vector columns-used-size #f)))
	  (if (opt-val gnc:pagename-display (N_ "Date"))
	      (vector-set! column-list 0 #t))
	  (if (opt-val gnc:pagename-display (N_ "Num"))
	      (vector-set! column-list 1 #t))
	  (if (opt-val gnc:pagename-display (N_ "Description"))
	      (vector-set! column-list 2 #t))
	  (if (opt-val gnc:pagename-display (N_ "Account"))
	      (vector-set! column-list 3 #t))
	  (if (opt-val gnc:pagename-display (N_ "Other Account"))
	      (vector-set! column-list 4 #t))
	  (if (opt-val gnc:pagename-display (N_ "Shares"))
	      (vector-set! column-list 5 #t))
	  (if (opt-val gnc:pagename-display (N_ "Price"))
	      (vector-set! column-list 6 #t))
	  (let ((amount-setting (opt-val gnc:pagename-display (N_ "Amount"))))
	    (if (eq? amount-setting 'single)
	      (vector-set! column-list 7 #t))
	    (if (eq? amount-setting 'double)
		(begin 
	      (vector-set! column-list 8 #t)
	      (vector-set! column-list 9 #t))))
	  (if (opt-val gnc:pagename-display (N_ "Running Balance"))
	      (vector-set! column-list 10 #t))
	  (if (opt-val gnc:pagename-display  (N_ "Use Full Account Name?"))
	      (vector-set! column-list 11 #t))
	;  (gnc:debug "Column list:" column-list)
      column-list))


  (define (make-heading-list column-vector)
    (let ((heading-list '()))
      (gnc:debug "Column-vector" column-vector)
      (if (used-date column-vector)
	  (addto! heading-list (_ "Date")))
      (if (used-num column-vector)
	  (addto! heading-list (_ "Num")))
      (if (used-description column-vector)
	  (addto! heading-list (_ "Description")))
      (if (used-account column-vector)
	  (addto! heading-list (_ "Account")))
      (if (used-other-account column-vector)
	  (addto! heading-list (_ "Transfer from/to")))
      (if (used-shares column-vector)
	  (addto! heading-list (_ "Shares")))
      (if (used-price column-vector)
	  (addto! heading-list (_ "Price")))
      (if (used-amount-single column-vector)
	  (addto! heading-list (_ "Amount")))
      ;; FIXME: Proper labels: what?
      (if (used-amount-double-positive column-vector)
	  (addto! heading-list (_ "Debit")))
      (if (used-amount-double-negative column-vector)
	  (addto! heading-list (_ "Credit")))
      (if (used-running-balance column-vector)
	  (addto! heading-list (_ "Balance")))
      (reverse heading-list)))

  (define (add-split-row table split column-vector
                         row-style account-types-to-reverse transaction-row?)
    (let* ((row-contents '())
	   (parent (gnc:split-get-parent split))
	   (account (gnc:split-get-account split))
	   (account-type (gw:enum-<gnc:AccountType>-val->sym
                          (gnc:account-get-type account) #f))
	   (currency (gnc:account-get-commodity account))
	   (damount (gnc:split-get-share-amount split))
	   (dummy1 (begin
		     (gnc:debug "account-type" account-type)
		     (gnc:debug "account-types-to-reverse"
                                account-types-to-reverse)
		     (gnc:debug "member result"
                                (member account-type account-types-to-reverse))
		     #f))
	   (split-value (gnc:make-gnc-monetary 
			 currency 
			 (if (member account-type account-types-to-reverse) 
			     (gnc:numeric-neg damount)
			     damount))))

      (if (used-date column-vector)
	  (addto! row-contents
                  (if transaction-row?
                      (gnc:timepair-to-datestring 
                       (gnc:transaction-get-date-posted parent))
                      " ")))
      (if (used-num column-vector)
	  (addto! row-contents
                  (if transaction-row?
                      (gnc:transaction-get-num parent)
                      " ")))
      (if (used-description column-vector)
	  (addto! row-contents
                  (if transaction-row?
                      (gnc:transaction-get-description parent)
                      " ")))
      (if (used-account column-vector)
	  (if (used-account-full-name column-vector)
	      (addto! row-contents (gnc:account-get-full-name account))
	      (addto! row-contents (gnc:account-get-name account))))

      (if (used-other-account column-vector)
	  (if (used-account-full-name column-vector)
	      
	  (addto! row-contents (gnc:split-get-corr-account-full-name 
				split))
	  (addto! row-contents (gnc:split-get-corr-account-name split))))

      (if (used-shares column-vector)
	  (addto! row-contents (gnc:split-get-share-amount split)))
      (if (used-price column-vector)
	  (addto! 
	   row-contents 
	   (gnc:make-gnc-monetary currency (gnc:split-get-share-price split))))
      (if (used-amount-single column-vector)
	  (addto! row-contents
                  (gnc:make-html-table-cell/markup "number-cell"
                                                   split-value)))
      (if (used-amount-double-positive column-vector)
	  (if (gnc:numeric-positive-p (gnc:gnc-monetary-amount split-value))
	      (addto! row-contents
                      (gnc:make-html-table-cell/markup "number-cell"
                                                       split-value))
	      (addto! row-contents " ")))
      (if (used-amount-double-negative column-vector)
	  (if (gnc:numeric-negative-p (gnc:gnc-monetary-amount split-value))
	      (addto! row-contents
                      (gnc:make-html-table-cell/markup
                       "number-cell" (gnc:monetary-neg split-value)))
	      (addto! row-contents " ")))
      (if (used-running-balance column-vector)
	  (addto! row-contents
                  (gnc:make-html-table-cell/markup
                   "number-cell"
                   (gnc:make-gnc-monetary currency
                                          (gnc:split-get-balance split)))))
      (gnc:html-table-append-row! table (reverse row-contents))
      (apply set-last-row-style! (cons table (cons "tr" row-style)))
      split-value))

  (define (lookup-sort-key sort-option)
    (vector-ref (cdr (assq sort-option comp-funcs-assoc-list)) 0))
  (define (lookup-subtotal-pred sort-option)
    (vector-ref (cdr (assq sort-option comp-funcs-assoc-list)) 1))
  (define (trep-options-generator)
    (define gnc:*transaction-report-options* (gnc:new-options))
    (define (gnc:register-trep-option new-option)
      (gnc:register-option gnc:*transaction-report-options* new-option))

    ;; General options

    (gnc:options-add-date-interval!
     gnc:*transaction-report-options*
     gnc:pagename-general (N_ "From") (N_ "To") "a")

    ;; goonie: what does this one mean? Btw it crashes anyway...
    (gnc:register-trep-option
     (gnc:make-multichoice-option
      gnc:pagename-general (N_ "Style")
      "d" (N_ "Report style")
      'single
      (list (vector 'multi-line
                    (N_ "Multi-Line")
                    (N_ "Display N lines"))
            (vector 'single
                    (N_ "Single")
                    (N_ "Display 1 line")))))

    ;; Accounts options

    ;; account to do report on
    (gnc:register-trep-option
     (gnc:make-account-list-option
      gnc:pagename-accounts (N_ "Account")
      "c" (N_ "Do transaction report on these accounts")
      (lambda ()
        ;; FIXME : gnc:get-current-accounts disappeared.
        (let ((current-accounts '())
              (num-accounts (gnc:group-get-num-accounts
                             (gnc:get-current-group)))
              (first-account (gnc:group-get-account
                              (gnc:get-current-group) 0)))
          (cond ((not (null? current-accounts)) (list (car current-accounts)))
                ((> num-accounts 0) (list first-account))
                (else ()))))
      #f #t))

    ;; Sorting options

    (let ((options gnc:*transaction-report-options*)
	  (key-choice-list 
	   (list (vector 'none
                         (N_ "None")
                         (N_ "Do not sort"))
		 (vector 'account-name
                         (N_ "Account Name(w/subtotal)")
                         (N_ "Sort & subtotal by account name"))
		 (vector 'account-code
                         (N_ "Account Code (w/subtotal)")
                         (N_ "Sort & subtotal by account code"))
		 (vector 'date
                         (N_ "Date")
                         (N_ "Sort by date"))
		 (vector 'date-monthly
                         (N_ "Date (subtotal monthly)")
                         (N_ "Sort by date & subtotal each month"))
		 
                 (vector 'date-yearly
                         (N_ "Date (subtotal yearly)")
                         (N_ "Sort by date & subtotal each year"))

                 (vector 'corresponding-acc-name
                         (N_ "Transfer from/to")
                         (N_ "Sort by account transferred from/to's name"))
		 
                 (vector 'corresponding-acc-name-subtotal
                         (N_ "Transfer from/to (w/subtotal)")
                         (N_ "Sort and subtotal by account transferred
 from/to's name"))
				
                 (vector 'corresponding-acc-code
                         (N_ "Transfer from/to code")
                         (N_ "Sort by account transferred from/to's code"))
		
                 (vector 'corresponding-acc-code-subtotal
                         (N_ "Transfer from/to (w/subtotal) by code")
                         (N_ "Sort and subtotal by account
 transferred from/to's code"))
		
                 (vector 'amount
                         (N_ "Amount")
                         (N_ "Sort by amount"))
		 
                 (vector 'description
                         (N_ "Description")
                         (N_ "Sort by description"))
		 
                 (vector 'number
                         (N_ "Number")
                         (N_ "Sort by check/transaction number"))
		 
                 (vector 'memo
                         (N_ "Memo")
                         (N_ "Sort by memo"))))
	  (subtotal-enabled '(account-name account-code date 
					   corresponding-acc-name
					   corresponding-acc-code)) 
					   ;;description))
	  (ascending-choice-list 
	   (list
	    (vector 'ascend
		    (N_ "Ascending")
		    (N_ "smallest to largest, earliest to latest"))
	    (vector 'descend
		    (N_ "Descending")
		    (N_ "largest to smallest, latest to earliest"))))
	  (subtotal-choice-list
	   (list
	    (vector 'by-week
		    (N_ "By week")
		    (N_ "By week"))
	    (vector 'by-month
		    (N_ "By month")
		    (N_ "By month"))
	    (vector 'by-year
		    (N_ "By year")
		    (N_ "By year")))))

      ;; primary sorting criterion
      (gnc:register-trep-option
       (gnc:make-multichoice-callback-option
	pagename-sorting optname-prime-sortkey
	"a" (N_ "Sort by this criterion first")
	'account-name
	key-choice-list #f
	(lambda (x)
	  (gnc:option-db-set-option-selectable-by-name
           gnc:*transaction-report-options*
	   pagename-sorting optname-prime-subtotal
	   (and (member x subtotal-enabled) #t)))))

      (gnc:register-trep-option
       (gnc:make-multichoice-option
	pagename-sorting (N_ "Primary Sort Order")
	"b" (N_ "Order of primary sorting")
	'ascend
	ascending-choice-list))

      (gnc:register-trep-option
       (gnc:make-complex-boolean-option
	pagename-sorting optname-prime-subtotal
	"c" 
	(N_ "Subtotal according to the primary key?")
	#t #f 
	(lambda (x) (gnc:option-db-set-option-selectable-by-name
                     gnc:*transaction-report-options*
		     pagename-sorting optname-prime-subtotal-by 
		     (and x 
			  (equal?
			   'date 
			   (gnc:option-value
			    (gnc:lookup-option
			     options
			     pagename-sorting optname-prime-sortkey))))))))
      
      (gnc:register-trep-option
       (gnc:make-multichoice-option
	pagename-sorting optname-prime-subtotal-by
	"d" (N_ "Subtotal by")
	'by-month
	subtotal-choice-list))
      
      ;; Secondary sorting criterion
      (gnc:register-trep-option
       (gnc:make-multichoice-callback-option
	pagename-sorting optname-sec-sortkey
	"e"
	(N_ "Sort by this criterion second")
	'date
	key-choice-list #f
	(lambda (x) 
	  (gnc:option-db-set-option-selectable-by-name
           gnc:*transaction-report-options*
	   pagename-sorting optname-sec-subtotal
	   (and (member x subtotal-enabled) #t)))))

      (gnc:register-trep-option
       (gnc:make-multichoice-option
	pagename-sorting (N_ "Secondary Sort Order")
	"f" (N_ "Order of Secondary sorting")
	'ascend
	ascending-choice-list))

      (gnc:register-trep-option
       (gnc:make-complex-boolean-option
	pagename-sorting optname-sec-subtotal
	"g" 
	(N_ "Subtotal according to the secondary key?")
	#t #f
	(lambda (x) (gnc:option-db-set-option-selectable-by-name
                     gnc:*transaction-report-options*
                     pagename-sorting optname-sec-subtotal-by x))))

      (gnc:register-trep-option
       (gnc:make-multichoice-option
	pagename-sorting optname-sec-subtotal-by
	"h" (N_ "Subtotal by")
	'by-month
	subtotal-choice-list)))

    ;; Display options

    (for-each
     (lambda (l)
       (gnc:register-trep-option
	(gnc:make-simple-boolean-option
	 gnc:pagename-display (car l) (cadr l) (caddr l) (cadddr l))))
     ;; One list per option here with: option-name, sort-tag,
     ;; help-string, default-value
     (list
      (list (N_ "Date") "a" (N_ "Display the date?") #t)
      (list (N_ "Num")  "b" (N_ "Display the check number?") #t)
      (list (N_ "Description") "c" (N_ "Display the description?") #t)
      (list (N_ "Memo") "d" (N_ "Display the memo?") #t)
      (list (N_ "Account") "e" (N_ "Display the account?") #t)
      (list (N_ "Use Full Account Name?") "f" 
	    (N_ "Display the full account name") #t)
      (list (N_ "Other Account")"g" 
	    (N_ "Display the other account?
 (if this is a split transaction, this parameter is guessed).") #f)
      (list (N_ "Shares") "h" (N_ "Display the number of shares?") #f)
      (list (N_ "Price") "i" "Display the shares price?" #f)
      ;; note the "Amount" multichoice option in between here
      (list (N_ "Running Balance") "k" (N_ "Display a running balance") #f)
      (list (N_ "Totals") "l" (N_ "Display the totals?") #t)))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      gnc:pagename-display (N_ "Amount")
      "j" (N_ "Display the amount?")  
      'single
      (list
       (vector 'none (N_ "None") (N_ "No amount display"))
      (vector 'single (N_ "Single") (N_ "Single Column Display"))
       (vector 'double (N_ "Double") (N_ "Two Column Display")))))
    
    (gnc:register-trep-option
     (gnc:make-multichoice-option
      gnc:pagename-display (N_ "Sign Reverses?")
      "m" "Reverse amount display for certain account types"
      'income-expense
      (list 
       (vector 'none (N_ "None") (N_ "Don't change any displayed amounts"))
       (vector 'income-expense (N_ "Income and Expense")
	       (N_ "Reverse amount display for Income and Expense Accounts"))
       (vector 'credit-accounts (N_ "Credit Accounts")
	       (N_ "Reverse amount display for Liability, Equity, Credit Card,
and Income accounts")))))

    ;; Color options

    (gnc:register-trep-option
     (gnc:make-color-option
      (N_ "Colors")  (N_ "Primary Subtotals/headings")
      "a" (N_ "Background color for primary subtotals and headings")
      (list #xff #xff #xff 0)
      255
      #f))

    (gnc:register-trep-option
     (gnc:make-color-option
      (N_ "Colors")  (N_ "Secondary Subtotals/headings")
      "b" (N_ "Background color for secondary subtotals and headings")
      (list #xff #xff #xff 0)
      255
      #f))
    (gnc:register-trep-option
     (gnc:make-color-option
      (N_ "Colors")  (N_ "Split Odd")
      "c" (N_ "Background color for odd-numbered splits (or main splits in a
 multi-line report)")
      (list #xff #xff #xff 0)
      255 
      #f))

    (gnc:register-trep-option
     (gnc:make-color-option
      (N_ "Colors") (N_ "Split Even")
      "d" (N_ "Background color for even-numbered splits
 (or \"other\" splits in a multi-line report)")
     (list #xff #xff #xff 0)
      255
      #f))

    (gnc:register-trep-option
     (gnc:make-color-option
      (N_ "Colors") (N_ "Grand Total")
      "e" (N_ "Background color for total")
      (list #xff #xff #xff 0)
      255
      #f))

    (gnc:options-set-default-section gnc:*transaction-report-options*
                                     gnc:pagename-general)

   gnc:*transaction-report-options*)

  (define (display-date-interval begin end)
    (let ((begin-string (strftime "%x" (localtime (car begin))))
	  (end-string (strftime "%x" (localtime (car end)))))
      (string-append (_ "From") " " begin-string (_"To") " " end-string)))

  (define (get-primary-subtotal-style options)
   (let ((bgcolor (gnc:lookup-option options 
                                     (N_ "Colors")
                                     (N_ "Primary Subtotals/headings"))))
        (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

  (define (get-secondary-subtotal-style options)
   (let ((bgcolor (gnc:lookup-option options
                                     (N_ "Colors")
                                     (N_ "Secondary Subtotals/headings"))))
        (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

  (define (get-grand-total-style options)
   (let ((bgcolor (gnc:lookup-option options
                                     (N_ "Colors")
                                     (N_ "Grand Total"))))
        (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

  (define (get-odd-row-style options)
    (let ((bgcolor (gnc:lookup-option options
                                     (N_ "Colors")
                                     (N_ "Split Odd"))))
         (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

  (define (get-even-row-style options)
    (let ((bgcolor (gnc:lookup-option options
                                      (N_ "Colors")
                                      (N_ "Split Even"))))
         (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))
              
  (define (make-split-table splits options)
    (define (add-subtotal-row table width subtotal-collector subtotal-style)
     (let ((currency-totals (subtotal-collector
                              'format gnc:make-gnc-monetary #f))
           (blanks (make-list (- width 1) #f)))
	(for-each (lambda (currency)
                    (gnc:html-table-append-row! 
                     table 
                     (append blanks
                             (list (gnc:make-html-table-cell/markup
                                    "total-number-cell" currency))))
                    (apply set-last-row-style! 
                           (cons table (cons "tr" subtotal-style))))
                  currency-totals)))

    (define (get-primary-subtotal-pred options)
      (vector-ref (cdr 
		     (assq (gnc:option-value
                            (gnc:lookup-option options
                                               pagename-sorting
                                               (N_ "Primary Key")))
			   comp-funcs-assoc-list)) 
		  1))

    (define (get-secondary-subtotal-pred options)
      (vector-ref (cdr 
                   (assq (gnc:option-value
                          (gnc:lookup-option options
                                             pagename-sorting
                                             (N_ "Secondary Key")))
			   comp-funcs-assoc-list))
		  1))

    (define (get-primary-subheading-renderer options)
      (vector-ref (cdr 
		   (assq (gnc:option-value
                          (gnc:lookup-option options
                                             pagename-sorting
                                             (N_ "Primary Key")))
			   comp-funcs-assoc-list))
		  2))

    (define (get-secondary-subheading-renderer options)
      (vector-ref (cdr 
		   (assq (gnc:option-value
                          (gnc:lookup-option options
                                             pagename-sorting
                                             (N_ "Secondary Key")))
			   comp-funcs-assoc-list))
		  2))

    (define (get-account-types-to-reverse options)
      (cdr (assq (gnc:option-value 
		  (gnc:lookup-option options
				     gnc:pagename-display
				     (N_ "Sign Reverses?")))
		 account-types-to-reverse-assoc-list)))
      

    (define (transaction-report-multi-rows-p options)
      (eq? (gnc:option-value
            (gnc:lookup-option options gnc:pagename-general (N_ "Style")))
	   'multi-line))

    (define (add-other-split-rows split table used-columns
                                  row-style account-types-to-reverse)
      (define (other-rows-driver split parent table used-columns i)
	(let ((current (gnc:transaction-get-split parent i)))
	  (gnc:debug "i" i)
	  (gnc:debug "current" current)
	  (cond ((not current) #f)
		((equal? current split)
		 (other-rows-driver split parent table used-columns (+ i 1)))
		(else (begin
			(add-split-row table current used-columns
                                       row-style account-types-to-reverse #f)
			(other-rows-driver split parent table used-columns
                                           (+ i 1)))))))

	     (other-rows-driver split (gnc:split-get-parent split)
                                table used-columns 0))

    (define (do-rows-with-subtotals splits 
                                    table 
                                    used-columns
                                    width
                                    multi-rows?
                                    odd-row?
				    account-types-to-reverse
                                    primary-subtotal-pred
                                    secondary-subtotal-pred 
                                    primary-subheading-renderer
                                    secondary-subheading-renderer
                                    main-row-style
                                    alternate-row-style
                                    primary-subtotal-style
                                    secondary-subtotal-style
                                    grand-total-style
                                    primary-subtotal-collector 
                                    secondary-subtotal-collector 
                                    total-collector)
      (if (null? splits)
          (begin
            (gnc:html-table-append-row!
             table
             (list
              (gnc:make-html-table-cell/size
               1 width (gnc:make-html-text (gnc:html-markup-hr)))))

            (add-subtotal-row table width total-collector grand-total-style))

	  (let* ((current (car splits))
                 (current-row-style (if multi-rows? main-row-style
                                        (if odd-row? main-row-style 
                                                     alternate-row-style)))
		 (rest (cdr splits))
		 (next (if (null? rest) #f
			   (car rest)))
	         (split-value (add-split-row 
                               table 
                               current 
                               used-columns 
                               current-row-style
			       account-types-to-reverse
                               #t)))
	    (if multi-rows?
                (add-other-split-rows 
                 current table used-columns alternate-row-style))
	    (primary-subtotal-collector 'add 
					(gnc:gnc-monetary-commodity
                                         split-value) 
					(gnc:gnc-monetary-amount split-value))
	    (secondary-subtotal-collector 'add
					  (gnc:gnc-monetary-commodity
                                           split-value)
					  (gnc:gnc-monetary-amount
                                           split-value))
	    (total-collector 'add
			     (gnc:gnc-monetary-commodity split-value)
			     (gnc:gnc-monetary-amount split-value))
	    (if (and secondary-subtotal-pred
                     (or (not next)
                         (and next
                              (not (secondary-subtotal-pred current next)))))
		(begin (add-subtotal-row table width
                                         secondary-subtotal-collector
                                         secondary-subtotal-style)
		       (secondary-subtotal-collector 'reset #f #f)
		       (if next
                           (secondary-subheading-renderer
                            next table width secondary-subtotal-style))))
	    (if (and primary-subtotal-pred
                     (or (not next)
                         (and next
                              (not (primary-subtotal-pred current next)))))
		(begin (add-subtotal-row table width 
                                         primary-subtotal-collector
                                         primary-subtotal-style)
		       (primary-subtotal-collector 'reset #f #f)
		       (if next
		       (primary-subheading-renderer
                        next table width primary-subtotal-style))))
	    (do-rows-with-subtotals rest 
				    table 
				    used-columns
                                    width 
				    multi-rows?
                                    (not odd-row?)
				    account-types-to-reverse
				    primary-subtotal-pred 
				    secondary-subtotal-pred
				    primary-subheading-renderer 
				    secondary-subheading-renderer
                                    main-row-style
                                    alternate-row-style
                                    primary-subtotal-style
                                    secondary-subtotal-style
                                    grand-total-style
				    primary-subtotal-collector 
				    secondary-subtotal-collector 
				    total-collector))))

    (let* ((table (gnc:make-html-table))
	  (used-columns (build-column-used options))
          (width (num-columns-required used-columns))
	  (multi-rows? (transaction-report-multi-rows-p options))
	  (primary-subtotal-pred (get-primary-subtotal-pred options))
	  (secondary-subtotal-pred (get-secondary-subtotal-pred options))
          (primary-subheading-renderer
           (get-primary-subheading-renderer options))
           (secondary-subheading-renderer
         (get-secondary-subheading-renderer options))
          (primary-subtotal-style
           (get-primary-subtotal-style options))
          (secondary-subtotal-style
            (get-secondary-subtotal-style options))
          (grand-total-style 
            (get-grand-total-style options))
          (odd-row-style 
           (get-odd-row-style options))
          (even-row-style
           (get-even-row-style options))
	  (account-types-to-reverse
	   (get-account-types-to-reverse options)))

      (gnc:debug "account-types-to-reverse " account-types-to-reverse)
      (gnc:html-table-set-col-headers!
      table
       (make-heading-list used-columns))
;     (gnc:warn "Splits:" splits)
     (if (not (null? splits))
      (if primary-subheading-renderer 
        (primary-subheading-renderer
         (car splits) table width primary-subtotal-style))
      (if secondary-subheading-renderer
        (secondary-subheading-renderer
         (car splits) table widthsecondary-subtotal-style)))

     (do-rows-with-subtotals splits table used-columns width
                             multi-rows? #t 
			     account-types-to-reverse
			     primary-subtotal-pred
                             secondary-subtotal-pred
			     primary-subheading-renderer
			     secondary-subheading-renderer
                             odd-row-style
                             even-row-style
                             primary-subtotal-style
                             secondary-subtotal-style
                             grand-total-style
 			     (gnc:make-commodity-collector)
                              (gnc:make-commodity-collector)
                              (gnc:make-commodity-collector))
      table))

  (define (trep-renderer report-obj)
    (define (opt-val section name)
      (gnc:option-value
       (gnc:lookup-option (gnc:report-options report-obj) section name)))

    (let ((document (gnc:make-html-document))
	  (c_accounts (opt-val gnc:pagename-accounts "Account"))
	  (begindate (gnc:timepair-start-day-time
		      (gnc:date-option-absolute-time
		       (opt-val gnc:pagename-general "From"))))
	  (enddate (gnc:timepair-end-day-time
		    (gnc:date-option-absolute-time
		     (opt-val gnc:pagename-general "To"))))
	  (primary-key (opt-val pagename-sorting "Primary Key"))
	  (primary-order (opt-val pagename-sorting "Primary Sort Order"))
	  (secondary-key (opt-val pagename-sorting "Secondary Key"))
	  (secondary-order (opt-val pagename-sorting "Secondary Sort Order"))
	  (splits '())
          (table '())
	  (query (gnc:malloc-query)))

      (gnc:query-set-group query (gnc:get-current-group))
      (gnc:query-add-account-match query
                                   (gnc:list->glist c_accounts)
                                   'acct-match-any 'query-and)
      (gnc:query-add-date-match-timepair
       query #t begindate #t enddate 'query-and)
      (gnc:query-set-sort-order query
				(lookup-sort-key primary-key)
                                (lookup-sort-key secondary-key)
				'by-none)
      (gnc:query-set-sort-increasing query
				     (eq? primary-order 'ascend)
				     (eq? secondary-order 'ascend)
				     #t)

      (set! splits (gnc:glist->list (gnc:query-get-splits query)
                                    <gnc:Split*>))
;      (gnc:warn "Splits in trep-renderer:" splits)
      (set! table (make-split-table splits (gnc:report-options report-obj)))
							 
      (gnc:html-document-set-title! document (_ "Transaction Report"))
      (gnc:html-document-add-object! 
       document
       (gnc:make-html-text
	(gnc:html-markup-h3 (display-date-interval begindate enddate))))
      (gnc:html-document-add-object!
       document 
       table)
      (gnc:free-query query)

      document))

  (gnc:define-report

   ;; The version of this report.
   'version 2

   ;; The name of this report. This will be used, among other things,
   ;; for making its menu item in the main menu. You need to use the
   ;; untranslated value here!
   'name (N_ "Transaction Report")

   ;; The options generator function defined above.
   'options-generator trep-options-generator

   ;; The rendering function defined above.
   'renderer trep-renderer)))
