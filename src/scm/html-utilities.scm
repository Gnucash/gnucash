;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-utilities.scm: Useful functions when using the HTML generator.
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "html-utilities.scm")

(gnc:depend "report-utilities.scm")
(gnc:depend "html-text.scm")
(gnc:depend "commodity-utilities.scm")

;; returns a list with n #f (empty cell) values 
(define (gnc:html-make-empty-cells n)
  (if (> n 0)
      (cons #f (gnc:html-make-empty-cells (- n 1)))
      '()))

;; returns the account name as html-text and anchor to the register.
(define (gnc:html-account-anchor acct)
  (gnc:make-html-text (gnc:html-markup-anchor
		       (string-append 
			"gnc-register:account=" 
			(gnc:account-get-full-name acct))
		       (gnc:account-get-name acct))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnc:html-build-acct-table
;;
;; Builds and returns a tree-(hierarchy-)shaped table as a html-table
;; object.
;;
;; Arguments: 
;;
;; <gnc:time-pair> start-date: Start date of reporting period. If #f,
;; everything till end-date will be considered. 
;;
;; <gnc:time-pair> end-date: End date of reporting period. 
;;
;; <int> tree-depth, <bool> show-subaccounts?, <gnc:list-of-account*>
;; accounts: An account is shown if ( tree-depth is large enough AND [
;; it is a member in accounts OR { show-subaccounts? == #t AND any of
;; the parents is member in accounts. }]) Note that the accounts shown
;; are totally independent from the calculated balance and vice
;; versa. 
;;
;; <bool> show-total? If #f, no total sum is shown. 
;;
;; #<procedure ...> get-total-fn: The function to calculate the total
;; sum, e.g. gnc:accounts-get-comm-total-{profit,assets}. 
;;
;; <chars> total-name: The name to show in the total sum line. 
;;
;; <bool> group-types?: Specify whether to group the accounts
;; according to their types and show a subtotal for each group.
;;
;; <bool> do-subtot?: Specify whether to include sub-account balances
;; in each account's balance. 
;;
;; <bool> show-other-curr?, <gnc:commodity*> report-commodity,
;; #<procedure ...> exchange-fn: The rightmost column always shows
;; balances in the currency report-commodity. If those balances happen
;; to be in another currency, they will get converted to the
;; report-commodity by means of the exchange-fn which e.g. came from
;; gnc:make-exchange-function. If show-other-curr? == #t, the
;; non-report-currencies will additionally be displayed in the
;; second-rightmost column.
;;
(define (gnc:html-build-acct-table 
	 start-date end-date 
	 tree-depth show-subaccts? accounts 
	 show-total? get-total-fn
	 total-name group-types? do-subtot? 
	 show-other-curr? report-commodity exchange-fn)
  (let ((table (gnc:make-html-table))
	(topl-accounts (gnc:group-get-account-list 
			(gnc:get-current-group))))

    ;; If start-date == #f then balance-at-date will be used (for
    ;; balance reports), otherwise balance-interval (for profit and
    ;; loss reports). Returns a commodity-collector.
    (define (my-get-balance account)
      (if start-date
	  (gnc:account-get-comm-balance-interval
	   account start-date end-date do-subtot?)
	  (gnc:account-get-comm-balance-at-date 
	   account end-date do-subtot?)))

    ;; show this account? Check against the account selection and,
    ;; if not selected, show-subaccts?==#t and any parent was
    ;; selected. (Maybe the other way around is more effective?)
    (define (show-acct? a)
      (or (member a accounts)
	  (and show-subaccts? 
	       (let ((parent (gnc:account-get-parent-account a)))
		 (and parent
		      (show-acct? parent))))))

    ;; sort an account list. Currently this uses only the account-code
    ;; field, but anyone feel free to add more options to this.
    (define (sort-fn accts)
      (sort accts
	    (lambda (a b) 
	      (string<? (gnc:account-get-code a)
			(gnc:account-get-code b)))))

    ;; just a trivial helper...
    (define (identity a) a)
    
    ;; The following functions are defined inside build-acct-table
    ;; to avoid passing tons of arguments which are constant anyway
    ;; inside this function.

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; functions for table without foreign commodities 
    
    ;; Returns a list which makes up a row in the table. current-depth
    ;; determines the number of empty cells, my-name is the
    ;; html-object to be displayed as name, and my-balance is a
    ;; gnc-monetary to be displayed in the balance column.
    (define (make-row-helper current-depth my-name my-balance)
      (append
       (gnc:html-make-empty-cells (- current-depth 1))
       (list (gnc:make-html-table-cell/size 
	      1 (+ 1 (- tree-depth current-depth)) 
	      my-name))
       (gnc:html-make-empty-cells (- tree-depth current-depth))
       ;; the account balance
       (list my-balance)
       (gnc:html-make-empty-cells (- current-depth 1))))

    ;; Returns a list which makes up a row in the table. The account
    ;; balance calculation is done here, but the row/cell setup is
    ;; done in the helper function.
    (define (make-row acct current-depth)
      (make-row-helper 
       current-depth 
       (gnc:html-account-anchor acct)
       ;; get the account balance, then exchange everything into the
       ;; report-commodity via gnc:sum-collector-commodity. If the
       ;; account-reverse-balance? returns true, then the sign gets
       ;; reversed, otherwise the value is left unchanged.
       ((if (gnc:account-reverse-balance? acct)
	    gnc:monetary-neg
	    identity)
	(gnc:sum-collector-commodity (my-get-balance acct) 
				     report-commodity exchange-fn))))
    
    ;; Adds rows to the table. Therefore it goes through the list of
    ;; accounts, runs make-row on each account.  If tree-depth and 
    ;; current-depth require, it will recursively call itself on the
    ;; list of children accounts. Is used if no foreign commodity is
    ;; shown.
    (define (traverse-accounts! accnts current-depth)
      (if (<= current-depth tree-depth)
	  (for-each (lambda (acct)
		      (begin
			(if (show-acct? acct)
			    (gnc:html-table-append-row!
			     table 
			     (make-row acct current-depth)))
			(traverse-accounts! 
			 (gnc:account-get-immediate-subaccounts acct)
			 (+ 1 current-depth))))
		    (sort-fn accnts))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; functions for table with foreign commodities visible
    
    ;; Adds all appropriate rows to the table which belong to one
    ;; balance, i.e. one row for each commodity. (Note: Multiple
    ;; commodities come e.g. from subaccounts with different
    ;; commodities.) my-name (a html-object) is the name to be printed
    ;; in the appropriate name column. my-commodity (a
    ;; <gnc:commodity*>) is the "natural" balance of the current
    ;; account. balance (a commodity-collector) is the balance to be
    ;; printed. If reverse-balance? == #t then the balance's signs get
    ;; reversed.
    (define (add-commodity-row-helper! 
	     current-depth my-name my-commodity balance reverse-balance?) 
      (begin
	;; the first row for each account: shows the name and the
	;; balance in the report-commodity
	(gnc:html-table-append-row! 
	 table
	 (append
	  (gnc:html-make-empty-cells (- current-depth 1))
	  (list (gnc:make-html-table-cell/size 
		 1 (+ 1 (- tree-depth current-depth)) 
		 my-name))
	  (gnc:html-make-empty-cells (* 2 (- tree-depth current-depth)))
	  (if (or do-subtot? 
		  (gnc:commodity-equiv? my-commodity report-commodity))
	      ;; usual case: the account balance in terms of report
	      ;; commodity
	      (list
	       (car (gnc:html-make-empty-cells 1))
	       (gnc:commodity-value->string 
		(balance 'getpair report-commodity reverse-balance?)))
	      ;; special case if do-subtot? was false and it is in a
	      ;; different commodity than the report: then the
	      ;; foreign commodity gets displayed in this line
	      ;; rather then the following lines (loop below).
	      (let ((my-balance 
		     (balance 'getpair my-commodity reverse-balance)))
		(list 
		 (gnc:commodity-value->string my-balance)
		 (gnc:commodity-value->string 
		  (exchange-fn my-balance report-commodity)))))
	  (gnc:html-make-empty-cells (* 2 (- current-depth 1)))))
	;; The additional rows: show no name, but the foreign currency
	;; balance and its corresponding value in the
	;; report-currency. One row for each non-report-currency. Is
	;; only used when do-subtot? == #f (otherwise this balance has
	;; only one commodity).
	(if do-subtot?
	    (balance 
	     'format 
	     (lambda (curr val)
	       (if (gnc:commodity-equiv? curr report-commodity)
		   '()
		   (gnc:html-table-append-row! 
		    table
		    (append
		     ;; print no account name 
		     (gnc:html-make-empty-cells tree-depth)
		     (gnc:html-make-empty-cells  
		      (* 2 (- tree-depth current-depth)))
		     ;; print the account balance in the respective
		     ;; commodity
		     (list
		      (gnc:commodity-value->string 
		       (list curr (if reverse-balance?
				      (gnc:numeric-neg val) val)))
		      (gnc:commodity-value->string 
		       (exchange-fn 
			(list curr (if reverse-balance?
				       (gnc:numeric-neg val) val))
			report-commodity)))
		     (gnc:html-make-empty-cells 
		      (* 2 (- current-depth 1))))))) 
	     #f))))
    
    
    
    
    ;; Adds all appropriate rows to the table which belong to one
    ;; account. Uses the above helper function, i.e. here the
    ;; necessary values only are "extracted" from the account. Is used
    ;; only if options "show foreign commodities" == #t.
    (define (add-commodity-rows! acct current-depth) 
      (add-commodity-row-helper! current-depth 
				 (gnc:html-account-anchor acct)
				 (gnc:account-get-commodity acct) 
				 (my-get-balance acct)
				 (gnc:account-reverse-balance? acct)))
    
    ;; The same as above (traverse-accounts!), but for showing foreign
    ;; currencies/commodities.
    (define (traverse-accounts-fcur! accnts current-depth) 
      (if (<= current-depth tree-depth)
	  (for-each (lambda (acct)
		      (begin
			(if (show-acct? acct)
			    (add-commodity-rows! acct current-depth))
			(traverse-accounts-fcur! 
			 (gnc:account-get-immediate-subaccounts acct)
			 (+ 1 current-depth))))
		    (sort-fn accnts))))

    ;; First iteration -- make the case destinction for
    ;; show-other-curr?.
    (define (start-traverse-accounts l d)
      (if show-other-curr?
	  (traverse-accounts-fcur! l d)
	  (traverse-accounts! l d)))


    ;;;;;;;;;;;;;;
    ;; Helper functions for the grouping of accounts according to their types.

    ;; Returns only those accounts out of the list l which have one of
    ;; the type identifiers in typelist.
    (define (filter-accountlist-type typelist l)
      (filter (lambda (a) 
		(member (gw:enum-<gnc:AccountType>-val->sym
			 (gnc:account-get-type a) #f)
			typelist) )
	      accounts))

    ;; Decompose a given list of accounts accts into different lists,
    ;; each with the name of that category as first element.
    (define (decompose-accountlist accts)
      (map (lambda (x) (cons
			(car x)
			(filter-accountlist-type (cdr x) accts)))
	   (list
	    (cons (_ "Assets") 
		  '(asset bank cash checking savings money-market 
			  stock mutual-fund currency))
	    (cons (_ "Liabilities") '(liability equity credit-line))
	    (cons (_ "Income") '(income))
	    (cons (_ "Expense") '(expense)))))

    ;; Generalization for a subtotal or the total balance.
    (define (add-subtotal-row! 
	     current-depth subtotal-name balance)
      (if show-other-curr?
	  (add-commodity-row-helper! current-depth subtotal-name 
				     report-commodity balance #f)
	  ;; Show no other currencies. Therefore just calculate
	  ;; one total via sum-collector-commodity and show it.
	  (gnc:html-table-append-row!
	   table 
	   (make-row-helper current-depth subtotal-name 
			    (gnc:sum-collector-commodity 
			     balance report-commodity 
			     exchange-fn)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; start the recursive account processing
    (if group-types?
	;; do a subtotal for each group
	(for-each 
	 (lambda (accts) 
	   (if (and (not (null? accts)) (not (null? (cdr accts))))
	       (begin
		 (add-subtotal-row! 
		  1 (car accts)
		  (let ((coll (make-commodity-collector)))
		    (for-each (lambda (x)
				(coll (if (gnc:account-reverse-balance? x)
					  'minusmerge 'merge)
				      (my-get-balance x) #f))
			      (cdr accts))
		    coll))
		 (start-traverse-accounts (cdr accts) 2))))
	 (decompose-accountlist topl-accounts))
	;; No extra grouping.
	(start-traverse-accounts topl-accounts 1))
    
    ;; Show the total sum.
    (if show-total?
	(add-subtotal-row! 
	 1 total-name (get-total-fn (filter show-acct? topl-accounts) 
				    my-get-balance)))
    
    ;; set default alignment to right, and override for the name
    ;; columns
    (gnc:html-table-set-style! 
     table "td" 
     'attribute '("align" "right")
     'attribute '("valign" "top"))

    (gnc:html-table-set-style! 
     table "th" 
     'attribute '("align" "right")
     'attribute '("valign" "top"))

    ;; there are tree-depth account name columns. 
    (let loop ((col 0))
      (gnc:html-table-set-col-style! 
       table col "td" 'attribute '("align" "left"))
      (gnc:html-table-set-col-style! 
       table col "th" 'attribute '("align" "left"))
      (if (< col (- tree-depth 1))
	  (loop (+ col 1))))
    
    table))

;; Print the exchangerate-list alist into the given html-txt object
;; txt-object, where the report's commodity is common-commodity.
(define (gnc:html-print-exchangerates!
	 txt-object common-commodity alist)
  (for-each 
   (lambda (pair)
     (gnc:html-text-append! 
      txt-object
      (gnc:html-markup-p
       (_ "Exchange rate ")
       (gnc:commodity-value->string 
	(list (car pair) (gnc:numeric-create 1 1)))
       " = "
       (gnc:commodity-value->string 
	(list common-commodity 
              ;; convert to 6 significant figures
	      (gnc:numeric-convert 
	       (cadr pair) 
               GNC-DENOM-AUTO 
               (logior (GNC-DENOM-SIGFIGS 6) GNC-RND-ROUND)))))))
   alist))

