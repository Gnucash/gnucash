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

(define (gnc:account-anchor-text acct)
  (string-append
   "gnc-register:guid=" 
   (gnc:account-get-guid acct)))

(define (gnc:split-anchor-text split)
  (string-append
   "gnc-register:guid=" 
   (gnc:split-get-guid split)))

(define (gnc:transaction-anchor-text trans)
  (string-append
   "gnc-register:guid=" 
   (gnc:transaction-get-guid trans)))

(define (gnc:report-anchor-text report-id)
  (string-append
   "gnc-report:id="
   (number->string report-id)))

;; returns the account name as html-text and anchor to the register.
(define (gnc:html-account-anchor acct)
  (gnc:make-html-text (gnc:html-markup-anchor
                       (gnc:account-anchor-text acct)
		       (gnc:account-get-name acct))))

(define (gnc:html-split-anchor split text)
  (gnc:make-html-text (gnc:html-markup-anchor
                       (gnc:split-anchor-text split)
                       text)))

(define (gnc:html-transaction-anchor trans text)
  (gnc:make-html-text (gnc:html-markup-anchor
                       (gnc:transaction-anchor-text trans)
                       text)))

(define (gnc:assign-colors num-colors)
  (define base-colors '("red" "orange" "yellow" "green"
                        "cyan" "blue" "purple" "magenta" 
			"orchid" "khaki" "gold" "orange"
			"red3" "orange3" "yellow3" "green3"
                        "cyan3" "blue3" "purple3" "magenta3" 
  			"orchid3" "khaki3" "gold3" "orange3"))
  (define (assign-colors i)
    (if (<= num-colors i)
        '()
        (cons (list-ref base-colors
                        (modulo i (length base-colors)))
              (assign-colors (+ i 1)))))
  (assign-colors 0))

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

    ;; The following functions are defined inside build-acct-table
    ;; to avoid passing tons of arguments which are constant anyway
    ;; inside this function.

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

    ;; just a stupid little helper
    (define (identity a)
      a)

    ;; Creates the table cell with given colspan (and rowspan=1), with
    ;; the content content and in boldface if boldface? is
    ;; true. content may be #f, or a string, or a html-text
    ;; object. Returns a html-table-cell object.
    (define (my-table-cell colspan content boldface?)
      (gnc:make-html-table-cell/size 
       1 colspan 
       (and content ;; if content == #f, just use #f
	    (if boldface? 
		;; Further improvement: use some other table cell
		;; style here ("grand-total") instead of the direct
		;; markup-b.
		(gnc:make-html-text
		 (if (gnc:html-text? content)
		     (apply gnc:html-markup-b 
			    (gnc:html-text-body content))
		     (gnc:html-markup-b content)))
		content))))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; function for table without foreign commodities 
    
    ;; Adds one row to the table. current-depth determines the number
    ;; of empty cells, my-name is the html-object to be displayed as
    ;; name, my-balance is a gnc-monetary to be displayed in the
    ;; balance column, and if reverse-balance? is #t the balance will
    ;; be displayed with the sign reversed.
    (define (add-row-helper! 
	     current-depth my-name my-balance reverse-balance? boldface?)
      (gnc:html-table-append-row! 
       table
       (append
	(gnc:html-make-empty-cells (- current-depth 1))
	(list (my-table-cell (+ 1 (- tree-depth current-depth))
			     my-name boldface?))
	(gnc:html-make-empty-cells (- tree-depth current-depth))
	;; the account balance
	(list (and my-balance
		   (gnc:make-html-text
		    ((if boldface? gnc:html-markup-b identity)
		     ((if reverse-balance? gnc:monetary-neg identity)
		      my-balance)))))
	(gnc:html-make-empty-cells (- current-depth 1)))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; function for table with foreign commodities visible

    ;; Adds all appropriate rows to the table which belong to one
    ;; balance, i.e. one row for each commodity. (Note: Multiple
    ;; commodities come e.g. from subaccounts with different
    ;; commodities.) my-name (a html-object) is the name to be printed
    ;; in the appropriate name column. my-commodity (a
    ;; <gnc:commodity*>) is the "natural" balance of the current
    ;; account. balance (a commodity-collector) is the balance to be
    ;; printed. If reverse-balance? == #t then the balance's signs get
    ;; reversed.
    (define (add-commodity-rows! 
	     current-depth my-name my-commodity balance 
	     reverse-balance? is-stock-account? boldface?) 
      ;; Adds one row to the table. my-name is the html-object
      ;; displayed in the name column; foreign-balance is the
      ;; <gnc-monetary> for the foreign column or #f if to be left
      ;; empty; domestic-balance is the <gnc-monetary> for the
      ;; domestic column.
      (define (commodity-row-helper! 
	       my-name foreign-balance domestic-balance)
	(gnc:html-table-append-row! 
	 table
	 (append
	  (gnc:html-make-empty-cells (- current-depth 1))
	  (list (my-table-cell (+ 1 (- tree-depth current-depth))
			       my-name boldface?))
	  (gnc:html-make-empty-cells (* 2 (- tree-depth current-depth)))
	  (if boldface?
	      (list 
	       (and foreign-balance 
		    (gnc:make-html-text (gnc:html-markup-b foreign-balance)))
	       (and domestic-balance
		    (gnc:make-html-text (gnc:html-markup-b domestic-balance))))
	      (list foreign-balance domestic-balance))
	  (gnc:html-make-empty-cells (* 2 (- current-depth 1))))))
      
      ;;;;;;;;;;
      ;; the first row for each account: shows the name and the
      ;; balance in the report-commodity
      (if (and (not is-stock-account?)
	       (or (gnc:commodity-equiv? my-commodity report-commodity)
		   do-subtot?))
	  ;; usual case: the account balance in terms of report
	  ;; commodity
	  (commodity-row-helper! 
	   my-name #f
	   (if balance 
	       (balance 'getmonetary report-commodity reverse-balance?)
	       #f))
	  ;; special case if do-subtot? was false and it is in a
	  ;; different commodity than the report: then the foreign
	  ;; commodity gets displayed in this line rather then the
	  ;; following lines (loop below). Is also used if
	  ;; is-stock-account? is true.
	  (let ((my-balance 
		 (if balance (balance 'getmonetary 
				      my-commodity reverse-balance?) #f)))
	    (commodity-row-helper! 
	     my-name
	     my-balance
	     (exchange-fn my-balance report-commodity))))
      
      ;; The additional rows: show no name, but the foreign currency
      ;; balance and its corresponding value in the
      ;; report-currency. One row for each non-report-currency. Is
      ;; only used when do-subtot? == #f (otherwise this balance has
      ;; only one commodity).
      (if (and do-subtot? (and balance (not is-stock-account?)))
	  (balance 
	   'format 
	   (lambda (curr val)
	     (if (gnc:commodity-equiv? curr report-commodity)
		 '()
		 (let ((bal 
			(if reverse-balance?
			    (gnc:monetary-neg (gnc:make-gnc-monetary curr val))
			    (gnc:make-gnc-monetary curr val))))
		   (commodity-row-helper!
		    ;; print no account name 
		    (car (gnc:html-make-empty-cells 1))
		    ;; print the account balance in the respective
		    ;; commodity
		    bal
		    (exchange-fn bal report-commodity)))))
	   #f)))
        
    ;; Adds all appropriate rows to the table which belong to one
    ;; account. Uses the above helper function, i.e. here the
    ;; necessary values only are "extracted" from the account.
    (define (add-account-rows! acct current-depth) 
      (if show-other-curr?
	  (add-commodity-rows! current-depth 
				     (gnc:html-account-anchor acct)
				     (gnc:account-get-commodity acct) 
				     (my-get-balance acct)
				     (gnc:account-reverse-balance? acct)
				     (gnc:account-has-shares? acct)
				     #f)
	  (add-row-helper! 
	   current-depth 
	   (gnc:html-account-anchor acct)
	   (gnc:sum-collector-commodity (my-get-balance acct) 
					report-commodity exchange-fn)
	   (gnc:account-reverse-balance? acct)
	   #f)))
  
    ;; Generalization of add-account-rows! for a subtotal or for the
    ;; total balance.
    (define (add-subtotal-row! 
	     current-depth subtotal-name balance boldface?)
      (if show-other-curr?
	  (add-commodity-rows! current-depth subtotal-name 
			       report-commodity 
			       (gnc:sum-collector-stocks 
				balance report-commodity exchange-fn)
			       #f #f boldface?)
	  ;; Show no other currencies. Therefore just calculate
	  ;; one total via sum-collector-commodity and show it.
	  (add-row-helper! current-depth subtotal-name 
			   (gnc:sum-collector-commodity 
			    balance report-commodity exchange-fn)
			   #f boldface?)))

    ;; This prints *all* the rows that belong to one group: the title
    ;; row, the subaccount tree, and the Total row with the balance of
    ;; the subaccounts. groupname may be a string or a html-text
    ;; object. subaccounts is a list of accounts. thisbalance is the
    ;; balance of this group, or it may be #f, in which case the
    ;; balance is calculated from the subaccounts list.
    (define (add-group! current-depth groupname subaccounts thisbalance)
      (begin
	;; first the group name
	(add-subtotal-row! current-depth groupname #f #f)
	;; then all the subaccounts
	(traverse-accounts! subaccounts (+ 1 current-depth))
	;; and now the "total" row
	(add-subtotal-row! 
	 current-depth 
	 (let ((total-text (gnc:make-html-text (_ "Total") " ")))
	   (if (gnc:html-text? groupname)
	       (apply gnc:html-text-append! 
		      total-text
		      (gnc:html-text-body groupname))
	       (gnc:html-text-append! total-text groupname))
	   total-text)
	 ;; A subbalance is only calculated if no thisbalance was
	 ;; given. (Because any "thisbalance" calculation already
	 ;; includes the appropriate subaccounts.)
	 (if thisbalance 
	     thisbalance
	     (gnc:accounts-get-balance-helper 
	      subaccounts my-get-balance gnc:account-reverse-balance?))
	 #t)
	;; and an empty line
	(add-subtotal-row! current-depth #f #f #f)))

    ;; Adds rows to the table. Therefore it goes through the list of
    ;; accounts, runs add-account-rows! on each account.  If
    ;; tree-depth and current-depth require, it will recursively call
    ;; itself on the list of children accounts.
    (define (traverse-accounts! accnts current-depth)
      (if (<= current-depth tree-depth)
	  (for-each 
	   (lambda (acct)
	     (let ((subaccts (filter 
			      show-acct?
			      (gnc:account-get-immediate-subaccounts acct))))
	       (if (or (= current-depth tree-depth) (null? subaccts))
		   (add-account-rows! acct current-depth)
		   (add-group! current-depth 
			       (gnc:html-account-anchor acct)
			       subaccts
			       (gnc:accounts-get-balance-helper 
				(list acct) my-get-balance 
				gnc:account-reverse-balance?)))))
	   (sort-fn accnts))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; start the recursive account processing
    (if group-types?
	;; Print a subtotal for each group.
	(for-each 
	 (lambda (accts) 
	   (if (and (not (null? accts)) (not (null? (cdr accts))))
	       (add-group! 1 (car accts) (cdr accts) #f)))
	 (gnc:decompose-accountlist (lset-intersection 
				     equal? accounts topl-accounts)))
	;; No extra grouping.
	(traverse-accounts! (filter show-acct? topl-accounts) 1))
    
    ;; Show the total sum.
    (if show-total?
        (begin
          (gnc:html-table-append-row! 
           table
           (list
            (gnc:make-html-table-cell/size
             1 (* 2 tree-depth) (gnc:make-html-text (gnc:html-markup-hr)))))
          (add-subtotal-row! 
           1 total-name 
           (get-total-fn (filter show-acct? topl-accounts) my-get-balance)
           #t)))
    
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

    ;; set some column headers 
    (gnc:html-table-set-col-headers!
     table 
     (list (gnc:make-html-table-header-cell/size 
	    1 tree-depth (_ "Account name"))
	   (gnc:make-html-table-header-cell/size
	    1 (if show-other-curr? 
		  (* 2 tree-depth)
		  tree-depth)
	    (_ "Balance"))))
    
    ;; there are tree-depth account name columns. 
    (let loop ((col 0))
      (gnc:html-table-set-col-style! 
       table col "td" 'attribute '("align" "left"))
      (gnc:html-table-set-col-style! 
       table col "th" 'attribute '("align" "left"))
      (if (< col (- tree-depth 1))
	  (loop (+ col 1))))
    
    table))


;; Returns a html-object which is a table of all exchange rates.
;; Where the report's commodity is common-commodity.
(define (gnc:html-make-exchangerates
	 common-commodity rate-alist accounts show-always?) 
  (let ((comm-list (delete-duplicates
		    (sort (map gnc:account-get-commodity accounts) 
			  (lambda (a b) 
			    (string<? (gnc:commodity-get-mnemonic a)
				      (gnc:commodity-get-mnemonic b))))))
	(table (gnc:make-html-table))
	(any-printed? #f))

    ;; Do something with each exchange rate.
    (for-each 
     (lambda (pair)
       (if (or show-always?
	       (member (car pair) comm-list))
	   (begin
	     (set! any-printed? #t)
	     (gnc:html-table-append-row! 
	      table
	      (list 
	       (gnc:make-gnc-monetary (car pair) (gnc:numeric-create 1 1))
	       ;; convert the foreign commodity to 6 significant digits
	       (gnc:make-gnc-monetary 
		common-commodity 
		(gnc:numeric-convert (cadr pair) GNC-DENOM-AUTO 
				     (logior (GNC-DENOM-SIGFIGS 6) 
					     GNC-RND-ROUND))))))))
     rate-alist)

    ;; Set some style
    (gnc:html-table-set-style! 
     table "td" 
     'attribute '("align" "right")
     'attribute '("valign" "top"))

    (if any-printed?
	;; set some column headers 
	(gnc:html-table-set-col-headers!
	 table 
	 (list (gnc:make-html-table-header-cell/size 
		1 2 (_ "Exchange rate ")))))

    table))
