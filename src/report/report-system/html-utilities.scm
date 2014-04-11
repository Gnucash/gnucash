;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-utilities.scm: Useful functions when using the HTML generator.
;; 
;; Modified slightly by David Montenegro 2004.06.18.
;; 
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns a list with n #f (empty cell) values 
(define (gnc:html-make-empty-cell) #f)
(define (gnc:html-make-empty-cells n)
  (if (> n 0)
      (cons #f (gnc:html-make-empty-cells (- n 1)))
      (list)))

(define (gnc:register-guid type guid)
  (gnc-build-url URL-TYPE-REGISTER (string-append type guid) ""))

(define (gnc:account-anchor-text acct)
  (gnc:register-guid "acct-guid=" (gncAccountGetGUID acct)))

(define (gnc:split-anchor-text split)
  (gnc:register-guid "split-guid=" (gncSplitGetGUID split)))

(define (gnc:transaction-anchor-text trans)
  (gnc:register-guid "trans-guid=" (gncTransGetGUID trans)))

(define (gnc:report-anchor-text report-id)
  (gnc-build-url URL-TYPE-REPORT
		      (string-append "id=" (number->string report-id))
		      ""))

(define (gnc:price-anchor-text price)
  (gnc-build-url URL-TYPE-PRICE
		      (string-append "price-guid=" (gncPriceGetGUID price))
		      ""))

;; Make a new report and return the anchor to it. The new report of
;; type 'reportname' will have the option values copied from
;; 'src-options', and additionally this function sets all options
;; according to 'optionlist'. Each element of optionlist is a list of
;; section, name, and value of the function.
(define (gnc:make-report-anchor reportname src-report
				optionlist)
  (let ((src-options (gnc:report-options src-report))
	(options (gnc:make-report-options reportname)))
    (if options
	(begin
	  (gnc:options-copy-values src-options options)
	  (for-each
	   (lambda (l)
	     (let ((o (gnc:lookup-option options (car l) (cadr l))))
	       (if o
		   (gnc:option-set-value o (caddr l))
		   (warn "gnc:make-report-anchor:" reportname
			 " No such option: " (car l) (cadr l)))))
	   optionlist)
	  (let ((id (gnc:make-report reportname options)))
	    (gnc:report-anchor-text id)))
	(warn "gnc:make-report-anchor: No such report: " reportname))))


;; returns the account name as html-text and anchor to the register.
(define (gnc:html-account-anchor acct)
  (gnc:make-html-text (if (and acct (not (null? acct)))
                          (gnc:html-markup-anchor
                           (gnc:account-anchor-text acct)
                           (xaccAccountGetName acct))
                          "")))

(define (gnc:html-split-anchor split text)
  (gnc:make-html-text (if (not (null? (xaccSplitGetAccount split)))
                          (gnc:html-markup-anchor
                           (gnc:split-anchor-text split)
                           text)
                          text)))

(define (gnc:html-transaction-anchor trans text)
  (gnc:make-html-text (gnc:html-markup-anchor
                       (gnc:transaction-anchor-text trans)
                       text)))

(define (gnc:html-price-anchor price value)
  (gnc:make-html-text (if price
                          (gnc:html-markup-anchor
                           (gnc:price-anchor-text price)
			   (if value
			       value
			       (gnc-price-get-value price)))
                          value)))

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

;; Appends a horizontal ruler to a html-table with the specified
;; colspan at, optionally, the specified column.
(define (gnc:html-table-append-ruler/at! table colskip colspan)
  (define empty-cell '())
  (gnc:html-table-append-row! 
   table
   (append (make-list colskip empty-cell)
    (list
     (gnc:make-html-table-cell/size
      1 colspan (gnc:make-html-text (gnc:html-markup-hr)))))))
     
(define (gnc:html-table-append-ruler/at/markup! table markup colskip colspan)
  (define empty-cell "")
  (gnc:html-table-append-row/markup! 
   table
   markup
   (append (make-list colskip empty-cell)
    (list
     (gnc:make-html-table-cell/size
      1 colspan (gnc:make-html-text (gnc:html-markup-hr)))))))

(define (gnc:html-table-append-ruler! table colspan)
  (gnc:html-table-append-ruler/at! table 0 colspan))

(define (gnc:html-table-append-ruler/markup! table markup colspan)
  (gnc:html-table-append-ruler/at/markup! table markup 0 colspan))

;; Creates a table cell with some text in it. The cell will be created
;; with the colspan 'colspan' (the rowspan==1), the content 'content'
;; and in boldface if 'boldface?' is true. 'content' may be #f, or a
;; string, or a <html-text> object. Returns a <html-table-cell>
;; object.
(define (gnc:html-acct-table-cell colspan content boldface?)
  ;; instead of html-markup-b, just use the corresponding html-table-styles.
  (define default-style "text-cell")
  (define boldface-style "total-label-cell")
  (gnc:make-html-table-cell/size/markup 
   1 colspan 
   (if boldface? boldface-style default-style)
   content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function for account table without foreign commodities 
    
;; Adds one row to the table. current-depth determines the number
;; of empty cells, my-name is the html-object to be displayed as
;; name, my-balance is a gnc-monetary to be displayed in the
;; balance column, and if reverse-balance? is #t the balance will
;; be displayed with the sign reversed.
(define (gnc:html-acct-table-row-helper! 
	 table tree-depth
	 current-depth my-name my-balance 
	 reverse-balance? row-style boldface? group-header-line?)
  ;; just a stupid little helper
  (define (identity a)
    a)
  (gnc:html-table-append-row/markup! 
   table
   row-style
   (append
    ;; left half of the table
    (gnc:html-make-empty-cells (- current-depth 1))
    (list (gnc:html-acct-table-cell (+ 1 (- tree-depth current-depth))
				    my-name boldface?))
    ;; right half of the table
    (gnc:html-make-empty-cells 
     (- tree-depth (+ current-depth (if group-header-line? 1 0))))
    ;; the account balance
    (list (and my-balance
	       (gnc:make-html-table-cell/markup 
		"number-cell"
		(gnc:make-html-text
		 ((if boldface? gnc:html-markup-b identity)
		  ((if reverse-balance? gnc:monetary-neg identity)
		   my-balance))))))
    (gnc:html-make-empty-cells (- current-depth 
				  (if group-header-line? 0 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function for account table with foreign commodities visible

;; Adds all appropriate rows to the table which belong to one
;; balance, i.e. one row for each commodity. (Note: Multiple
;; commodities come e.g. from subaccounts with different
;; commodities.) my-name (a html-object) is the name to be printed
;; in the appropriate name column. my-commodity (a
;; <gnc:commodity*>) is the "natural" balance of the current
;; account. balance (a commodity-collector) is the balance to be
;; printed. If reverse-balance? == #t then the balances' signs get
;; reversed.
;; DM: If you trace this function through gnc:html-build-acct-table,
;; my-commodity always ends up being report-commodity.
(define (gnc:html-acct-table-comm-row-helper!
	 table tree-depth report-commodity exchange-fn
	 current-depth my-name my-commodity balance 
	 reverse-balance? is-stock-account? main-row-style other-rows-style 
	 boldface? group-header-line?) 
  (let ((already-printed #f))
    ;; Adds one row to the table. my-name is the html-object
    ;; displayed in the name column; foreign-balance is the
    ;; <gnc-monetary> for the foreign column or #f if to be left
    ;; empty; domestic-balance is the <gnc-monetary> for the
    ;; domestic column.
    (define (commodity-row-helper! 
	     my-name foreign-balance domestic-balance row-style)
      (gnc:html-table-append-row/markup!
       table
       row-style
       (append
	;; left third of the table
	(gnc:html-make-empty-cells (- current-depth 1))
	(list (gnc:html-acct-table-cell (+ 1 (- tree-depth current-depth))
					my-name boldface?))
	;; right two-thirds of the table
	(gnc:html-make-empty-cells 
	 (* 2 (- tree-depth (+ current-depth (if group-header-line? 1 0)))))
	(if boldface?
	    (list 
	     (and foreign-balance 
		  (gnc:make-html-table-cell/markup 
		   "number-cell"
		   (gnc:make-html-text (gnc:html-markup-b foreign-balance))))
	     (and 
	      domestic-balance
	      (gnc:make-html-table-cell/markup 
	       "number-cell"
	       (gnc:make-html-text (gnc:html-markup-b domestic-balance)))))
	    (list 
	     (and foreign-balance
	    	(gnc:make-html-table-cell/markup 
	        "number-cell"
	        foreign-balance))
	     (and domestic-balance
		(gnc:make-html-table-cell/markup 
	        "number-cell"
	        domestic-balance))))
	(gnc:html-make-empty-cells (* 2 (- current-depth 
					   (if group-header-line? 0 1)))))))
    
    ;;;;;;;;;;
    ;; the first row for each account: shows the name and the
    ;; balance in the report-commodity
    (if (and (not is-stock-account?)
	     ;; FIXME: need to check whether we really have only one
	     ;; foreign currency if is-stock-account==#t.
	     (gnc-commodity-equiv my-commodity report-commodity))
	;; usual case: the account balance in terms of report
	;; commodity
	(commodity-row-helper! 
	 my-name #f
	 (if balance 
	     (gnc-commodity-collector-assoc
	      balance report-commodity reverse-balance?)
	     #f)
	 main-row-style)
	;; Special case for stock-accounts: then the foreign commodity
	;; gets displayed in this line rather then the following lines
	;; (loop below). Is also used if is-stock-account? is true.
	(let ((my-balance 
	       (if balance 
		   (gnc-commodity-collector-assoc
		    balance my-commodity reverse-balance?) #f)))
	  (set! already-printed my-commodity)
	  (commodity-row-helper! 
	   my-name
	   my-balance
	   (exchange-fn my-balance report-commodity)
	   main-row-style)))
    
    ;; The additional rows: show no name, but the foreign currency
    ;; balance and its corresponding value in the
    ;; report-currency. One row for each non-report-currency. 
    (if (and balance (not is-stock-account?))
	(gnc-commodity-collector-map
	 balance 
	 (lambda (curr val)
	   (if (or (gnc-commodity-equiv curr report-commodity)
		   (and already-printed
			(gnc-commodity-equiv curr already-printed)))
	       '()
	       (let ((bal 
		      (if reverse-balance?
			  (gnc:monetary-neg (gnc:make-gnc-monetary curr val))
			  (gnc:make-gnc-monetary curr val))))
		 (commodity-row-helper!
		  ;; print no account name 
		  (gnc:html-make-empty-cell)
		  ;; print the account balance in the respective
		  ;; commodity
		  bal
		  (exchange-fn bal report-commodity)
		  other-rows-style))))
	 ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnc:html-build-acct-table
;;
;; Builds and returns a tree-(hierarchy-)shaped table as a html-table
;; object. 
;;
;; Arguments by topic: 
;;
;; Reporting period -- start-date, end-date
;;
;; Selected accounts -- tree-depth, show-subaccts?, accounts
;;
;; Foreign currency -- show-other-curr?, report-commodity,
;;                     exchange-fn
;;
;; Output fine-tuning -- show-col-headers?, show-total? (with
;;                       total-name, get-total-fn), group-types?,
;;                       show-parent-balance?, show-parent-total?
;;
;; Feedback while building -- start-percent, delta-percent
;;
;; Note: The returned table object will have 2*tree-depth columns if
;; show-other-curr?==#f, else it will have 3*tree-depth columns.
;;
;; Arguments in detail: 
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
;; <bool> show-col-headers?: show column headings "Account" and
;; "Balance"
;;
;; <bool> show-total?: If #f, no total sum is shown. 
;;
;; #<procedure ...> get-total-fn: The function to calculate the total
;; sum, e.g. gnc:accounts-get-comm-total-{profit,assets}. 
;;
;; <chars> total-name: The name to show in the total sum line. 
;;
;; <bool> group-types?: Specify whether to group the accounts
;; according to their types and show a subtotal for each group.
;;
;; <bool> show-parent-balance?: Specify whether to show balances of
;; non-leaf accounts seperately.
;;
;; <bool> show-parent-total?: Whether to show a line with the label
;; e.g. "Total My-Assets" and the subtotal for this account and its
;; children.
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
;; <int> start-percent, delta-percent: Fill in the [start:start+delta]
;; section of the progress bar while running this function.
;;
(define (gnc:html-build-acct-table 
	 start-date end-date 
	 tree-depth show-subaccts? accounts 
	 start-percent delta-percent
	 show-col-headers?
	 show-total? get-total-fn
	 total-name group-types? show-parent-balance? show-parent-total? 
	 show-other-curr? report-commodity exchange-fn show-zero-entries?)
  ;; Select, here, which version of gnc:html-build-acct-table you want
  ;; to use by default.
  (define fn-version 'first)
  (if (equal? fn-version 'second)
      (gnc:second-html-build-acct-table 
       start-date end-date 
       tree-depth show-subaccts? accounts 
       start-percent delta-percent
       show-col-headers?
       show-total? get-total-fn
       total-name group-types? show-parent-balance? show-parent-total? 
       show-other-curr? report-commodity exchange-fn show-zero-entries?)
      (gnc:first-html-build-acct-table 
       start-date end-date 
       tree-depth show-subaccts? accounts 
       start-percent delta-percent
       show-col-headers?
       show-total? get-total-fn
       total-name group-types? show-parent-balance? show-parent-total? 
       show-other-curr? report-commodity exchange-fn show-zero-entries?)
      )
  )

(define (gnc:first-html-build-acct-table 
	 start-date end-date 
	 tree-depth show-subaccts? accounts 
	 start-percent delta-percent
	 show-col-headers?
	 show-total? get-total-fn
	 total-name group-types? show-parent-balance? show-parent-total? 
	 show-other-curr? report-commodity exchange-fn show-zero-entries?)
  (let ((table (gnc:make-html-table))
	(work-to-do 0)
	(work-done 0)
	(topl-accounts (gnc-account-get-children-sorted
			(gnc-get-current-root-account))))

    ;; The following functions are defined inside build-acct-table
    ;; to avoid passing tons of arguments which are constant anyway
    ;; inside this function.

    ;; If start-date == #f then balance-at-date will be used (for
    ;; balance reports), otherwise balance-interval (for profit and
    ;; loss reports). This function takes only the current account
    ;; into consideration, i.e. none of the subaccounts are included
    ;; in the balance. Returns a commodity-collector.
    (define (my-get-balance-nosub account)
      (if start-date
	  (gnc:account-get-comm-balance-interval
	   account start-date end-date #f)
	  (gnc:account-get-comm-balance-at-date 
	   account end-date #f)))

    ;; Additional function that includes the subaccounts as
    ;; well. Note: It is necessary to define this here (instead of
    ;; changing an argument for account-get-balance) because the
    ;; use-acct? query is needed.
    (define (my-get-balance account)
      ;; this-collector for storing the result
      (let ((this-collector (my-get-balance-nosub account)))
	(for-each 
	 (lambda (x) (if x 
			 (gnc-commodity-collector-merge
			  this-collector x )))
	 (gnc:account-map-descendants
	  (lambda (a)
	    ;; Important: Calculate the balance if and only if the
	    ;; account a is shown, i.e. (use-acct? a) == #t.
	    (and (use-acct? a)
		 (my-get-balance-nosub a)))
	  account))
	this-collector))

    ;; Use this account in the account hierarchy? Check against the
    ;; account selection and, if not selected, show-subaccts?==#t and
    ;; any parent was selected. (Maybe the other way around is more
    ;; effective?)
    (define (use-acct? a)
      (or (member a accounts)
	  (and show-subaccts? 
	       (let ((parent (gnc-account-get-parent a)))
		 (and parent
		      (use-acct? parent))))))

    ;; Show this account? Only if nonzero amount or appropriate
    ;; preference.
    (define (show-acct? a)
      (and (or show-zero-entries?
	       (not (gnc-commodity-collector-allzero?
		     (my-get-balance a))))
	   (use-acct? a)))

    ;; sort an account list. Currently this uses only the account-code
    ;; field, but anyone feel free to add more options to this.
    (define (sort-fn accts)
      (sort accts
	    (lambda (a b) 
	      (string<? (xaccAccountGetCode a)
			(xaccAccountGetCode b)))))

    ;; Remove the last appended row iff *all* its fields are empty
    ;; (==#f) or have an html-table-cell which in turn is empty
    ;; (resulting from the add-group! function above). Note: This
    ;; depends on the structure of html-table-data, i.e. if those are
    ;; changed then this might break.
    (define (remove-last-empty-row)
      (if (and (not (null? (gnc:html-table-data table))) 
	       (not (or-map
		(lambda (e) 
		  (if (gnc:html-table-cell? e)
		      (car (gnc:html-table-cell-data e))
		      e))
		    (car (gnc:html-table-data table)))))
	  (gnc:html-table-remove-last-row! table)))

    ;; Wrapper for gnc:html-acct-table-row-helper!
    (define (add-row-helper! 
	     current-depth my-name my-balance 
	     reverse-balance? row-style boldface? group-header-line?)
      (gnc:html-acct-table-row-helper! 
       table tree-depth
       current-depth my-name my-balance 
       reverse-balance? row-style boldface? group-header-line?))
    
    ;; Wrapper
    (define (add-commodity-rows! 
	     current-depth my-name my-commodity balance 
	     reverse-balance? is-stock-account? 
	     main-row-style other-rows-style boldface? group-header-line?) 
      (gnc:html-acct-table-comm-row-helper!
       table tree-depth report-commodity exchange-fn
       current-depth my-name my-commodity balance 
       reverse-balance? is-stock-account? 
       main-row-style other-rows-style boldface? group-header-line?))
        
    ;; Adds all appropriate rows to the table which belong to one
    ;; account. Uses the above helper function, i.e. here the
    ;; necessary values only are "extracted" from the account.
    (define (add-account-rows! acct current-depth alternate-row?) 
      (let ((row-style (if alternate-row? "alternate-row" "normal-row")))
      (if show-other-curr?
	  (add-commodity-rows! current-depth 
			       (gnc:html-account-anchor acct)
			       (xaccAccountGetCommodity acct)
			       (my-get-balance acct)
			       (gnc-reverse-balance acct)
			       (gnc:account-has-shares? acct)
			       row-style row-style
			       #f #f)
	  (add-row-helper! 
	   current-depth 
	   (gnc:html-account-anchor acct)
	   (gnc:sum-collector-commodity (my-get-balance acct) 
					report-commodity exchange-fn)
	   (gnc-reverse-balance acct)
	   row-style
	       #f #f))))
  
    ;; Generalization of add-account-rows! for a subtotal or for the
    ;; total balance.
    (define (add-subtotal-row! 
	     current-depth subtotal-name balance 
	     row-style boldface? group-header-line?)
      (if show-other-curr?
	  (add-commodity-rows! current-depth subtotal-name 
			       report-commodity 
			       (gnc:sum-collector-stocks 
				balance report-commodity exchange-fn)
			       #f #f row-style row-style
			       boldface? group-header-line?)
	  ;; Show no other currencies. Therefore just calculate
	  ;; one total via sum-collector-commodity and show it.
	  (add-row-helper! current-depth subtotal-name 
			   (gnc:sum-collector-commodity 
			    balance report-commodity exchange-fn)
			   #f 
			   row-style
			   boldface? group-header-line?)))

    (define (count-accounts! current-depth accnts)
      (if (<= current-depth tree-depth)
	  (let ((sum 0))
	    (for-each 
	     (lambda (acct)
	       (let ((subaccts (filter 
				use-acct?
				(gnc-account-get-children acct))))
		 (set! sum (+ sum  1))
		 (if (or (= current-depth tree-depth) (null? subaccts))
		     sum
		     (set! sum (+ sum (count-accounts! (+ 1 current-depth) subaccts))))))
	     accnts)
	    sum)
	  0))

    ;; This prints *all* the rows that belong to one group: the title
    ;; row, the subaccount tree, and the Total row with the balance of
    ;; the subaccounts. groupname may be a string or a html-text
    ;; object. subaccounts is a list of accounts. thisbalance is the
    ;; balance of this group, or it may be #f, in which case the
    ;; balance is calculated from the subaccounts list.
    (define (add-group! current-depth groupname subaccounts 
			thisbalance group-total-line?)
      (let ((heading-style (if (= current-depth 1)
				"primary-subheading"
				"secondary-subheading")))
	    
	;; first the group name
	(add-subtotal-row! current-depth groupname 
			   (and show-parent-balance? thisbalance) 
			   heading-style
			   (not (and show-parent-balance? thisbalance)) #t)
	;; then all the subaccounts
	(traverse-accounts! subaccounts (+ 1 current-depth))
	;; and now the "total" row
	(if group-total-line?
	    (begin
	      (remove-last-empty-row) ;; FIXME: do this here or not?
	      (add-subtotal-row! 
	       current-depth 
	       (let ((total-text (gnc:make-html-text (_ "Total") " ")))
		 (if (gnc:html-text? groupname)
		     (apply gnc:html-text-append! 
			    total-text
			    (gnc:html-text-body groupname))
		     (gnc:html-text-append! total-text groupname))
		 total-text)
	       ;; Calculate the balance, including the subbalances.
	       ;; A subbalance is only calculated if no thisbalance was
	       ;; given. (Because any "thisbalance" calculation already
	       ;; includes the appropriate subaccounts.)
	       (let ((subbalance (gnc:accounts-get-balance-helper 
				  subaccounts my-get-balance 
				  gnc-reverse-balance)))
		 (if thisbalance 
		     (gnc-commodity-collector-merge subbalance thisbalance))
		 subbalance)
	       heading-style
	       #t #f)))))
	      ;; and an empty line
	;      (add-subtotal-row! current-depth #f #f heading-style #f #f)))))

    ;; Adds rows to the table. Therefore it goes through the list of
    ;; accounts, runs add-account-rows! on each account.  If
    ;; tree-depth and current-depth require, it will recursively call
    ;; itself on the list of children accounts.
    (define (traverse-accounts! accnts current-depth)
      (let ((alternate #f))
      (if (<= current-depth tree-depth)
	  (for-each 
	   (lambda (acct)
	     (let ((subaccts (filter 
			      use-acct?
			      (gnc-account-get-children acct))))
	       (set! work-done (+ 1 work-done))
	       (if start-percent
		   (gnc:report-percent-done
		    (+ start-percent (* delta-percent (/ work-done work-to-do)))))
	       (if (or (= current-depth tree-depth) (null? subaccts))
		   (begin
		     (if (show-acct? acct)
			 (add-account-rows! acct current-depth alternate))
		     (set! alternate (not alternate)))
		   (add-group! current-depth 
			       (gnc:html-account-anchor acct)
			       subaccts
			       (gnc:accounts-get-balance-helper 
				(list acct) my-get-balance-nosub 
				gnc-reverse-balance)
			       show-parent-total?))))
	   (sort-fn accnts)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; start the recursive account processing
    (set! work-to-do (count-accounts!
		      (if group-types? 2 1)
		      (filter use-acct? topl-accounts)))
    (if group-types?
	;; Print a subtotal for each group.
	(for-each 
	 (lambda (accts) 
	   (if (and (not (null? accts)) (not (null? (cdr accts))))
	       (add-group! 1 
			   (gnc:account-get-type-string-plural (car accts))
			   (cdr accts) #f #t)))
	 (gnc:decompose-accountlist (lset-intersection 
				     equal? accounts topl-accounts)))
	;; No extra grouping. 
	;; FIXME: go through accounts even if not
	;; shown, because the children might be shown.
	(traverse-accounts! (filter use-acct? topl-accounts) 1))

    (remove-last-empty-row)

    ;; Show the total sum.
    (if show-total?
        (begin
	  (gnc:html-table-append-ruler/markup!
	   table "grand-total" (* (if show-other-curr? 3 2) tree-depth))
          (add-subtotal-row! 
           1 total-name 
           (get-total-fn (filter use-acct? topl-accounts) my-get-balance)
	   "grand-total"
           #t #f)))
    
    ;; set default alignment to right, and override for the name
    ;; columns
    (gnc:html-table-set-style! 
     table "td" 
     'attribute '("align" "right")
     'attribute '("valign" "top"))

    (gnc:html-table-set-style! 
     table "th" 
     'attribute '("align" "center")
     'attribute '("valign" "top"))

    ;; set some column headers 
    (if show-col-headers?
	(gnc:html-table-set-col-headers!
	 table 
	 (list (gnc:make-html-table-header-cell/size 
		1 tree-depth (_ "Account name"))
	       (gnc:make-html-table-header-cell/size
		1 (if show-other-curr? 
		      (* 2 tree-depth)
		      tree-depth)
		(_ "Balance")))))

    ;; No extra alignment here because that's already done in
    ;; html-acct-table-cell.

    table))


;; Create a html-table of all exchange rates. The report-commodity is
;; 'common-commodity', the exchange rates are given through the
;; function 'exchange-fn' and the 'accounts' determine which
;; commodities to show. Returns a html-object, a <html-table>.
(define (gnc:html-make-exchangerates
	 common-commodity exchange-fn accounts) 
  (let ((comm-list 
	 (gnc:accounts-get-commodities accounts common-commodity))
	(table (gnc:make-html-table)))

    (if (not (null? comm-list))
	;; Do something with each exchange rate.
	(begin
	  (for-each 
	   (lambda (commodity)
	     (let 
		 ;; slight hack: exchange a value greater than one,
		 ;; to get enough digits, and round later.
		 ((exchanged 
		   (exchange-fn 
		    (gnc:make-gnc-monetary commodity 
					   (gnc-numeric-create 1000 1))
		    common-commodity)))
	       (gnc:html-table-append-row! 
		table
		(list 
		 (gnc:make-gnc-monetary commodity 
					(gnc-numeric-create 1 1))
		 (gnc:make-gnc-monetary
		  common-commodity
		  (gnc-numeric-div
		   (gnc:gnc-monetary-amount exchanged)
		   (gnc-numeric-create 1000 1)
		   GNC-DENOM-AUTO 
		   (logior (GNC-DENOM-SIGFIGS 6) 
			   GNC-RND-ROUND)))))))
	   comm-list)
	  
	  ;; Set some style
	  (gnc:html-table-set-style! 
	   table "td" 
	   'attribute '("align" "right")
	   'attribute '("valign" "top"))
	  
	  ;; set some column headers 
	  (gnc:html-table-set-col-headers!
	   table 
	   (list (gnc:make-html-table-header-cell/size 
		  1 2 (if (= 1 (length comm-list))
			  (_ "Exchange rate")
			  (_ "Exchange rates")))))))
    
    table))


(define (gnc:html-make-generic-budget-warning report-title-string)
  (let ((p (gnc:make-html-text)))
    (gnc:html-text-append!
     p
     (gnc:html-markup-h2 (string-append (_ report-title-string) ":"))
     (gnc:html-markup-h2 "")
     (gnc:html-markup-p
       (_ "No budgets exist.  You must create at least one budget.")))
    p))


;; TODO: How 'bout factoring the "Edit report options" stuff out of
;; these 3 functions?

(define (gnc:html-make-generic-options-warning
	 report-title-string report-id)
  (let ((p (gnc:make-html-text)))
    (gnc:html-text-append!
     p
     (gnc:html-markup-h2 (string-append
			  (_ report-title-string)
			  ":"))
     (gnc:html-markup-h2 "")
     (gnc:html-markup-p
      (_ "This report requires you to specify certain report options.")))
    (if report-id
	(gnc:html-text-append!
	 p
	 (gnc:html-markup-p
	  (gnc:html-markup-anchor
	   (gnc-build-url URL-TYPE-OPTIONS
			       (string-append "report-id="
					      (sprintf #f "%a" report-id))
			       "")
	   (_ "Edit report options")))))
    p))


(define (gnc:html-make-no-account-warning
	 report-title-string report-id)
  (let ((p (gnc:make-html-text)))
    (gnc:html-text-append! 
     p 
     (gnc:html-markup-h2 (string-append 
			  (_ report-title-string)
			  ":"))
     (gnc:html-markup-h2 (_ "No accounts selected"))
     (gnc:html-markup-p
      (_ "This report requires accounts to be selected.")))
    (if report-id
	(gnc:html-text-append! 
	 p 
	 (gnc:html-markup-p
	  (gnc:html-markup-anchor
	   (gnc-build-url URL-TYPE-OPTIONS
			       (string-append "report-id="
					      (sprintf #f "%a" report-id))
			       "")
	   (_ "Edit report options")))))
    p))

(define (gnc:html-make-empty-data-warning 
	 report-title-string report-id)
  (let ((p (gnc:make-html-text)))
    (gnc:html-text-append! 
     p 
     (gnc:html-markup-h2 
      (string-append report-title-string ":"))
     (gnc:html-markup-h2 (_ "No data"))
     (gnc:html-markup-p
      (_ "The selected accounts contain no data/transactions (or only zeroes) for the selected time period")))
    (if report-id
	(gnc:html-text-append! 
	 p 
	 (gnc:html-markup-p
	  (gnc:html-markup-anchor
	   (gnc-build-url URL-TYPE-OPTIONS
			       (string-append "report-id="
					      (sprintf #f "%a" report-id))
			       "")
	   (_ "Edit report options")))))
    p))
