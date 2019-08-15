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

(use-modules (gnucash utilities))

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

(define (guid-ref idstr type guid)
  (gnc-build-url type (string-append idstr guid) ""))

(define (gnc:customer-anchor-text customer)
  (guid-ref "customer=" URL-TYPE-CUSTOMER (gncCustomerReturnGUID customer)))

(define (gnc:job-anchor-text job)
  (guid-ref "job=" URL-TYPE-JOB (gncJobReturnGUID job)))

(define (gnc:vendor-anchor-text vendor)
  (guid-ref "vendor=" URL-TYPE-VENDOR (gncVendorReturnGUID vendor)))

(define (gnc:employee-anchor-text employee)
  (guid-ref "employee=" URL-TYPE-EMPLOYEE (gncEmployeeReturnGUID employee)))

(define (gnc:invoice-anchor-text invoice)
  (guid-ref "invoice=" URL-TYPE-INVOICE (gncInvoiceReturnGUID invoice)))

(define (gnc:owner-anchor-text owner)
  (let ((type (gncOwnerGetType (gncOwnerGetEndOwner owner))))
    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (gnc:customer-anchor-text (gncOwnerGetCustomer owner)))

      ((eqv? type GNC-OWNER-VENDOR)
       (gnc:vendor-anchor-text (gncOwnerGetVendor owner)))

      ((eqv? type GNC-OWNER-EMPLOYEE)
       (gnc:employee-anchor-text (gncOwnerGetEmployee owner)))

      ((eqv? type GNC-OWNER-JOB)
       (gnc:job-anchor-text (gncOwnerGetJob owner)))

      (else
       ""))))

(define (gnc:owner-report-text owner acc)
  (let* ((end-owner (gncOwnerGetEndOwner owner))
	 (type (gncOwnerGetType end-owner))
	 (ref #f))

    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (set! ref "owner=c:"))

      ((eqv? type GNC-OWNER-VENDOR)
       (set! ref "owner=v:"))

      ((eqv? type GNC-OWNER-EMPLOYEE)
       (set! ref "owner=e:"))

      (else (set! ref "unknown-type=")))

    (if ref
	(begin
	  (set! ref (string-append ref (gncOwnerReturnGUID end-owner)))
	  (if (not (null? acc))
	      (set! ref (string-append ref "&acct="
				       (gncAccountGetGUID acc))))
	  (gnc-build-url URL-TYPE-OWNERREPORT ref ""))
	ref)))

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
  ;; default CSS colours
  ;; (define base-colors '("red" "orange" "yellow" "green"
  ;;                       "cyan" "blue" "purple" "magenta"
  ;;                       "orchid" "khaki" "gold" "orange"
  ;;                       "red3" "orange3" "yellow3" "green3"
  ;;                       "cyan3" "blue3" "purple3" "magenta3"
  ;;                       "orchid3" "khaki3" "gold3" "orange3"))

  ;; new base-colors from http://clrs.cc/ and flatuicolors.com
  (define base-colors (list "#FF4136" "#FF851B" "#FFDC00" "#2ECC40"
                            "#0074D9" "#001f3f" "#85144b" "#7FDBFF"
                            "#F012BE" "#3D9970" "#39CCCC" "#f39c12"
                            "#e74c3c" "#e67e22" "#9b59b6" "#8e44ad"
                            "#16a085" "#d35400"))
  (let lp ((i 0) (result '()) (colors base-colors))
    (cond
     ((<= num-colors i) (reverse result))
     ((null? colors)    (lp (1+ i) (cons (car base-colors) result) (cdr base-colors)))
     (else              (lp (1+ i) (cons (car colors) result) (cdr colors))))))

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
  (issue-deprecation-warning
   "gnc:html-table-append-ruler/markup! is unused.")
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
  (issue-deprecation-warning
   "gnc:html-acct-table-row-helper! is unused.")
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
  (issue-deprecation-warning
   "gnc:html-acct-table-comm-row-helper! is unused.")
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
	 (and balance
              (balance 'getmonetary report-commodity reverse-balance?))
	 main-row-style)
	;; Special case for stock-accounts: then the foreign commodity
	;; gets displayed in this line rather then the following lines
	;; (loop below). Is also used if is-stock-account? is true.
	(let ((my-balance
               (and balance
                    (balance 'getmonetary my-commodity reverse-balance?))))
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
	(balance
         'format
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
	 #f))))


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
;; non-leaf accounts separately.
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

(define (gnc:first-html-build-acct-table . args)
  (issue-deprecation-warning
   "gnc:first-html-build-acct-table is deprecated. use gnc:html-build-acct-table.")
  (apply gnc:html-build-acct-table args))

(define (gnc:html-build-acct-table 
	 start-date end-date 
	 tree-depth show-subaccts? accounts 
	 start-percent delta-percent
	 show-col-headers?
	 show-total? get-total-fn
	 total-name group-types? show-parent-balance? show-parent-total? 
	 show-other-curr? report-commodity exchange-fn show-zero-entries?)
  (issue-deprecation-warning
   "gnc:html-build-acct-table is unused.")
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
			 (this-collector 'merge x #f)))
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
		     (subbalance 'merge thisbalance #f))
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
(define (gnc:html-make-exchangerates common-commodity exchange-fn accounts)
  (let ((comm-list (gnc:accounts-get-commodities accounts common-commodity))
        (markup (lambda (c) (gnc:make-html-table-cell/markup "number-cell" c)))
        (table (gnc:make-html-table)))
    (unless (null? comm-list)
      (for-each
       (lambda (commodity)
         (let* ((orig-amt (gnc:make-gnc-monetary commodity 1))
                (exchanged (exchange-fn orig-amt common-commodity)))
           (gnc:html-table-append-row!
            table (map markup (list orig-amt exchanged)))))
       comm-list)
      (gnc:html-table-set-col-headers!
       table (list (gnc:make-html-table-header-cell/size
                    1 2 (if (null? (cdr comm-list))
                            (_ "Exchange rate")
                            (_ "Exchange rates"))))))
    table))


(define (gnc:html-make-generic-budget-warning report-title-string)
  (gnc:html-make-generic-simple-warning
    report-title-string
    (_ "No budgets exist. You must create at least one budget.")))


(define (gnc:html-make-generic-simple-warning report-title-string message)
  (let ((p (gnc:make-html-text)))
    (gnc:html-text-append!
     p
     (gnc:html-markup-h2 (string-append report-title-string ":"))
     (gnc:html-markup-h2 "")
     (gnc:html-markup-p message))
    p))


(define (gnc:html-make-options-link report-id)
   (if report-id
    (gnc:html-markup-p
     (gnc:html-markup-anchor
      (gnc-build-url URL-TYPE-OPTIONS
       (string-append "report-id=" (format #f "~a" report-id))
       "")
      (_ "Edit report options")))))

(define* (gnc:html-render-options-changed options #:optional plaintext?)
  ;; options -> html-object or string, depending on plaintext?.  This
  ;; summarises options that were changed by the user. Set plaintext?
  ;; to #t for unit-tests only.
  (define (disp d)
    ;; option-value -> string.  The option is passed to various
    ;; scm->string converters; ultimately a generic stringify
    ;; function handles symbol/string/other types.
    (define (try proc)
      ;; Try proc with d as a parameter, catching 'wrong-type-arg
      ;; exceptions to return #f to the or evaluator.
      (catch 'wrong-type-arg
        (lambda () (proc d))
        (const #f)))
    (or (and (boolean? d) (if d (_ "Enabled") (_ "Disabled")))
        (and (null? d) "null")
        (and (list? d) (string-join (map disp d) ", "))
        (and (pair? d) (format #f "~a . ~a"
                               (car d)
                               (if (eq? (car d) 'absolute)
                                   (qof-print-date (cdr d))
                                   (disp (cdr d)))))
        (try gnc-commodity-get-mnemonic)
        (try xaccAccountGetName)
        (try gnc-budget-get-name)
        (format #f "~a" d)))
  (let ((render-list '()))
    (define (add-option-if-changed option)
      (let* ((section (gnc:option-section option))
             (name (gnc:option-name option))
             (default-value (gnc:option-default-value option))
             (value (gnc:option-value option))
             (retval (cons (format #f "~a / ~a" section name)
                           (disp value))))
        (if (not (or (equal? default-value value)
                     (char=? (string-ref section 0) #\_)))
            (addto! render-list retval))))
    (gnc:options-for-each add-option-if-changed options)
    (if plaintext?
        (string-append
         (string-join
          (map (lambda (item)
                 (format #f "~a: ~a\n" (car item) (cdr item)))
               render-list)
          "")
         "\n")
        (apply
         gnc:make-html-text
         (apply
          append
          (map
           (lambda (item)
             (list
              (gnc:html-markup-b (car item))
              ": "
              (cdr item)
              (gnc:html-markup-br)))
           render-list))))))

(define (gnc:html-make-generic-warning
         report-title-string report-id
         warning-title-string warning-string)
  (let ((p (gnc:make-html-text)))
   (gnc:html-text-append!
    p
    (gnc:html-markup-h2 (string-append (_ report-title-string) ":"))
    (gnc:html-markup-h2 warning-title-string)
    (gnc:html-markup-p warning-string)
    (gnc:html-make-options-link report-id))
   p))

(define (gnc:html-make-generic-options-warning
         report-title-string report-id)
  (gnc:html-make-generic-warning
    report-title-string
    report-id
    ""
    (_ "This report requires you to specify certain report options.")))

(define (gnc:html-make-no-account-warning
         report-title-string report-id)
  (gnc:html-make-generic-warning
    report-title-string
    report-id
    (_ "No accounts selected")
    (_ "This report requires accounts to be selected in the report options.")))

(define (gnc:html-make-empty-data-warning
         report-title-string report-id)
  (gnc:html-make-generic-warning
    report-title-string
    report-id
    (_ "No data")
    (_ "The selected accounts contain no data/transactions (or only zeroes) for the selected time period")))

(define (gnc:html-js-include file)
  (format #f
          "<script language=\"javascript\" type=\"text/javascript\" src=\"file:///~a\"></script>\n"
          (gnc-path-find-localized-html-file file)))

(define (gnc:html-css-include file)
  (format #f
          "<link rel=\"stylesheet\" type=\"text/css\" href=\"file:///~a\" />\n"
          (gnc-path-find-localized-html-file file)))

;; function to sanitize strings prior to sending to html
(define (gnc:html-string-sanitize str)
  (with-output-to-string
    (lambda ()
      (string-for-each
       (lambda (c)
         (display
          (case c
            ((#\&) "&amp;")
            ((#\<) "&lt;")
            ((#\>) "&gt;")
            (else c))))
       str))))


