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
;; builds and returns a tree-(hierarchy-)shaped table as a html-table object
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ok, i will write more doc, later
(define (gnc:html-build-acct-table 
	 start-date end-date 
	 tree-depth show-subaccts? accounts 
	 show-total? do-subtot? 
	 show-other-curr? report-commodity exchange-fn)
  (let ((table (gnc:make-html-table))
	(topl-accounts (gnc:group-get-account-list 
			(gnc:get-current-group))))

    ;; If start-date == #f then balance-at-date will be used (for
    ;; balance reports), otherwise balance-interval (for profit and
    ;; loss reports). Returns a commodity-collector.
    (define (my-get-balance account)
      (if start-date
	  ;; FIXME: the get-balance-interval function uses this date
	  ;; rightaway, but since it calculates a difference it should
	  ;; rather take the end-day-time of one day before that. This
	  ;; needs to be fixed in report-utilities.scm.
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
    
    ;; returns a list which makes up a row in the table
    (define (make-row acct current-depth)
      (append
       (gnc:html-make-empty-cells (- current-depth 1))
       (list (gnc:make-html-table-cell/size 
	      1 (+ 1 (- tree-depth current-depth)) 
	      (gnc:html-account-anchor acct)))
       (gnc:html-make-empty-cells (- tree-depth current-depth))
       ;; the account balance
       (list 
	;; get the account balance, then exchange everything into the
	;; report-commodity via gnc:sum-collector-commodity. If the
	;; account-reverse-balance? returns true, then the sign gets
	;; reversed.
	((if (gnc:account-reverse-balance? acct)
	     gnc:monetary-neg
	     identity)
	 (gnc:sum-collector-commodity (my-get-balance acct) 
				      report-commodity exchange-fn)))
       (gnc:html-make-empty-cells (- current-depth 1))))
    
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
    
    ;; adds all appropriate rows to the table which belong to one
    ;; account, i.e. one row for each commodity. (Note: Multiple
    ;; commodities come from subaccounts with different commodities.) Is
    ;; used only if options "show foreign commodities" == #t.
    (define (add-commodity-rows! acct current-depth) 
      (let ((balance (my-get-balance acct)))
	;; the first row for each account: shows the name and the
	;; balance in the report-commodity
	(gnc:html-table-append-row! 
	 table
	 (append
	  (gnc:html-make-empty-cells (- current-depth 1))
	  (list (gnc:make-html-table-cell/size 
		 1 (+ 1 (- tree-depth current-depth)) 
		 (gnc:html-account-anchor acct)))
	  (gnc:html-make-empty-cells (* 2 (- tree-depth current-depth)))
	  (if (or do-subtot? 
		  (gnc:commodity-equiv? 
		   (gnc:account-get-commodity acct) 
		   report-commodity))
	      ;; usual case: the account balance in terms of report
	      ;; commodity
	      (list
	       (car (gnc:html-make-empty-cells 1))
	       (gnc:commodity-value->string 
		(balance 'getpair report-commodity 
			 (gnc:account-reverse-balance? acct))))
	      ;; special case if do-subtot? was false and it is in a
	      ;; different commodity than the report: then the
	      ;; foreign commodity gets displayed in this line
	      ;; rather then the following lines (loop below).
	      (let ((my-balance 
		     (balance 'getpair 
			      (gnc:account-get-commodity acct) 
			      (gnc:account-reverse-balance? acct))))
		(list 
		 (gnc:commodity-value->string my-balance)
		 (gnc:commodity-value->string 
		  (exchange-fn my-balance report-commodity)))))
	  (gnc:html-make-empty-cells (* 2 (- current-depth 1)))))
	;; The additional rows: show no name, but the foreign currency
	;; balance and its corresponding value in the
	;; report-currency. One row for each non-report-currency.
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
		       (list curr 
			     (if (gnc:account-reverse-balance? acct) 
				 (gnc:numeric-neg val) val)))
		      (gnc:commodity-value->string 
		       (exchange-fn 
			(list curr 
			      (if (gnc:account-reverse-balance? acct) 
				  (gnc:numeric-neg val) val))
			report-commodity)))
		     (gnc:html-make-empty-cells 
		      (* 2 (- current-depth 1))))))) 
	     #f))))
    
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

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; start the recursive account processing
    (if show-other-curr?
	(traverse-accounts-fcur! topl-accounts 1)
	(traverse-accounts! topl-accounts 1))

    ;; Show the total sum.
    (if show-total?
	(let ((total-collector (make-commodity-collector)))
	  (for-each (lambda (acct)
		      (total-collector 
		       (if (gnc:account-reverse-balance? acct)
			   'minusmerge
			   'merge)
		       (my-get-balance acct) #f))
		    (filter show-acct? topl-accounts))
	  (if show-other-curr?
	      (begin
		;; Show other currencies. Then show the report's
		;; currency in the first line.
		(gnc:html-table-append-row! 
		 table
		 (append (list (gnc:make-html-table-cell/size 
				1 tree-depth (_ "Total")))
			 (gnc:html-make-empty-cells 
			  (+ 1 (* 2 (- tree-depth 1))))
			 (list (gnc:commodity-value->string 
				(total-collector 'getpair 
						 report-commodity #f)))))
		;; Additional lines, one for each foreign currency.
		(total-collector
		 'format 
		 (lambda (curr val)
		   (if (gnc:commodity-equiv? curr report-commodity)
		       '()
		       (gnc:html-table-append-row! 
			table
			(append
			 ;; print no account name, and then leave
			 ;; subbalance columns empty, i.e. make
			 ;; tree-d + 2*(tree-d - 1) empty cells.
			 (gnc:html-make-empty-cells  
			  (- (* 3 tree-depth) 2))
			 (list
			  ;; print the account balance in the
			  ;; respective commodity
			  (gnc:commodity-value->string (list curr val))
			  (gnc:commodity-value->string 
			   (exchange-fn (list curr val) 
					report-commodity)))))))
		 #f))
	      ;; Show no other currencies. Then just calculate one
	      ;; total via sum-collector-commodity and show it.
	      (gnc:html-table-append-row! 
	       table
	       (append (list (gnc:make-html-table-cell/size 
			      1 tree-depth (_ "Total")))
		       (gnc:html-make-empty-cells (- tree-depth 1))
		       (list (gnc:sum-collector-commodity 
			      total-collector report-commodity 
			      exchange-fn)))))))
    
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

;; Print the exchangerate-alist into a given html-txt object. 
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
	      (gnc:numeric-convert 
	       ;; FIXME: remove the constant 100000
	       (cadr pair) 100000 GNC-RND-ROUND))))))
   alist))

