;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; account-summary.scm : brief account listing 
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
;; Copyright 2000-2001 Bill Gribble <grib@gnumatic.com>
;;
;; Even older original version by  Terry D. Boldt (tboldt@attglobal.net>
;;   Author makes no implicit or explicit guarantee of accuracy of
;;   these calculations and accepts no responsibility for direct
;;   or indirect losses incurred as a result of using this software.
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

(gnc:support "report/account-summary.scm")
(gnc:depend  "report-html.scm")

;; account summary report
;; prints a table of account information with clickable 
;; links to open the corresponding register window.

(let () 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; options generator
  ;; select accounts to report on, whether to show subaccounts,
  ;; whether to include subtotaled subaccount balances in the report,
  ;; and what date to show the summary for.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-options-generator)
    (let* ((options (gnc:new-options))
           (opt-register 
            (lambda (opt)
              (gnc:register-option options opt))))
      
      ;; date at which to report balance
      (opt-register 
       (gnc:make-date-option
        (_ "General") (_ "Date")
        "a" (_ "Select a date to report on")
	(lambda ()
	  (cons 'absolute 
		(gnc:timepair-end-day-time     
		 (gnc:secs->timepair 
		  (car (mktime (localtime (current-time))))))))
        #f 'absolute #f))
      
      ;; set of accounts to do report on 
      (opt-register 
       (gnc:make-account-list-option
        (_ "General") (_ "Account")
        "b" (_ "Report on these account(s)")
        (lambda ()
          (let ((current-accounts (gnc:get-current-accounts)))
            (cond ((not (null? current-accounts)) current-accounts)
                  (else
                   (gnc:group-get-account-list (gnc:get-current-group))))))
        #f #t))
      
      (opt-register 
       (gnc:make-multichoice-option
        (_ "General") (_ "Account Display Depth")
        "c" (_ "Show accounts to this depth.") 1
        (list (list->vector
	       (list 'all
		     (_ "All")
		     (_ "Show all accounts")))
	      (list->vector
               (list 1
                     "1"
                     (_ "Top-level")))
              (list->vector
               (list 2
                     "2"
                     (_ "Second-level")))
              (list->vector
               (list 3
                     "3"
                     (_ "Third-level")))
              (list->vector
               (list 4
                     "4"
                     (_ "Fourth-level"))))))
      
      (opt-register 
       (gnc:make-simple-boolean-option
        (_ "General") (_ "Include Sub-Account balances")
        "d" (_ "Include sub-account balances in printed balance?") #t))
      
      (opt-register
       (gnc:make-simple-boolean-option
	(_ "General") (_ "Show Foreign Currencies")
	"da" (_ "Display the account's foreign currency amount?") #f))
      
      (opt-register
       (gnc:make-currency-option 
	(_ "General") (_ "Report's currency") 
	"db" (_ "All other currencies will get converted to this currency.")
	(gnc:locale-default-currency)))
      
      options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Start of report generating code
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; build-acct-table
  ;; builds and returns the tree-shaped table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (build-acct-table accounts end-date tree-depth do-subtot?
			    show-fcur? report-commodity exchange-fn)
    (let ((table (gnc:make-html-table)))

      ;; The following functions are defined inside build-acct-table
      ;; to avoid passing tons of arguments which are constant anyway
      ;; inside this function.

      ;;;;;;;;;;;;;;;;;
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
	  ;; FIXME: report-commodity is ignored right now.
	  (let ((pair ((gnc:account-get-comm-balance-at-date 
			acct end-date do-subtot?) 
		       'getpair (gnc:account-get-commodity acct) #f)))
	    ;; pair is a list of one gnc:commodity and 
	    ;; one gnc:numeric value. 
	    (gnc:commodity-value->string pair)))
	 (gnc:html-make-empty-cells (- current-depth 1))))
      
      ;; Adds rows to the table. Therefore it goes through the list of
      ;; accounts, runs make-row on each account.  If tree-depth and
      ;; current-depth require, it will recursively call itself on the
      ;; list of children accounts. Is used if the foreign commodity is
      ;; not shown.
      (define (traverse-accounts! accnts current-depth)
	(if (<= current-depth tree-depth)
	    (map (lambda (acct)
		   (begin
		     (gnc:html-table-append-row!
		      table 
		      (make-row acct current-depth))
		     (let ((children 
			     (gnc:account-get-immediate-subaccounts acct)))
		       (if (not (null? children))
			   (traverse-accounts! 
			    children (+ 1 current-depth))))))
		 accnts)))
      
      ;;;;;;;;;;;;;;;;;;;
      ;; functions for table with foreign commodities visible

      ;; adds all appropriate rows to the table which belong to one
      ;; account, i.e. one row for each commodity. (Note: Multiple
      ;; commodities come from subaccounts with different commodities.) Is
      ;; used only if options "show foreign commodities" == #t.
      (define (add-commodity-rows! acct current-depth) 
	(let ((balance (gnc:account-get-comm-balance-at-date 
			acct end-date do-subtot?)))
	  ;; the first row for each account: shows the name and the
	  ;; balance in the report-commodity
	  (gnc:html-table-append-row! 
	   table
	   (append
	    (gnc:html-make-empty-cells (- current-depth 1))
	    (list (gnc:make-html-table-cell/size 
		   1 (+ 1 (- tree-depth current-depth)) 
		   (gnc:html-account-anchor acct)))
	    (gnc:html-make-empty-cells (+ 1 (* 2 (- tree-depth current-depth))))
	    ;; the account balance in terms of report commodity
	    (list
	     (gnc:commodity-value->string 
	      (balance 'getpair report-commodity #f)))
	    (gnc:html-make-empty-cells (* 2 (- current-depth 1)))))
	  ;; The additional rows: show no name, but the foreign currency
	  ;; balance and its corresponding value in the
	  ;; report-currency. One row for each non-report-currency.
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
	       (gnc:html-make-empty-cells  (* 2 (- tree-depth current-depth)))
	       ;; print the account balance in the respective commodity
	       (list
		(gnc:commodity-value->string (list curr val))
		(gnc:commodity-value->string 
		 (exchange-fn (list curr val) report-commodity)))
	       (gnc:html-make-empty-cells (* 2 (- current-depth 1))))))) 
       #f)))
  
      ;; The same as above, but for showing foreign currencies/commodities.
      (define (traverse-accounts-fcur! accnts current-depth) 
	(if (<= current-depth tree-depth)
	    (map (lambda (acct)
		   (begin
		     (add-commodity-rows! acct current-depth)
		     (let* ((children 
			     (gnc:account-get-immediate-subaccounts acct)))
		       (if (not (null? children))
			   (traverse-accounts-fcur! 
			    children (+ 1 current-depth))))))
		 accnts)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
      ;; start the recursive account processing
      (if show-fcur?
	  (traverse-accounts-fcur! accounts 1)
	  (traverse-accounts! accounts 1))
      
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
  
  ;; returns the maximum integer>0 in the given list... I'm
  ;; sure there is a predefined function for this task, but I don't
  ;; know where to look for that.
  (define (gnc:find-max-int l)
    (if (null? l)
	0
	(let ((a (gnc:find-max-int (cdr l))))
	  (if (> a (car l))
	      a
	      (car l)))))
  
  ;; return the number of children/depth of the given account tree
  ;; (needed if no tree-depth was specified)
  (define (gnc:accounts-get-children-depth tree)
    (gnc:find-max-int
     (map (lambda (acct)
	    (let ((children 
		   (gnc:account-get-immediate-subaccounts acct)))
	      (if (null? children)
		  1
		  (+ 1 (gnc:accounts-get-children-depth children)))))
	  tree)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; accsum-renderer
  ;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-renderer report-obj)
    (define (get-option optname)
      (gnc:option-value
       (gnc:lookup-option 
        (gnc:report-options report-obj) (_ "General") optname)))
    
    (let ((accounts (get-option (_ "Account")))
          (display-depth (get-option (_ "Account Display Depth")))
          (do-subtotals? (get-option (_ "Include Sub-Account balances")))
	  (show-fcur? (get-option (_ "Show Foreign Currencies")))
	  (report-currency (get-option (_ "Report's currency")))
	  ;; FIXME: So which splits are actually included and which
	  ;; are not??  Permanent repair (?): Change the semantics of
	  ;; the date-option to return not the first but the last
	  ;; second of the desired day.
          (date-tp (gnc:timepair-end-day-time 
		    (vector-ref (get-option (_ "Date")) 1)))
          (doc (gnc:make-html-document))
	  (txt (gnc:make-html-text)))
      
      (gnc:html-document-set-title! doc "Account Summary")
      (if (not (null? accounts))
	  ;; if no max. tree depth is given we have to find the
	  ;; maximum existing depth
	  (let* ((tree-depth (if (equal? display-depth 'all)
				 (gnc:accounts-get-children-depth accounts)
				 display-depth))
		 (exchange-alist (gnc:make-exchange-alist 
				  report-currency date-tp))
		 (exchange-fn (gnc:make-exchange-function exchange-alist))
		 ;; do the processing here
		 (table (build-acct-table 
			 accounts date-tp tree-depth do-subtotals?
			 show-fcur? report-currency exchange-fn)))

	    ;; set some column headers 
	    (gnc:html-table-set-col-headers!
	     table 
	     (list (gnc:make-html-table-header-cell/size 
		    1 tree-depth (_ "Account name"))
		   (gnc:make-html-table-header-cell/size
		    1 (if show-fcur? 
			  (* 2 tree-depth)
			  tree-depth)
		    (_ "Balance"))))
	    
	    ;; add the table 
	    (gnc:html-document-add-object! doc table)

	    ;; add the currency information
	    (for-each 
	     (lambda (pair)
	       (gnc:html-text-append! 
		txt
		(_ "Exchange rate ")
		(gnc:commodity-value->string 
		 (list (car pair) (gnc:numeric-create 1 1)))
		" = "
		(gnc:commodity-value->string 
		 (list report-currency 
		       (gnc:numeric-convert 
			;; FIXME: remove the constant 100000
			(cadr pair) 100000 GNC-RND-ROUND)))))
	     exchange-alist)

	    (if show-fcur?
		(gnc:html-document-add-object! doc txt)))
	  
	  ;; error condition: no accounts specified
          (let ((p (gnc:make-html-text)))
            (gnc:html-text-append! 
             p 
             (gnc:html-markup-h2 (_ "No accounts selected"))
             (gnc:html-markup-p
              (_ "This report requires accounts to be selected.")))
            (gnc:html-document-add-object! doc p)))      
      doc))
  
  (gnc:define-report 
   'version 1
   'name (_ "Account Summary")
   'options-generator accsum-options-generator
   'renderer accsum-renderer))
