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
        (lambda () (cons 'absolute (cons (current-time) 0)))
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
      
      options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Start of report generating code
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; pair is a list of one gnc:commodity and one gnc:numeric
  ;; value. This function should disappear once this is an "official"
  ;; data type.
  (define (commodity-value->string pair)
    (gnc:commodity-amount->string 
     (cadr pair) (gnc:commodity-print-info (car pair) #t)))

  ;; returns a list with n #f (empty cell) values 
  (define (make-empty-cells n)
    (if (> n 0)
	(cons #f (make-empty-cells (- n 1)))
	'()))

  ;; returns a list which makes up a row in the table
  (define (make-row acct end-date tree-depth current-depth subtot?)
    (append
     (make-empty-cells (- current-depth 1))
     (list (gnc:make-html-table-cell/size 
	    1 (+ 1 (- tree-depth current-depth)) 
	    (gnc:make-html-text (gnc:html-markup-anchor
				 (string-append 
				  "gnc-register:account=" 
				  (gnc:account-get-full-name acct))
				 (gnc:account-get-name acct)))))
     (make-empty-cells (- tree-depth current-depth))
     ;; the account balance
     (list 
      (let ((pair ((gnc:account-get-comm-balance-at-date 
		    acct end-date subtot?) 
		   'getpair (gnc:account-get-commodity acct) #f)))
	;; pair is a list of one gnc:commodity and 
	;; one gnc:numeric value. 
	(commodity-value->string pair)))
     (make-empty-cells (- current-depth 1))))
  
  ;; Goes through the list of accounts, runs make-row on each account.
  ;; If tree-depth and current-depth require, it will recursively call
  ;; itself on the list of children accounts.
  (define (traverse-accounts accounts table end-date 
			     tree-depth current-depth subtot?)
    (if (<= current-depth tree-depth)
	(map (lambda (acct)
	       (begin
		 (gnc:html-table-append-row!
		  table 
		  (make-row acct end-date tree-depth current-depth 
			    subtot?))
		 (let* ((children 
			 (gnc:account-get-immediate-subaccounts acct)))
		   (if (not (null? children))
		       (traverse-accounts children table end-date
					  tree-depth (+ 1 current-depth)
					  subtot?)))))
	     accounts)))
	
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; build-acct-table
  ;; builds the tree-shaped table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (build-acct-table accounts end-date tree-depth do-subtot?)
    (let ((table (gnc:make-html-table)))

      ;; start the recursive account processing
      (traverse-accounts accounts table end-date tree-depth 1 do-subtot?)
            
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
  (define (find-max-int l)
    (if (null? l)
	0
	(let ((a (find-max-int (cdr l))))
	  (if (> a (car l))
	      a
	      (car l)))))
  
  ;; return the depth of the given account tree (needed if no
  ;; tree-depth was specified)
  (define (find-depth tree)
    (find-max-int
     (map (lambda (acct)
	    (let ((children 
		   (gnc:account-get-immediate-subaccounts acct)))
	      (if (null? children)
		  1
		  (+ 1 (find-depth children)))))
	  tree)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; accsum-renderer
  ;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-renderer options)
    (define (get-option optname)
      (gnc:option-value
       (gnc:lookup-option options (_ "General") optname)))
    
    (let ((accounts (get-option (_ "Account")))
          (date-tp (vector-ref (get-option (_ "Date")) 1))
          (display-depth (get-option (_ "Account Display Depth")))
          (do-subtotals? (get-option (_ "Include Sub-Account balances")))
          (doc (gnc:make-html-document)))
      
      (gnc:html-document-set-title! doc "Account Summary")
      (if (not (null? accounts))
	  ;; if no max. tree depth is given we have to find the
	  ;; maximum existing depth
	  (let ((tree-depth (if (equal? display-depth 'all)
				(find-depth accounts)
				display-depth)))
	    ;; do the (recursive) processing here
	    (let ((table (build-acct-table accounts date-tp 
					   tree-depth do-subtotals?)))
	      ;; set some column headers 
              (gnc:html-table-set-col-headers!
               table 
               (list (gnc:make-html-table-header-cell/size 
                      1 tree-depth (_ "Account name"))
                     (gnc:make-html-table-header-cell/size
                      1 tree-depth (_ "Balance"))))

              ;; add the table 
              (gnc:html-document-add-object! doc table)))	        

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
