;; -*-scheme-*-
;; $Id$
;; Balance and Profit/Loss Reports

(gnc:support "report/balance-and-pnl.scm")
(gnc:depend "text-export.scm")
(gnc:depend "report-utilities.scm")

(let 
    ((l0-collector (make-stats-collector))
     (l1-collector (make-stats-collector))
     (l2-collector (make-stats-collector)))
  ;; Just a private scope.

  (define (render-level-2-account level-2-account l2-value)
    (let ((account-name (gnc:account-get-name level-2-account))
          (type-name (gnc:account-get-type-string
                      (gnc:account-get-type level-2-account))))
      (html-table-row
       (list
	account-name type-name l2-value))))

  (define (render-level-1-account account l1-value l2-value)
    (let ((name (gnc:account-get-name account))
          (type (gnc:account-get-type-string (gnc:account-get-type account))))
      (html-table-row 
       (list name type l2-value l1-value
	     "&nbsp;" "&nbsp;"))))

  (define (render-total l0-value)
    (html-table-row (list "&nbsp;" "&nbsp;" "&nbsp;" (html-strong "Net")
			  "&nbsp;" l0-value)))

  (define (is-it-on-balance-sheet? type balance?)
    (eq? 
     (not (member type '(INCOME EXPENSE)))
     (not balance?)))

  (define (generate-balance-sheet-or-pnl report-name
					 report-description
					 balance-sheet?)
    ;; currency symbol that is printed is a dollar sign, for now
    ;; currency amounts get printed with two decimal places
    ;; balance sheet doesn't print income or expense
    ;; top-level accounts get printed in right-most column

    ;; This code could definitely be more "schemy", but for now I mostly
    ;; just translated it directly from the old ePerl with a few
    ;; schemifications.

  (define (handle-level-1-account account)
    (let ((type (gnc:account-type->symbol (gnc:account-get-type account))))
      (if (is-it-on-balance-sheet? type balance-sheet?)
	  ;; Ignore
	  '()
	  (let 
	      ((childrens-output (gnc:group-map-accounts
				  handle-level-2-account
				  (gnc:account-get-children account)))

	       (account-balance (gnc:account-get-balance account)))
	    
	    (if (not balance-sheet?)
		(set! account-balance (- account-balance)))
	    (l2-collector 'add account-balance)
	    (l1-collector 'add account-balance)
	    (l0-collector 'add (l1-collector 'total #f))
	    (let ((level-1-output 
		   (render-level-1-account account
					   (l1-collector 'total #f)
					   (l2-collector 'total #f))))
	      (l1-collector 'reset #f)
	      (l2-collector 'reset #f)
	      (list childrens-output level-1-output))))))

    (define (handle-level-2-account account)
      (let 
	  ((type (gnc:account-type->symbol (gnc:account-get-type account)))
	   (balance (make-stats-collector))
	   (rawbal (gnc:account-get-balance account)))
	(balance 'add 
		 (if balance-sheet? 
		     rawbal
		     (- rawbal)))
	(if (is-it-on-balance-sheet? type balance-sheet?)
	    ;; Ignore
	    '()
	    ;; add in balances for any sub-sub groups
	    (let ((grandchildren (gnc:account-get-children account)))
	      (if (not (pointer-token-null? grandchildren))
		  (balance 'add 
			   ((if balance-sheet? + -) 
			    0
			    (gnc:group-get-balance grandchildren))))
	      (l2-collector 'add (balance 'get #f))
	      (l1-collector 'add (l2-collector 'get #f))
	      (let 
		  ((result (render-level-2-account 
			    account (l2-collector 'get #f))))
		(l2-collector 'reset #f)
		result)))))
    (let
	((current-group (gnc:get-current-group))
	 (output '()))

	;;; Now, the main body
	;;; Reset all the balance collectors
      (l0-collector 'reset #f)
      (l1-collector 'reset #f)
      (l2-collector 'reset #f)
      (if (not (pointer-token-null? current-group))
	  (set! output
		(list
		 (gnc:group-map-accounts
		  handle-level-1-account 
		  current-group)
		 (render-total  (l0-collector 'total #f)))))

      (list
       "<html>"
       "<head>"
       "<title>" (gnc:_ report-name) "</title>"
       "</head>"

       "<body bgcolor=#ccccff>"
       (gnc:_ report-description)
       "<p>"

       "<table cellpadding=1>"
       "<caption><b>" (gnc:_ report-name) "</b></caption>"
       "<tr><th>"(gnc:_ "Account Name")"<th align=center>" (gnc:_ "Type")
       "<th> <th align=center>"(gnc:_ "Balance")

       output

       "</table>"
       "</body>"
       "</html>")))


  (gnc:define-report
     ;; version
     1
     ;; Menu name
     "Balance sheet"
     ;; Options Generator (none currently)
     #f
     ;; Code to generate the report   
     (lambda (options)
       (generate-balance-sheet-or-pnl "Balance Sheet"
				      "This page shows your net worth."
				      #t)))

    (gnc:define-report
     ;; version
     1
     ;; Menu name
     "Profit and Loss"
     ;; Options (none currently)
     #f
     ;; Code to generate the report   
     (lambda (options)
       (generate-balance-sheet-or-pnl 
	"Profit and Loss"
	"This page shows your profits and losses."
	#f))))
