;; -*-scheme-*-
;; $Id$
;; Balance and Profit/Loss Reports

(gnc:support "report/balance-and-pnl.scm")
(gnc:depend "text-export.scm")
(gnc:depend "report-utilities.scm")
(gnc:depend "options.scm")


;; Just a private scope.
(let 
    ((l0-collector (make-stats-collector))
     (l1-collector (make-stats-collector))
     (l2-collector (make-stats-collector)))
  
  (define string-db (gnc:make-string-database))

  (define (balsht-options-generator)
    (define gnc:*balsht-report-options* (gnc:new-options))
    (define (gnc:register-balsht-option new-option)
      (gnc:register-option gnc:*balsht-report-options* new-option)) 

    (gnc:register-balsht-option
     (gnc:make-date-option
      "Report Options" "To"
      "a" "Calculate balance sheet up to this date"
      (lambda ()
        (let ((bdtime (localtime (current-time))))
          (set-tm:sec bdtime 59)
          (set-tm:min bdtime 59)
          (set-tm:hour bdtime 23)
          (let ((time (car (mktime bdtime))))
            (cons time 0))))
      #f))
    gnc:*balsht-report-options*)

  (define  (pnl-options-generator)
    (define gnc:*pnl-report-options* (gnc:new-options))
    (define (gnc:register-pnl-option new-option)
      (gnc:register-option gnc:*pnl-report-options* new-option))

    (gnc:register-pnl-option
     (gnc:make-date-option
      "Report Options" "From"
      "a" "Start of reporting period"
      (lambda ()
        (let ((bdtime (localtime (current-time))))
          (set-tm:sec bdtime 0)
          (set-tm:min bdtime 0)
          (set-tm:hour bdtime 0)
          (set-tm:mday bdtime 1)
          (set-tm:mon bdtime 0)
          (let ((time (car (mktime bdtime))))
            (cons time 0))))
      #f)) 

    (gnc:register-pnl-option
     (gnc:make-date-option
      "Report Options" "To"
      "b" "End of reporting period"
      (lambda ()
        (let ((bdtime (localtime (current-time))))
          (set-tm:sec bdtime 59)
          (set-tm:min bdtime 59)
          (set-tm:hour bdtime 23)
          (let ((time (car (mktime bdtime))))
            (cons time 0))))
      #f))
    gnc:*pnl-report-options*)

  (define (render-level-2-account level-2-account l2-value)
    (let ((account-name (string-append "&nbsp;&nbsp;&nbsp;&nbsp;"
                                       (gnc:account-get-full-name
                                        level-2-account)))
          (type-name (gnc:account-get-type-string
                      (gnc:account-get-type level-2-account))))
      (html-table-row-align
       (list
	account-name type-name (gnc:amount->formatted-string l2-value #f))
       (list "left" "center" "right"))))

  (define (render-level-1-account account l1-value)
    (let ((name (gnc:account-get-full-name account))
          (type (gnc:account-get-type-string (gnc:account-get-type account))))
      (html-table-row-align
       (list name type "&nbsp;"
             (gnc:amount->formatted-string l1-value #f)
	     "&nbsp;" "&nbsp;")
       (list "left" "center" "right" "right" "right" "right"))))

  (define (render-total l0-value)
    (html-table-row-align
     (list "&nbsp;" "&nbsp;" "&nbsp;"
           (html-strong (string-db 'lookup 'net))
           "&nbsp;"
           (gnc:amount->formatted-string l0-value #f))
     (list "left" "center" "right" "right" "right" "right")))

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

    (let* ((from-option (gnc:lookup-option options "Report Options" "From"))
           (from-value (if from-option (gnc:option-value from-option) #f))
           (to-value (gnc:option-value
                      (gnc:lookup-option options "Report Options" "To"))))

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
                (l1-collector 'add account-balance)
                (l1-collector 'add (l2-collector 'total #f))
                (l0-collector 'add (l1-collector 'total #f))
                (let ((level-1-output
                       (render-level-1-account account
                                               (l1-collector 'total #f))))
                  (l1-collector 'reset #f)
                  (l2-collector 'reset #f)
                  (if (null? childrens-output)
                      level-1-output
                      (list blank-line
                            level-1-output
                            childrens-output
                            blank-line)))))))

    (define (handle-level-2-account account options)
      (let
	  ((type (gnc:account-type->symbol (gnc:account-get-type account)))
	   (balance (make-stats-collector))
	   (rawbal
	    (if balance-sheet?
		(gnc:account-get-balance-at-date account to-value #f)
		(gnc:account-get-balance-interval 
		 account 
		 from-value
		 to-value #f))))
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
			    (if balance-sheet? 
				(gnc:group-get-balance-at-date grandchildren 
                                                               to-value)
				(gnc:group-get-balance-interval grandchildren
								from-value
								to-value)))))
	      (l2-collector 'add (balance 'total #f))
              (render-level-2-account account (balance 'total #f))))))

    (let
	((current-group (gnc:get-current-group))
	 (output '()))

      ;; Now, the main body
      ;; Reset all the balance collectors
      (l0-collector 'reset #f)
      (l1-collector 'reset #f)
      (l2-collector 'reset #f)
      (if (not (pointer-token-null? current-group))
	  (set! output
		(list
		 (gnc:group-map-accounts
		  (lambda (x) (handle-level-1-account x options))
		  current-group)
		 (render-total  (l0-collector 'total #f)))))

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
       "<th align=center>" (string-db 'lookup 'type)  "</th>"
       "<th align=right>" (string-db 'lookup 'subaccounts) "</th>"
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
