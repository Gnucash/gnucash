;; -*-scheme-*-
;; transaction-report.scm
;; Report on all transactions in an account
;; Robert Merkel (rgmerk@mira.net)

(gnc:support "report/transaction-report.scm")
(gnc:depend "report-utilities.scm")
(gnc:depend "date-utilities.scm")
(gnc:depend "html-generator.scm")

(let ()

  (define (trans-report-make-split-parent-proc parent-proc)
    (lambda (split)
      (parent-proc (gnc:split-get-parent split))))

  ;; given a split, find the account-full-name from the other split.
  ;; not useful when there is more than one split in a transaction
  (define (split-get-other-account-full-name split)
    (gnc:account-get-full-name
     (gnc:split-get-account
      (let ((trans (gnc:split-get-parent split)))
	(let ((split0 (gnc:transaction-get-split trans 0))
	      (split1 (gnc:transaction-get-split trans 1)))
	  (if (equal? split0 split) split1 split0))))))
  
  (define trans-report-specs
    (list
     (make-report-spec 
      "Date"
      (trans-report-make-split-parent-proc gnc:transaction-get-date-posted)
      (html-make-left-cell
       (lambda (date) (html-string (gnc:print-date date))))
      #f ; total-proc
      #f ; subtotal-html-proc
      #f ; total-html-proc
      #t ; first-last-preference
      #f ; subs-list-proc
      #f) ; subentry-html-proc
     (make-report-spec 
      "Num"
      (trans-report-make-split-parent-proc gnc:transaction-get-num)
      (html-make-left-cell html-string)
      #f ; total-proc
      #f ; subtotal-html-proc
      #f ; total-html-proc
      #t ; first-last-preference
      #f ; subs-list-proc
      #f) ; subentry-html-proc
     (make-report-spec 
      "Description"
      (trans-report-make-split-parent-proc gnc:transaction-get-description)
      (html-make-left-cell html-string)
      #f ; total-proc
      #f ; subtotal-html-proc
      #f ; total-html-proc
      #t ; first-last-preference
      #f ; subs-list-proc
      #f) ; subentry-html-proc
     (make-report-spec 
      "Memo"
      gnc:split-get-memo
      (html-make-left-cell html-string)
      #f ; total-proc
      #f ; subtotal-html-proc
      #f ; total-html-proc
      #t ; first-last-preference
      #f ; subs-list-proc
      #f) ; subentry-html-proc
     (make-report-spec 
      "Account"
      split-get-other-account-full-name
      (html-make-left-cell html-string)			   
      #f ; total-proc
      #f ; subtotal-html-proc
      #f ; total-html-proc
      #t ; first-last-preference
      #f ; subs-list-proc
      #f) ; subentry-html-proc
     (make-report-spec
      "Amount"
      gnc:split-get-value
      (html-make-right-cell html-currency)
      + ; total-proc
      (html-make-right-cell (html-make-strong html-currency))
      (html-make-right-cell (html-make-strong html-currency))
      #t ; first-last-preference
      #f ; subentry-list-proc
      #f))) ; subentry-html-proc

  (define trans-report-sort-specs
    (list
     (make-report-sort-spec
      (lambda (split) (gnc:account-get-full-name (gnc:split-get-account split)))
      string-ci<?
      string-ci=?
      string-ci=?
      (lambda (x) x))
     (make-report-sort-spec
      (trans-report-make-split-parent-proc gnc:transaction-get-date-posted)
      (lambda (a b) (< (car a) (car b)))
      (lambda (a b) (= (car a) (car b)))
      #f
      #f)))

  (define (make-split-list account split-filter-pred)
    (remove-if-not 
     split-filter-pred 
     (gnc:account-get-split-list account)))

  ;; returns a predicate that returns true only if a split-scm is
  ;; between early-date and late-date
  (define (gnc:tr-report-make-date-filter-predicate begin-date-secs end-date-secs)
    (lambda (split) 
      (let ((date 
	     (car (gnc:timepair-canonical-day-time 
		   (gnc:transaction-get-date-posted
		    (gnc:split-get-parent split))))))
	(and (>= date begin-date-secs)
	     (<= date end-date-secs)))))

  ;; register a configuration option for the transaction report
  (define (trep-options-generator)
    (define gnc:*transaction-report-options* (gnc:new-options))
    (define (gnc:register-trep-option new-option)
      (gnc:register-option gnc:*transaction-report-options* new-option))
    ;; from date
    ;; hack alert - could somebody set this to an appropriate date?
    (gnc:register-trep-option
     (gnc:make-date-option
      "Report Options" "From"
      "a" "Report Items from this date" 
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
    ;; to-date
    (gnc:register-trep-option
     (gnc:make-date-option
      "Report Options" "To"
      "b" "Report items up to and including this date"
      (lambda () (cons (current-time) 0))
      #f))

    ;; account to do report on
    (gnc:register-trep-option
     (gnc:make-account-list-option
      "Report Options" "Account"
      "c" "Do transaction report on these accounts"
      (lambda ()
        (let ((current-accounts (gnc:get-current-accounts))
              (num-accounts (gnc:group-get-num-accounts
                             (gnc:get-current-group)))
              (first-account (gnc:group-get-account
                              (gnc:get-current-group) 0)))
          (cond ((not (null? current-accounts)) (list (car current-accounts)))
                ((> num-accounts 0) (list first-account))
                (else ()))))
      #f #t))

    ;; primary sorting criterion
    (gnc:register-trep-option
     (gnc:make-multichoice-option
      "Sorting" "Primary Key"
      "a" "Sort by this criterion first"
      'date
      (list #(date
              "Date"
              "Sort by date")
            #(time
              "Time"
              "Sort by EXACT entry time")
            #(corresponding-acc
              "Transfer from/to"
              "Sort by account transferred from/to's name")
            #(amount
              "Amount"
              "Sort by amount")
            #(description
              "Description"
              "Sort by description")
            #(number
              "Number"
              "Sort by check/transaction number")
            #(memo
              "Memo"
              "Sort by memo"))))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      "Sorting" "Primary Sort Order"
      "b" "Order of primary sorting"
      'ascend
      (list
       #(ascend "Ascending" "smallest to largest, earliest to latest")
       #(descend "Descending" "largest to smallest, latest to earliest"))))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      "Sorting" "Secondary Key"
      "c"
      "Sort by this criterion second"
      'corresponding-acc
      (list #(date
              "Date"
              "Sort by date")
            #(time
              "Time"
              "Sort by EXACT entry time")
            #(corresponding-acc
              "Transfer from/to"
              "Sort by account transferred from/to's name")
            #(amount
              "Amount"
              "Sort by amount")
            #(description
              "Description"
              "Sort by description")
            #(number
              "Number"
              "Sort by check/transaction number")
            #(memo
              "Memo"
              "Sort by memo"))))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      "Sorting" "Secondary Sort Order"
      "d" "Order of Secondary sorting"
      'ascend
      (list
       #(ascend "Ascending" "smallest to largest, earliest to latest")
       #(descend "Descending" "largest to smallest, latest to earliest"))))

    gnc:*transaction-report-options*)

  (define string-db (gnc:make-string-database))

  (define (gnc:titles)
    (map (lambda (key) (string-append "<TH>" (string-db 'lookup key) "</TH>"))
         (list 'date 'num 'desc 'memo 'category 'credit 'debit 'balance)))

  (define (gnc:trep-renderer options)
    (let* ((begindate (gnc:lookup-option options "Report Options" "From"))
           (enddate (gnc:lookup-option options "Report Options" "To"))
           (tr-report-account-op (gnc:lookup-option
                                  options "Report Options" "Account"))
           (tr-report-primary-key-op (gnc:lookup-option options
                                                        "Sorting"
                                                        "Primary Key"))
           (tr-report-primary-order-op (gnc:lookup-option
                                        options "Sorting"
                                        "Primary Sort Order"))
           (tr-report-secondary-key-op (gnc:lookup-option options
                                                          "Sorting"
                                                          "Secondary Key"))
           (tr-report-secondary-order-op
            (gnc:lookup-option options "Sorting" "Secondary Sort Order"))
           (accounts (gnc:option-value tr-report-account-op))
           (date-filter-pred (gnc:tr-report-make-date-filter-predicate
                              (car (gnc:option-value begindate))
                              (car (gnc:option-value enddate))))	   
	   (split-list 
	    (apply
	     append
	     (map
	      (lambda (account)
		(make-split-list account date-filter-pred))
	      accounts))))
      (gnc:debug split-list)
      (list
       (html-start-document-title "Transaction Report")
	(html-para "Transaction report using the new reporting framework in html-generator.scm")
	(html-start-table)
	(html-table-headers trans-report-specs)
	(html-table-render-entries split-list
				   trans-report-specs
				   trans-report-sort-specs
				   html-table-entry-render-entries-only
				   #f)
	(html-table-totals split-list trans-report-specs)
	(html-end-table)
	(html-end-document))))

  (gnc:define-report
   'version 1
   'name "Transactions 2"
   'options-generator trep-options-generator
   'renderer gnc:trep-renderer))
