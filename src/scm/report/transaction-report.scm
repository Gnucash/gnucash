;; -*-scheme-*-
;; transaction-report.scm
;; Report on all transactions in an account
;; Robert Merkel (rgmerk@mira.net)

(gnc:support "report/transaction-report.scm")
(gnc:depend "report-utilities.scm")
(gnc:depend "date-utilities.scm")
(gnc:depend "html-generator.scm")

(let ()

  ;; functions for manipulating total inflow and outflow counts.

  (define gnc:total-inflow 0)
  (define gnc:total-outflow 0)

  (define (gnc:set-total-inflow! x)
    (set! gnc:total-inflow x))

  (define (gnc:set-total-outflow! x)
    (set! gnc:total-outflow x))

  (define (gnc:tr-report-initialize-inflow-and-outflow!)
    (set! gnc:total-inflow 0)
    (set! gnc:total-outflow 0))

  ;; returns a list contains elements of the-list for which predicate is true
  (define (gnc:filter-list the-list predicate)
    (cond ((not (list? the-list))
           (gnc:error "Attempted to filter a non-list object"))
          ((null? the-list) '())
          ((predicate (car the-list))
           (cons (car the-list)
                 (gnc:filter-list (cdr the-list) predicate)))
          (else (gnc:filter-list (cdr the-list) predicate))))

  ;; like map, but restricted to one dimension, and
  ;; guaranteed to have inorder semantics.
  (define (gnc:inorder-map the-list fn)
    (cond ((not (list? the-list))
           (gnc:error "Attempted to map a non-list object"))
          ((not (procedure? fn))
           (gnc:error "Attempted to map a non-function object to a list"))
          ((null? the-list) '())
          (else (cons (fn (car the-list))
                      (gnc:inorder-map (cdr the-list) fn)))))

  ;; extract fields out of the scheme split representation

  (define (gnc:tr-report-get-memo split-scm)
    (vector-ref split-scm 0))

  (define (gnc:tr-report-get-action split-scm)
    (vector-ref split-scm 1))

  (define (gnc:tr-report-get-description split-scm)
    (vector-ref split-scm 2))

  (define (gnc:tr-report-get-date split-scm)
    (vector-ref split-scm 3))

  (define (gnc:tr-report-get-reconcile-state split-scm)
    (vector-ref split-scm 4))

  (define (gnc:tr-report-get-reconcile-date split-scm)
    (vector-ref split-scm 5))

  (define (gnc:tr-report-get-share-amount split-scm)
    (vector-ref split-scm 6))

  (define (gnc:tr-report-get-share-price split-scm)
    (vector-ref split-scm 7))

  (define (gnc:tr-report-get-value split-scm)
    (vector-ref split-scm 8))

  (define (gnc:tr-report-get-num split-scm)
    (vector-ref split-scm 9))

  (define (gnc:tr-report-get-other-splits split-scm)
    (vector-ref split-scm 10))

  (define (gnc:tr-report-get-first-acc-name split-scm)
    (let ((other-splits (gnc:tr-report-get-other-splits split-scm)))
      (cond ((= (length other-splits) 0) "-")
            (else  (caar other-splits)))))

  ;; builds a list of the account name and values for the other
  ;; splits in a transaction
  (define (gnc:split-get-corresponding-account-name-and-values 
           split split-filter) 
    (let* ((diff-list '())
           (parent-transaction (gnc:split-get-parent split))
           (num-splits (gnc:transaction-get-split-count parent-transaction)))
      (gnc:for-loop 
       (lambda (n) 
         (let* ((split-in-trans 
                 (gnc:transaction-get-split parent-transaction n))
                (sub-split
                 (list 
                  (gnc:split-get-account-name split-in-trans)
                  (gnc:split-get-value split-in-trans))))
           (if (split-filter sub-split)
               (set! diff-list
                     (cons sub-split diff-list)))))
       0 num-splits 1)
      (reverse diff-list)))


  ;; takes a C split, extracts relevant data and converts to a scheme 
  ;; representation.  split-filter is a predicate that filters the splits.
  (define (gnc:make-split-scheme-data split split-filter)
    (vector
     (gnc:split-get-memo split) 
     (gnc:split-get-action split)
     (gnc:split-get-description-from-parent split)
     (gnc:split-get-transaction-date split)
     (gnc:split-get-reconcile-state split)
     (gnc:split-get-reconciled-date split)
     (gnc:split-get-share-amount split)
     (gnc:split-get-share-price split)
     (gnc:split-get-value split)
     (gnc:transaction-get-num (gnc:split-get-parent split))
     (gnc:split-get-corresponding-account-name-and-values split split-filter)))

  ;; Note: This can be turned into a lookup table which will
  ;; *massively* simplify it...
  (define (gnc:sort-predicate-component component order)
    (let ((ascending-order-comparator
           (begin 
             (cond
              ((eq? component 'date) 
               (lambda (split-scm-a split-scm-b)
                 (- 
                  (car 
                   (gnc:timepair-canonical-day-time 
                    (gnc:tr-report-get-date split-scm-a)))
                  (car
                   (gnc:timepair-canonical-day-time
                    (gnc:tr-report-get-date split-scm-b))))))

              ((eq? component 'time) 
               (lambda (split-scm-a split-scm-b)
                 (-
                  (car (gnc:tr-report-get-date split-scm-a))
                  (car (gnc:tr-report-get-date split-scm-b)))))

              ((eq? component 'amount) 
               (lambda (split-scm-a split-scm-b)
                 (-
                  (gnc:tr-report-get-value split-scm-a)
                  (gnc:tr-report-get-value split-scm-b))))

              ((eq? component 'description)
               (lambda (split-scm-a split-scm-b)
                 (let ((description-a
                        (gnc:tr-report-get-description split-scm-a))
                       (description-b
                        (gnc:tr-report-get-description split-scm-b)))
                   (cond ((string<? description-a description-b) -1)
                         ((string=? description-a description-b) 0)
                         (else 1)))))

              ;; hack alert - should probably use something more sophisticated
              ;; here - perhaps even making it user-definable
              ((eq? component 'number)
               (lambda (split-scm-a split-scm-b)
                 (let ((num-a (gnc:tr-report-get-num split-scm-a))
                       (num-b (gnc:tr-report-get-num split-scm-b)))
                   (cond ((string<? num-a num-b) -1)
                         ((string=? num-a num-b) 0)
                         (else 1)))))

              ((eq? component 'corresponding-acc)
               (lambda (split-scm-a split-scm-b)
                 (let ((corr-acc-a
                        (gnc:tr-report-get-first-acc-name split-scm-a))
                       (corr-acc-b
                        (gnc:tr-report-get-first-acc-name split-scm-b)))
                   (cond ((string<? corr-acc-a corr-acc-b) -1)
                         ((string=? corr-acc-a corr-acc-b) 0)
                         (else 1)))))

              ((eq? component 'memo)
               (lambda (split-scm-a split-scm-b)
                 (let ((memo-a (gnc:tr-report-get-memo split-scm-a))
                       (memo-b (gnc:tr-report-get-memo split-scm-b)))
                   (cond ((string<? memo-a memo-b) -1)
                         ((string=? memo-a memo-b) 0)
                         (else 1)))))
              (else (gnc:error
                     (sprintf "transaction report: illegal sorting option %s"
                              (symbol->string (component)))))))))
      (cond ((eq? order 'descend) 
             (lambda (my-split-a my-split-b)
               (- (ascending-order-comparator my-split-a my-split-b))))
            (else ascending-order-comparator))))

  ;; returns a predicate
  (define (gnc:tr-report-make-sort-predicate
           primary-key-op primary-order-op secondary-key-op secondary-order-op)
    (let ((primary-comp (gnc:sort-predicate-component
                         (gnc:option-value primary-key-op)
                         (gnc:option-value primary-order-op)))
	  (secondary-comp (gnc:sort-predicate-component
			   (gnc:option-value secondary-key-op)
			   (gnc:option-value secondary-order-op))))
      (lambda (split-a split-b)  
        (let ((primary-comp-value (primary-comp split-a split-b)))
          (cond ((< primary-comp-value 0) #t)
                ((> primary-comp-value 0) #f)
                (else 
                 (let ((secondary-comp-value (secondary-comp split-a split-b)))
                   (cond ((< secondary-comp-value 0) #t)
                         (else #f)))))))))

  ;; returns a predicate that returns true only if a split-scm is
  ;; between early-date and late-date
  (define (gnc:tr-report-make-filter-predicate early-date late-date)
    (lambda (split-scm)
      (let ((split-date (gnc:tr-report-get-date split-scm)))
        (and (gnc:timepair-later-or-eq-date split-date early-date)
             (gnc:timepair-earlier-or-eq-date split-date late-date)))))

  ;; makes a predicate that returns true only if a sub-split account
  ;; does not match one of the accounts
  (define (gnc:tr-report-make-sub-split-filter-predicate accounts)
    (lambda (sub-split)
      (let loop
          ((list accounts))
        (if (null? list)
            #f
            (or (not (equal? (gnc:account-get-name (car list))
                             (car sub-split)))
                (loop (cdr list)))))))

  ;; converts a scheme split representation to a line of HTML,
  ;; updates the values of total-inflow and total-outflow based
  ;; on the split value
  (define (gnc:tr-report-split-to-html split-scm
                                       starting-balance)
    (let ((other-splits (gnc:tr-report-get-other-splits split-scm))
          (report-string ""))
      (cond ((> (gnc:tr-report-get-value split-scm) 0)
             (gnc:set-total-inflow! (+ gnc:total-inflow
                                       (gnc:tr-report-get-value split-scm))))
            (else 
             (gnc:set-total-outflow! (+ gnc:total-outflow
                                        (- (gnc:tr-report-get-value 
                                            split-scm))))))
      (for-each
       (lambda (split-sub first last)
         (set! report-string
               (string-append
                report-string
                "<TR><TD>"
                (cond (first (gnc:print-date
                              (gnc:tr-report-get-date split-scm)))
                      (else ""))
                "</TD><TD>"
                (cond (first (gnc:tr-report-get-num split-scm))
                      (else ""))
                "</TD><TD>"
                (cond (first (gnc:tr-report-get-description split-scm))
                      (else ""))
                "</TD><TD>"
                (cond (first (gnc:tr-report-get-memo split-scm))
                      (else ""))
                "</TD><TD>"
                (if (string? (car split-sub)) (car split-sub) "")
                "</TD><TD>"
                (cond ((< (cadr split-sub) 0)
                       (string-append
                        (gnc:amount->string (- (cadr split-sub)) #f #t #f)
                        "</TD><TD>"))
                      (else
                       (string-append
                        "</TD><TD>"	
                        (gnc:amount->string (cadr split-sub) #f #t #f))))
                "</TD>"
                (cond ((not last) "</TR>")
                      (else "")))))
       other-splits
       (if (null? other-splits)
           ()
           (append (list #t) (make-list (- (length other-splits) 1) #f)))
       (if (null? other-splits)
           ()
           (append (make-list (- (length other-splits) 1) #f) (list #t))))
      (string-append
       report-string
       "<TD>"
       (gnc:amount->string (- (+ starting-balance gnc:total-inflow)
                              gnc:total-outflow) #f #t #f)
       "</TD></TR>")))

  ;; gets the balance for a list of splits before beginning-date
  ;; hack alert -
  ;; we are doing multiple passes over the list - if it becomes a performance
  ;; problem some code optimisation will become necessary
  (define (gnc:tr-report-get-starting-balance scm-split-list beginning-date)
    (cond ((or 
            (eq? scm-split-list '())
            (gnc:timepair-later-date
             (gnc:tr-report-get-date (car scm-split-list))
             beginning-date))
           0)
          (else
           (+ 
            (gnc:tr-report-get-value 
             (car scm-split-list))
            (gnc:tr-report-get-starting-balance
             (cdr scm-split-list) beginning-date)))))

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
      "c" "Do transaction report on this account"
      (lambda ()
        (let ((current-accounts (gnc:get-current-accounts))
              (num-accounts (gnc:group-get-num-accounts
                             (gnc:get-current-group)))
              (first-account (gnc:group-get-account
                              (gnc:get-current-group) 0)))
          (cond ((not (null? current-accounts)) (list (car current-accounts)))
                ((> num-accounts 0) (list first-account))
                (else ()))))
      #f #f))

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
    (gnc:tr-report-initialize-inflow-and-outflow!)
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
           (prefix  (list "<HTML>" "<BODY bgcolor=#99ccff>"
                          "<TABLE>" (gnc:titles)))
           (suffix (list "</TABLE>" "</BODY>" "</HTML>"))
           (balance-line '())
           (inflow-outflow-line '())
           (net-inflow-line '())
           (report-lines '())
           (accounts (gnc:option-value tr-report-account-op))
           (date-filter-pred (gnc:tr-report-make-filter-predicate
                              (gnc:option-value begindate) 
                              (gnc:option-value enddate)))
           (sub-split-filter-pred
            (gnc:tr-report-make-sub-split-filter-predicate accounts))
           (starting-balance 0))
      (if (null? accounts)
          (set! report-lines
                (list
                 "<TR><TD>" (string-db 'lookup 'no-accounts) "</TD></TR>"))
          (begin
            ; reporting on more than one account not yet supported
            (gnc:for-each-split-in-account
             (car accounts)
             (lambda (split)		
               (set! report-lines 
                     (append! report-lines 
                              (list (gnc:make-split-scheme-data 
                                     split sub-split-filter-pred))))))
            (set! starting-balance
                  (gnc:tr-report-get-starting-balance
                   report-lines (gnc:option-value begindate)))	
            (set! report-lines (gnc:filter-list report-lines
                                                date-filter-pred))
            (set! report-lines
                  (sort!
                   report-lines 
                   (gnc:tr-report-make-sort-predicate
                    tr-report-primary-key-op tr-report-primary-order-op
                    tr-report-secondary-key-op tr-report-secondary-order-op)))
            (let ((html-mapper (lambda (split-scm)
                                 (gnc:tr-report-split-to-html
                                  split-scm
                                  starting-balance))))
              (set! report-lines (gnc:inorder-map report-lines html-mapper)))
            (set!
             balance-line 
             (list "<TR><TD><STRONG>"
                   (gnc:print-date (gnc:option-value begindate))
                   "</STRONG></TD>"
                   "<TD></TD>" 
                   "<TD></TD>"
                   "<TD></TD>"
                   "<TD></TD>"
                   "<TD></TD>"
                   "<TD></TD>"
                   "<TD><STRONG>"
                   (gnc:amount->string starting-balance #f #t #f)
                   "</STRONG></TD></TR>"))
            (set!
             inflow-outflow-line
             (list "<TR><TD><STRONG>"
                   (string-db 'lookup 'totals)
                   "</STRONG></TD>"
                   "<TD></TD>" 
                   "<TD></TD>"
                   "<TD></TD>"
                   "<TD></TD>"
                   "<TD><STRONG>"
                   (gnc:amount->string gnc:total-inflow #f #t #f)
                   "</TD></STRONG>"
                   "<TD><STRONG>"
                   (gnc:amount->string gnc:total-outflow #f #t #f)
                   "</TD></STRONG>"
                   "<TD></TD></TR>"))
            (set!
             net-inflow-line
             (list "<TR><TD><STRONG>"
                   (string-db 'lookup 'net-inflow)
                   "</STRONG></TD>"
                   "<TD></TD>"
                   "<TD></TD>" 
                   "<TD></TD>" 
                   "<TD></TD>"
                   "<TD></TD>" 
                   "<TD></TD>"
                   "<TD><STRONG>"
                   (gnc:amount->string (- gnc:total-inflow gnc:total-outflow)
                                       #f #t #f)
                   "</TD></STRONG></TR>"))))
      (append prefix balance-line report-lines
              inflow-outflow-line net-inflow-line suffix)))

  (string-db 'store 'date        "Date")
  (string-db 'store 'num         "Num")
  (string-db 'store 'desc        "Description")
  (string-db 'store 'memo        "Memo")
  (string-db 'store 'category    "Category")
  (string-db 'store 'credit      "Credit")
  (string-db 'store 'debit       "Debit")
  (string-db 'store 'balance     "Balance")
  (string-db 'store 'no-accounts "There are no accounts to report on.")
  (string-db 'store 'totals      "Totals")
  (string-db 'store 'net-inflow  "Net Inflow")

  (gnc:define-report
   ;; version
   1
   ;; Name
   "Account Transactions"
   ;; Options
   trep-options-generator
   ;; renderer
   gnc:trep-renderer))
