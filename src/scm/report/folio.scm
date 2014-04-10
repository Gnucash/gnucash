(gnc:support "report/folio.scm")
(gnc:depend "report-utilities.scm")
(gnc:depend "html-generator.scm")

(let ()

  (define string-db (gnc:make-string-database))

  (define (folio-options-generator)

    (define gnc:*folio-report-options* (gnc:new-options))
    (define (gnc:register-folio-option new-option)
      (gnc:register-option gnc:*folio-report-options* new-option)) 

    (gnc:register-folio-option
     (gnc:make-date-option
      "Portfolio Options" "At"
      "a" "Calculate stock portfolio value at this date"
      (lambda ()
        (let ((bdtime (localtime (current-time))))
          (set-tm:sec bdtime 59)
          (set-tm:min bdtime 59)
          (set-tm:hour bdtime 23)
          (let ((time (car (mktime bdtime))))
            (cons 'absolute (cons time 0)))))
      #f 'absolute #f))

    gnc:*folio-report-options*)

  (define (titles)
    (map (lambda (key) (string-db 'lookup key))
         '(name ticker shares recent value cost profit-loss)))

  (define (gnc:account-get-last-split account)
    (let ((num-splits (gnc:account-get-split-count account)))
      (gnc:account-get-split account (if (> num-splits 0)
                                         (- num-splits 1)
                                         0))))

  (define (report-rows)

    (define total-value (make-stats-collector))
    (define total-cost (make-stats-collector))

    (define blank-row
      (list "&nbsp" "&nbsp" "&nbsp" "&nbsp" "&nbsp" "&nbsp" "&nbsp"))

    (define (report-row account)
      (let ((last-split (gnc:account-get-last-split account)))
        (let ((shares (gnc:split-get-share-balance last-split))
              (price (gnc:split-get-share-price last-split))
              (balance (gnc:split-get-balance last-split))
              (cost (gnc:split-get-cost-basis last-split)))

          (total-value 'add balance)
          (total-cost 'add cost)

          (list
           (gnc:account-get-name account)
           (gnc:account-get-security account)
           (gnc:amount->string shares #f #t #t)
           (gnc:amount->string price #f #t #f)
           (gnc:amount->string balance #f #t #f)
           (gnc:amount->string cost #f #t #f)
           (gnc:amount->string (- balance cost) #f #t #f)))))

    (define (net-row)
      (let ((value (total-value 'total #f))
            (cost (total-cost 'total #f)))
        (list (html-strong (string-db 'lookup 'net))
              "&nbsp" "&nbsp" "&nbsp"
              (gnc:amount->string value #f #t #f)
              (gnc:amount->string cost #f #t #f)
              (gnc:amount->string (- value cost) #f #t #f))))

    (define (report-rows-main)
      (gnc:group-map-all-accounts
       (lambda (account)
         (let ((type (gnc:account-type->symbol
                      (gnc:account-get-type account))))
           (if (member type '(STOCK MUTUAL))
               (report-row account)
               #f)))
       (gnc:get-current-group)))

    (define (collapse list collapsed)
      (cond ((null? list) collapsed)
            (else (collapse (cdr list)
                            (if (car list)
                                (cons (car list) collapsed)
                                collapsed)))))

    (let ((main-rows (collapse (report-rows-main) '())))
      (reverse (cons (net-row)
                     (cons blank-row main-rows)))))

  (define (folio-renderer options)
    (list
     (html-start-document-title (string-db 'lookup 'title) "#bfdeba")
     (html-table (string-db 'lookup 'title) (titles) (report-rows))
     (html-end-document)))

  (string-db 'store 'title "Stock Portfolio Valuation")
  (string-db 'store 'name "Name")
  (string-db 'store 'ticker "Ticker")
  (string-db 'store 'shares "Shares")
  (string-db 'store 'recent "Recent Price")
  (string-db 'store 'value "Value")
  (string-db 'store 'cost "Cost")
  (string-db 'store 'profit-loss "Profit/Loss")
  (string-db 'store 'net "Net")

  (gnc:define-report
   'version 1
   'name "Stock Portfolio"
   'renderer folio-renderer))
