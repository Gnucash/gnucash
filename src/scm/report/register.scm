;; -*-scheme-*-
;; register.scm

(require 'record)
(gnc:support "report/register.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")

(let ()

  (define-syntax addto!
    (syntax-rules ()
		  ((_ alist element) (set! alist (cons element alist)))))

  (define (set-last-row-style! table tag . rest)
    (let ((arg-list 
           (cons table 
                 (cons (- (gnc:html-table-num-rows table) 1)
                       (cons tag rest)))))
      (apply gnc:html-table-set-row-style! arg-list)))

  (define (used-date columns-used)
    (vector-ref columns-used 0))
  (define (used-num columns-used)
    (vector-ref columns-used 1))
  (define (used-description columns-used)
    (vector-ref columns-used 2))
  (define (used-account columns-used)
    (vector-ref columns-used 3))
  (define (used-shares columns-used)
    (vector-ref columns-used 4))	
  (define (used-price columns-used)
    (vector-ref columns-used 5))	
  (define (used-amount-single columns-used)
    (vector-ref columns-used 6))	
  (define (used-amount-double-positive columns-used)
    (vector-ref columns-used 7))	
  (define (used-amount-double-negative columns-used)
    (vector-ref columns-used 8))	
  (define (used-running-balance columns-used)
    (vector-ref columns-used 9))	

  (define columns-used-size 10)

  (define (num-columns-required columns-used)  
    (do ((i 0 (+ i 1)) 
         (col-req 0 col-req)) 
        ((>= i columns-used-size) col-req)
      (if (vector-ref columns-used i) (set! col-req (+ col-req 1)))))

  (define (build-column-used options)   
    (define (opt-val section name)
      (gnc:option-value 
       (gnc:lookup-option options section name)))

    (let ((column-list (make-vector columns-used-size #f)))
      (if (opt-val (N_ "Display") (N_ "Date"))
          (vector-set! column-list 0 #t))
      (if (opt-val (N_ "Display") (N_ "Num"))
          (vector-set! column-list 1 #t))
      (if (opt-val (N_ "Display") (N_ "Description"))
          (vector-set! column-list 2 #t))
      (if (opt-val (N_ "Display") (N_ "Account"))
          (vector-set! column-list 3 #t))
      (if (opt-val (N_ "Display") (N_ "Shares"))
          (vector-set! column-list 4 #t))
      (if (opt-val (N_ "Display") (N_ "Price"))
          (vector-set! column-list 5 #t))
      (let ((amount-setting (opt-val (N_ "Display") (N_ "Amount"))))
        (if (eq? amount-setting 'single)
            (vector-set! column-list 6 #t))
        (if (eq? amount-setting 'double)
            (begin 
	      (vector-set! column-list 7 #t)
	      (vector-set! column-list 8 #t))))
      (if (opt-val (N_ "Display") (N_ "Running Balance"))
          (vector-set! column-list 9 #t))

      column-list))

  (define (make-heading-list column-vector debit-string credit-string)
    (let ((heading-list '()))
      (gnc:debug "Column-vector" column-vector)
      (if (used-date column-vector)
	  (addto! heading-list (N_ "Date")))
      (if (used-num column-vector)
	  (addto! heading-list (N_ "Num")))
      (if (used-description column-vector)
	  (addto! heading-list (N_ "Description")))
      (if (used-account column-vector)
	  (addto! heading-list (N_ "Account")))
      (if (used-shares column-vector)
	  (addto! heading-list (N_ "Shares")))
      (if (used-price column-vector)
	  (addto! heading-list (N_ "Price")))
      (if (used-amount-single column-vector)
	  (addto! heading-list (N_ "Amount")))
      (if (used-amount-double-positive column-vector)
	  (addto! heading-list debit-string))
      (if (used-amount-double-negative column-vector)
	  (addto! heading-list credit-string))
      (if (used-running-balance column-vector)
	  (addto! heading-list (N_ "Balance")))
      (reverse heading-list)))

  (define (add-split-row table split column-vector row-style
                         transaction-info? split-info?)
    (let* ((row-contents '())
	   (parent (gnc:split-get-parent split))
	   (account (gnc:split-get-account split))
	   (currency (gnc:account-get-commodity account))
	   (damount (gnc:split-get-share-amount split))
	   (split-value (gnc:make-gnc-monetary currency damount)))

      (if (used-date column-vector)
	  (addto! row-contents
                  (if transaction-info?
                      (gnc:timepair-to-datestring 
                       (gnc:transaction-get-date-posted parent))
                      " ")))
      (if (used-num column-vector)
	  (addto! row-contents
                  (if transaction-info?
                      (gnc:transaction-get-num parent)
                      " ")))
      (if (used-description column-vector)
	  (addto! row-contents
                  (if transaction-info?
                      (gnc:transaction-get-description parent)
                      " ")))
      (if (used-account column-vector)
	  (addto! row-contents
                  (if split-info?
                      (gnc:account-get-name account)
                      " ")))
      (if (used-shares column-vector)
	  (addto! row-contents
                  (if split-info?
                      (gnc:split-get-share-amount split)
                      " ")))
      (if (used-price column-vector)
	  (addto! row-contents 
                  (if split-info?
                      (gnc:make-gnc-monetary
                       currency (gnc:split-get-share-price split))
                      " ")))
      (if (used-amount-single column-vector)
	  (addto! row-contents
                  (if split-info?
                      (gnc:html-split-anchor
                       split
                       (gnc:make-html-table-header-cell/markup "number-cell"
                                                               split-value))
                      " ")))
      (if (used-amount-double-positive column-vector)
	  (if (gnc:numeric-positive-p (gnc:gnc-monetary-amount split-value))
	      (addto! row-contents
                      (if split-info?
                          (gnc:make-html-table-header-cell/markup
                           "number-cell"
                           (gnc:html-split-anchor split split-value))
                          " "))
	      (addto! row-contents " ")))
      (if (used-amount-double-negative column-vector)
	  (if (gnc:numeric-negative-p (gnc:gnc-monetary-amount split-value))
	      (addto! row-contents
                      (if split-info?
                          (gnc:make-html-table-header-cell/markup
                           "number-cell"
                           (gnc:html-split-anchor
                            split (gnc:monetary-neg split-value)))
                          " "))
	      (addto! row-contents " ")))
      (if (used-running-balance column-vector)
	  (addto! row-contents
                  (if transaction-info?
                      (gnc:make-html-table-header-cell/markup
                       "number-cell"
                       (gnc:html-split-anchor
                        split
                        (gnc:make-gnc-monetary currency
                                               (gnc:split-get-balance split))))
                      " ")))

      (gnc:html-table-append-row! table (reverse row-contents))
      (apply set-last-row-style! (cons table (cons "tr" row-style)))
      split-value))

  (define (lookup-sort-key sort-option)
    (vector-ref (cdr (assq sort-option comp-funcs-assoc-list)) 0))
  (define (lookup-subtotal-pred sort-option)
    (vector-ref (cdr (assq sort-option comp-funcs-assoc-list)) 1))

  (define (reg-options-generator)

    (define gnc:*report-options* (gnc:new-options))

    (define (gnc:register-reg-option new-option)
      (gnc:register-option gnc:*report-options* new-option))

    (gnc:register-reg-option
     (gnc:make-internal-option "__reg" "query" #f))
    (gnc:register-reg-option
     (gnc:make-internal-option "__reg" "journal" #f))
    (gnc:register-reg-option
     (gnc:make-internal-option "__reg" "debit-string" (_ "Debit")))
    (gnc:register-reg-option
     (gnc:make-internal-option "__reg" "credit-string" (_ "Credit")))

    (gnc:register-reg-option
     (gnc:make-string-option
      (N_ "Report Options") (N_ "Title")
      "a" (N_ "The title of the report")
      (N_ "Register Report")))

    (gnc:register-reg-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Date")
      "b" (N_ "Display the date?") #t))

    (gnc:register-reg-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Num")
      "c" (N_ "Display the check number?") #t))

    (gnc:register-reg-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Description")
      "d" (N_ "Display the description?") #t))

    (gnc:register-reg-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Memo")
      "f" (N_ "Display the memo?") #t))

    (gnc:register-reg-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Account")
      "g" (N_ "Display the account?") #t))

    (gnc:register-reg-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Shares")
      "ha" (N_ "Display the number of shares?") #f))

    (gnc:register-reg-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Price")
      "hb" "Display the shares price?" #f))

    (gnc:register-reg-option
     (gnc:make-multichoice-option
      (N_ "Display") (N_ "Amount")
      "i" (N_ "Display the amount?")  
      'double
      (list
       (vector 'none (N_ "None") (N_ "No amount display"))
       (vector 'single (N_ "Single") (N_ "Single Column Display"))
       (vector 'double (N_ "Double") (N_ "Two Column Display")))))

    (gnc:register-reg-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Running Balance")
      "k" (N_ "Display a running balance") #f))

    (gnc:register-reg-option
     (gnc:make-simple-boolean-option
      (N_ "Display") (N_ "Totals")
      "l" (N_ "Display the totals?") #t))

    (gnc:register-reg-option
     (gnc:make-color-option
      (N_ "Colors")  (N_ "Split Odd")
      "c" (N_ "Background color for odd-numbered splits (or main splits in a
 multi-line report)")
      (list #xff #xff #xff 0)
      255 
      #f))

    (gnc:register-reg-option
     (gnc:make-color-option
      (N_ "Colors") (N_ "Split Even")
      "d" (N_ "Background color for even-numbered splits
 (or \"other\" splits in a multi-line report)")
      (list #xff #xff #xff 0)
      255
      #f))

    (gnc:register-reg-option
     (gnc:make-color-option
      (N_ "Colors") (N_ "Grand Total")
      "e" (N_ "Background color for total")
      (list #xff #xff #xff 0)
      255
      #f))

    (gnc:options-set-default-section gnc:*report-options* "Report Options")

    gnc:*report-options*)

  (define (display-date-interval begin end)
    (let ((begin-string (strftime "%x" (localtime (car begin))))
	  (end-string (strftime "%x" (localtime (car end)))))
      (sprintf #f (_ "From %s To %s") begin-string end-string)))

  (define (get-grand-total-style options)
    (let ((bgcolor (gnc:lookup-option options
                                      (N_ "Colors")
                                      (N_ "Grand Total"))))
      (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

  (define (get-odd-row-style options)
    (let ((bgcolor (gnc:lookup-option options
                                      (N_ "Colors")
                                      (N_ "Split Odd"))))
      (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

  (define (get-even-row-style options)
    (let ((bgcolor (gnc:lookup-option options
                                      (N_ "Colors")
                                      (N_ "Split Even"))))
      (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

  (define (make-split-table splits options debit-string credit-string)
    (define (opt-val section name)
      (gnc:option-value (gnc:lookup-option options section name)))

    (define (add-subtotal-row table width subtotal-collector subtotal-style)
      (let ((currency-totals (subtotal-collector
                              'format gnc:make-gnc-monetary #f))
            (blanks (make-list (- width 1) #f)))

        (gnc:html-table-append-row!
         table
         (list
          (gnc:make-html-table-cell/size
           1 width (gnc:make-html-text (gnc:html-markup-hr)))))

        (for-each (lambda (currency)
                    (gnc:html-table-append-row! 
                     table 
                     (append blanks
                             (list (gnc:make-html-table-header-cell/markup
                                    "number-cell" currency))))
                    (apply set-last-row-style! 
                           (cons table (cons "tr" subtotal-style))))
                  currency-totals)))

    (define (reg-report-journal?)
      (opt-val "__reg" "journal"))

    (define (add-other-split-rows split table used-columns row-style)
      (define (other-rows-driver split parent table used-columns i)
	(let ((current (gnc:transaction-get-split parent i)))
          (if current
              (begin
                (add-split-row table current used-columns row-style #f #t)
                (other-rows-driver split parent table
                                   used-columns (+ i 1))))))

      (other-rows-driver split (gnc:split-get-parent split)
                         table used-columns 0))

    (define (do-rows-with-subtotals splits 
                                    table 
                                    used-columns
                                    width
                                    multi-rows?
                                    odd-row?
                                    main-row-style
                                    alternate-row-style
                                    grand-total-style
                                    total-collector)
      (if (null? splits)
          (add-subtotal-row table width total-collector grand-total-style)

	  (let* ((current (car splits))
                 (current-row-style (if multi-rows? main-row-style
                                        (if odd-row? main-row-style 
                                            alternate-row-style)))
		 (rest (cdr splits))
		 (next (if (null? rest) #f
			   (car rest)))
	         (split-value (add-split-row table 
                                             current 
                                             used-columns 
                                             current-row-style
                                             #t
                                             (not multi-rows?))))

	    (if multi-rows?
                (add-other-split-rows 
                 current table used-columns alternate-row-style))

	    (total-collector 'add
			     (gnc:gnc-monetary-commodity split-value)
			     (gnc:gnc-monetary-amount split-value))

	    (do-rows-with-subtotals rest 
				    table 
				    used-columns
                                    width 
				    multi-rows?
                                    (not odd-row?)
                                    main-row-style
                                    alternate-row-style
                                    grand-total-style
				    total-collector))))

    (let* ((table (gnc:make-html-table))
           (used-columns (build-column-used options))
           (width (num-columns-required used-columns))
           (multi-rows? (reg-report-journal?))
           (grand-total-style 
            (get-grand-total-style options))
           (odd-row-style 
            (get-odd-row-style options))
           (even-row-style
            (get-even-row-style options)))

      (gnc:html-table-set-col-headers!
       table
       (make-heading-list used-columns debit-string credit-string))

      (do-rows-with-subtotals splits
                              table
                              used-columns
                              width
                              multi-rows?
                              #t
                              odd-row-style
                              even-row-style
                              grand-total-style
                              (gnc:make-commodity-collector))
      table))

  (define (reg-renderer report-obj)
    (define (opt-val section name)
      (gnc:option-value
       (gnc:lookup-option (gnc:report-options report-obj) section name)))

    (let ((document (gnc:make-html-document))
	  (splits '())
          (table '())
	  (query    (opt-val "__reg" "query"))
          (journal? (opt-val "__reg" "journal"))
          (debit-string (opt-val "__reg" "debit-string"))
          (credit-string (opt-val "__reg" "credit-string"))
          (title (opt-val "Report Options" "Title")))

      (gnc:query-set-group query (gnc:get-current-group))

      (set! splits (gnc:glist->list
                    (if journal?
                        (gnc:query-get-splits-unique-trans query)
                        (gnc:query-get-splits query))
                    <gnc:Split*>))

      (set! table (make-split-table splits
                                    (gnc:report-options report-obj)
                                    debit-string credit-string))

      (gnc:html-document-set-title! document title)
;      (gnc:html-document-add-object! 
;       document
;       (gnc:make-html-text
;	(gnc:html-markup-h3 (display-date-interval begindate enddate))))
      (gnc:html-document-add-object!
       document 
       table)

      document))

  (gnc:define-report
   'version 1
   'name (N_ "Register")
   'options-generator reg-options-generator
   'renderer reg-renderer
   'in-menu? #f))

(define (gnc:apply-register-report func query journal? title
                                   debit-string credit-string)
  (let* ((options (gnc:make-report-options "Register"))
         (query-op (gnc:lookup-option options "__reg" "query"))
         (journal-op (gnc:lookup-option options "__reg" "journal"))
         (title-op (gnc:lookup-option options "Report Options" "Title"))
         (debit-op (gnc:lookup-option options "__reg" "debit-string"))
         (credit-op (gnc:lookup-option options "__reg" "credit-string")))

    (gnc:option-set-value query-op query)
    (gnc:option-set-value journal-op journal?)
    (gnc:option-set-value title-op title)
    (gnc:option-set-value debit-op debit-string)
    (gnc:option-set-value credit-op credit-string)

    (func (gnc:make-report "Register" options))))

(define (gnc:show-register-report . rest)
  (apply gnc:apply-register-report (cons gnc:report-window rest)))

(define (gnc:print-register-report . rest)
  (apply gnc:apply-register-report (const gnc:print-report rest)))
