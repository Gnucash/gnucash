;; -*-scheme-*-
;; $Id$
;; by  Richard -Gilligan- Uschold 
;; Extensivbly modified from balance-and-pnl.scm
;;
;; This prints Tax related accounts and exports TXF files for import to
;; TaxCut, TurboTax, etc.
;;
;; It also prints a Hierarchical Report for any account.
;;
;; For this to work, the user has to segregate taxable and not taxable
;; income to different accounts, as well as deductible and non
;; deductible expenses.
;;
;; Tax related accounts have "{tax}" in the notes field.  This can be
;; set/reset from the parameters dialog.
;; The user selects the accounts(s) to be printed, if none, all are checked.
;; Automatically prints up to 15 sub-account levels below selected
;; account.  Accounts below that are not printed. If you really need
;; more levels, change the MAX_LEVELS constant
;;
;; Optionally, does NOT print accounts with $0.00 values.  Prints data
;; between the From and To dates.  Optional alternate periods:
;; "Last Year", "1st Est Tax Quarter", ... "4th Est Tax Quarter"
;; "Last Yr Est Tax Qtr", ... "Last Yr Est Tax Qtr"
;; Estimated Tax Quarters: Dec 31, Mar 31, Jun 30, Aug 31)
;; Optionally prints brief or full account names
;;
;; NOTE: setting of specific dates is squirly! and seems to be
;; current-date dependabnt!  Actually, time of day dependant!  Just
;; after midnight gives diffenent dates than just before!  Referencing
;; all times to noon seems to fix this.  Subtracting 1 year sometimes
;; subtracts 2!  see "(to-value"

(gnc:support "report/taxtxf.scm")
(gnc:depend "text-export.scm")
(gnc:depend "report-utilities.scm")
(gnc:depend "options.scm")
(gnc:depend "date-utilities.scm")
(gnc:depend "report/txf-export.scm")
(gnc:depend "report/txf-export-help.scm")

;; This and the next function are the same as in transaction-report.scm
(define (make-split-list account split-filter-pred)
  (let ((num-splits (gnc:account-get-split-count account)))
    (let loop ((index 0)
	       (split (gnc:account-get-split account 0))
	       (slist '()))
      (if (= index num-splits)
	  (reverse slist)
	  (loop (+ index 1)
		(gnc:account-get-split account (+ index 1))
		(if (split-filter-pred split)
		    (cons split slist)
		    slist))))))

;; returns a predicate that returns true only if a split is
;; between early-date and late-date
(define (split-report-make-date-filter-predicate begin-date-tp
						 end-date-tp)
  (lambda (split) 
    (let ((tp
	   (gnc:transaction-get-date-posted
	    (gnc:split-get-parent split))))
      (and (gnc:timepair-ge-date tp begin-date-tp)
	   (gnc:timepair-le-date tp end-date-tp)))))

;; This is nearly identical to, and could be shared with
;; display-report-list-item in report.scm. This adds warn-msg parameter
(define (gnc:display-report-list-item item port warn-msg)
  (cond
   ((string? item) (display item port))
   ((null? item) #t)
   ((list? item) (map (lambda (item) (gnc:display-report-list-item item port
								   warn-msg))
		      item))
   (else (gnc:warn warn-msg item " is the wrong type."))))

;; make a list of accounts from a group pointer
(define (gnc:group-ptr->list group-prt)
  (if (pointer-token-null? group-prt)
      '()
      (gnc:group-map-accounts (lambda (x) x) group-prt)))

;; some html helpers
(define (html-blue html)
  (if html
      (string-append  "<font color=\"#0000ff\">"  html "</font>")
      #f))

(define (html-red html)
  (if html
      (string-append  "<font color=\"#ff0000\">"  html "</font>")
      #f))

(define (html-black html)
  (if html
      (string-append  "<font color=\"#000000\">"  html "</font>")
      #f))

(define (html-table-row-align-color color lst align-list)
  (if (string? lst) 
      lst
      (list "<TR bgcolor=" color ">"
	    (map html-table-col-align lst align-list)
	    "</TR>")))

;; a few string functions I couldn't find elsewhere
(define (string-search string sub-str start)
  (do ((sub-len (string-length sub-str))
       ;; must recompute sub-len because order is unknown
       (limit (- (string-length string) (string-length sub-str)))
       (char0 (string-ref sub-str 0))
       ;; find first char of sub-str ; must recompute char0
       (match0 (string-index string (string-ref sub-str 0) start) ; init
	       (string-index string char0 (+ 1 match0))) ; step
       (match #f #f))
      ((or (not match0) (> match0 limit)
	   ;; does entire sub-str match?
	   (let ()
	     (set! match (string=? sub-str (substring string match0 
						      (+ match0 sub-len))))
	     (if match (set! match match0))
	     match))
       match)))

(define (string-search? string sub-str start)
  (number? (string-search string sub-str start)))

(define (string-substitute string search-str sub-str start)
  (let ((pos (string-search string search-str start)))
    (if pos
	(let ((search-len (string-length search-str)))
	  (string-append (substring string 0 pos) sub-str
			 (substring string (+ pos search-len))))
	string)))

(define (make-level-collector num-levels)
  (let ((level-collector (make-vector num-levels)))
    (do ((i 0 (+ i 1)))
        ((= i num-levels) i)
      (vector-set! level-collector i (make-stats-collector)))
    level-collector))

;; Just a private scope.
(let* ((MAX-LEVELS 16)			; Maximum Account Levels
       (levelx-collector (make-level-collector MAX-LEVELS))
       (bg-color "#f6ffdb")
       (white "#ffffff"))
  
  (define (lx-collector level action value)
    ((vector-ref levelx-collector (- level 1)) action value))

  (define string-db (gnc:make-string-database))

  ;; IRS asked congress to make the tax quarters sthe same as real quarters
  ;;   This is the year it is effective.  THIS IS A Y10K BUG!
  (define tax-qtr-real-qtr-year 10000)
  
  (define tax-tab-title "TAX Report Options")

  (define hierarchical-tab-title "Hierarchical Options")

  (define (tax-options-generator)
    (options-generator #f tax-tab-title))

  (define (hierarchical-options-generator)
    (options-generator #t hierarchical-tab-title))

  (define (options-generator  hierarchical? tab-title)
    (define gnc:*tax-report-options* (gnc:new-options))
    (define (gnc:register-tax-option new-option)
      (gnc:register-option gnc:*tax-report-options* new-option))
    
    
    (gnc:register-tax-option
     (gnc:make-date-option
      tab-title "From"
      "a" "Start of reporting period"
      (lambda ()
        (let ((bdtm (gnc:timepair->date (gnc:timepair-canonical-day-time
					 (cons (current-time) 0)))))
	  (set-tm:mday bdtm 1)  ; 01
          (set-tm:mon bdtm 0)   ; Jan
	  (cons 'absolute (cons (car (mktime bdtm)) 0))))
      #f 'absolute #f))
    
    (gnc:register-tax-option
     (gnc:make-date-option
      tab-title "To"
      "b" "End of reporting period"
      (lambda ()
        (cons 'absolute (gnc:timepair-canonical-day-time
			 (cons (current-time) 0))))
      #f 'absolute #f))
    
    (gnc:register-tax-option
     (gnc:make-multichoice-option
      tab-title "Alternate Period"
      "c" "Overide or modify From: & To:" 'from-to
      (list #(from-to "Use From - To" "Use From - To period")
            #(1st-est "1st Est Tax Quarter" "Jan 1 - Mar 31")
            #(2nd-est "2nd Est Tax Quarter" "Apr 1 - May 31")
            #(3rd-est "3rd Est Tax Quarter" "Jun 1 - Aug 31")
            #(4th-est "4th Est Tax Quarter" "Sep 1 - Dec 31")
            #(last-year "Last Year" "Last Year")
            #(1st-last "Last Yr 1st Est Tax Qtr" "Jan 1 - Mar 31, Last year")
            #(2nd-last "Last Yr 2nd Est Tax Qtr" "Apr 1 - May 31, Last year")
            #(3rd-last "Last Yr 3rd Est Tax Qtr" "Jun 1 - Aug 31, Last year")
            #(4th-last "Last Yr 4th Est Tax Qtr" "Sep 1 - Dec 31, Last year")
	    )))
    
    (gnc:register-tax-option
     (gnc:make-account-list-option
      tab-title "Select Accounts (none = all)"
      "d" "Select accounts"
      (lambda () (gnc:get-current-accounts))
      #f
      #t))
    
    (gnc:register-tax-option
     (gnc:make-simple-boolean-option
      tab-title "Suppress $0.00 values"
      "f" "$0.00 valued Accounts won't be printed." #t))
    
    (gnc:register-tax-option
     (gnc:make-simple-boolean-option
      tab-title "Print Full account names"
      "g" "Print all Parent account names" #f))
    
    (if (not hierarchical?)
	(begin
	  (gnc:register-tax-option
	   (gnc:make-multichoice-option
	    tab-title "Set/Reset Tax Status"
	    "h" "Set/Reset Selected Account Tax Status" 'tax-no-change
	    (list #(tax-no-change "No Change" "No Change")
		  #(tax-set "Set Tax Related" "Set Selected accounts as Tax\
 Related")
		  #(tax-reset "Reset Tax Related"
			      "Reset Selected accounts as not Tax Related")
		  #(tax-set-kids "Set Tax Related & sub-accounts" 
				 "Set Selected & sub-accounts as Tax Related")
		  #(tax-reset-kids
		    "Reset Tax Related & sub-accounts"
		    "Reset Selected & sub-accounts as not Tax Related")
		  )))
    
	  (gnc:register-tax-option
	   (gnc:make-account-list-option
	    "TXF Export Init" "Select Account"
	    "a" "Select Account"
	    (lambda () (gnc:get-current-accounts))
	    #f
	    #t))
	  
	  (gnc:register-tax-option
	   (gnc:make-simple-boolean-option
	    "TXF Export Init" "Print extended TXF HELP messages"
	    "b" "Print TXF HELP" #f))
	  
	  (gnc:register-tax-option
	   ;;(gnc:make-multichoice-option
	   (gnc:make-list-option
	    "TXF Export Init" "For INCOME accounts, select here.   < ^ #\
 see help"
	    "c" "Select a TXF Income catagory"
	    '()
	    txf-income-catagories
	    ))
	  
	  (gnc:register-tax-option
	   ;;(gnc:make-multichoice-option
	   (gnc:make-list-option
	    "TXF Export Init" "For EXPENSE accounts, select here.   < ^ #\
 see help"
	    "d" "Select a TXF Expense catagory"
	    '()
	    txf-expense-catagories
	    ))
    
	  (gnc:register-tax-option
	   (gnc:make-multichoice-option
	    "TXF Export Init" "< ^   Payer Name source"
	    "e" "Select the source of the Payer Name" 'default
	    (list #(default "Default" "Use Indicated Default")
		  #(current "< Current Account" "Use Current Account Name")
		  #(parent "^ Parent Account" "Use Parent Account Name")
		  )))))
    
    gnc:*tax-report-options*)
  
  (define tax-key "{tax}")
  
  (define tax-end-key "{/tax}")
  
  ;; Render txf information
  (define txf-last-payer "")		; if same as current, inc txf-l-coount
					; this only works if different
					; codes from the same payer are
					; grouped in the accounts list
  (define txf-l-count 0)		; count repeated N codes
  (define txf-notes "")			; tmp storage for account notes
  (define txf-pos 0)			; tmp storage for tax-end-key in notes
  (define tax-pos 0)			; tmp storage for tax-key in notes
  ;; stores assigned txf codes so we can check for duplicates
  (define txf-dups-alist '())

  (define (txf-payer? str)
    (member str '("<" "^")))
  
  ;; These gnc:account-get-xxx functions will be relpaced when the tax
  ;; and txf information gets its own account fields, and is no longer
  ;; in the notes field.

  ;; This is a bit of a fudge, matching against strings in account notes.
  ;; It'd be better if these were unique account fields.
  (define (gnc:account-get-tax account)
    (let* ((notes (gnc:account-get-notes account)))
      (string-search? (if notes notes "") tax-key 0)))
  
  (define (gnc:account-get-txf account)
    (let* ((notes (gnc:account-get-notes account)))
      (set! txf-notes (if notes notes ""))
      (set! txf-pos (string-search txf-notes tax-end-key 0))
      (if txf-pos
	  (begin (set! tax-pos (+ (string-search txf-notes tax-key 0) 
				  (string-length tax-key)))
		 #t)
	  #f)))
  
  ;; NOTE: You must call gnc:account-get-txf FIRST, or the txf-notes, tax-pos,
  ;;       and txf-pos variables will not be valid!!
  (define (gnc:account-get-txf-code account)
    (if txf-pos
	(substring txf-notes (- txf-pos 4) txf-pos)
	"000"))
  (define (gnc:account-get-txf-format account)
    (if txf-pos
	(string->number (substring txf-notes (- txf-pos 8) (- txf-pos 7)))
	0))
  (define (gnc:account-get-txf-payer-source account)
    (if txf-pos
	(substring txf-notes tax-pos (+ 1 tax-pos))
	" "))
  (define (gnc:account-get-txf-string account)
    (if txf-pos
	(substring txf-notes tax-pos txf-pos)
	" "))
  
  ;; because we use the list-option input structure, we have to build our own
  ;; search function
  (define (txfq-ref key txf-list)
    (do ((i 0 (+ i 1))
	 (len (length txf-list)))
	((or (>= i len) (eq? key (vector-ref (list-ref txf-list i) 0)))
	 (if (>= i len)
	     (list-ref txf-list 0)
	     (list-ref txf-list i)))))
  
  ;; return a string to insert in account-notes, or an error symbol
  ;; We only want one, but list-option returns a list.
  (define (txf-string code-lst catagories-lst)
    (cond ((or (null? code-lst)
	       (not (symbol? (car code-lst))))
	   'none)
	  ((> (length code-lst) 1)	; only allow ONE selection at a time
	   'mult)			; The GUI should exclude this
	  ((eq? 'N000 (car code-lst))
	   #f)
	  (else
	   (let ((txf-vec (txfq-ref (car code-lst) catagories-lst)))
	     (if txf-vec
		 (let ((str (vector-ref txf-vec 1)))
		   (if (equal? "#" (substring str 0 1))
		       'notyet		; not implimented yet
		       (string-append str " \\ "
				      (number->string (vector-ref txf-vec 3))
				      " \\ " (symbol->string 
					      (car code-lst))))))))))
  
  ;; insert help strings in txf catagories
  (define (txf-help cat-list)
    (do ((i 0 (+ i 1))
	 (len (length cat-list)))
	((>= i len))
      (let* ((item (list-ref cat-list i))
	     (code (vector-ref item 0))
	     (help (assq-ref txf-help-strings code)))
	(if help
	    (begin (array-set! item help 2)
		   (list-set! cat-list i item)
		   #t)
	    #f))))
  
  ;; print txf help strings
  (define (txf-print-help vect inc)
    (let* ((form-desc (vector-ref vect 1))
	   (code (symbol->string (vector-ref vect 0)))
	   (desc-len (string-length form-desc))
	   (bslash (string-search form-desc "\\" 0))
	   (form (substring form-desc 0 (+ bslash 2)))
	   (desc (substring form-desc (+ bslash 2) desc-len))
	   (form-code-desc (string-append 
			    "<b>" (string-substitute form "<" "&lt;" 0) "</b>"
			    code "<br>" desc))
	   (help (vector-ref vect 2)))
      (html-table-row-align (if inc
				(list (html-blue form-code-desc) 
				      (html-blue help))
				(list (html-red form-code-desc)
				      (html-red help)))
			    (list "left" "left"))))
  
  ;; Set or Reset txf string in account notes. str == #f resets.
  ;; Returns a code that indicates the function executed.
  (define (txf-status account key end-key str)
    (let ((key-len (string-length key))
	  (end-len (string-length end-key)))
      (let* ((notes (gnc:account-get-notes account))
	     (notes (if notes notes ""))
	     (key-start (string-search notes key 0))
	     (end-start (string-search notes end-key 0))
	     (notes-len (string-length notes)))
	
	;; 8 conditions: (key-start, end-start, str) function
	;;                   #f         #f      #f    nothing
	;;                   num        #f      #f    nothing
	;;                   #f         num     #f    nothing (illegal)
	;;                   #f         num     str   nothing (illegal)
	;;                   num        num     #f    reset
	;;                   num        num     str   replace
	;;                   #f         #f      str   set, tax too
	;;                   num        #f      str   set
	
	(if key-start
	    (let ((key-end (+ key-start key-len)))
	      (cond ((and end-start (not str))
		     ;; reset txf status
		     (let ((ret-val 'remove))
		       (gnc:account-set-notes 
			account (string-append (substring notes 0 key-end)
					       (substring 
						notes (+ end-start end-len)
						notes-len)))
		       ret-val))
		    ((and end-start str)
		     ;; replace txf status with str
		     (let ((ret-val 'replace))
		       (gnc:account-set-notes
			account (string-append (substring notes 0 key-end)
					       str
					       (substring notes end-start
							  notes-len)))
		       ret-val))
		    ((and (not end-start) str)
		     ;; set str and end-key
		     (let ((ret-val 'add))
		       (gnc:account-set-notes
			account (string-append (substring notes 0 key-end)
					       str end-key
					       (substring notes key-end
							  notes-len)))
		       ret-val))
		    (else
		     'none1)))
	    (if (and (not end-start) str)
		;; insert key, str and end-key
		(let ((ret-val 'both))
		  (gnc:account-set-notes account (string-append
						  key str end-key notes))
		  ret-val)
		'none2)))))
  
  ;; execute the selected function on the account.  Return a list
  ;; containing the function code executed and the txf-string or error message
  (define (txf-function acc txf-inc txf-exp txf-payer)
    (if acc
	(let ((txf-type (gnc:account-type->symbol
			 (gnc:account-get-type acc))))
	  (if (is-type-income-or-expense? txf-type)
	      (let* ((str (if (is-type-income? txf-type)
			      (txf-string txf-inc 
					  txf-income-catagories)
			      (txf-string txf-exp 
					  txf-expense-catagories)))
		     (fun (case str
			    ((mult)
			     (set! str
				   "multiple TXF codes were selected,")
			     'none)
			    ((none)
			     (set! str "no TXF code was selected,")
			     'none)
			    ((notyet)
			     (set! str
				   "selected TXF code is not implimented yet,")
			     'none)
			    (else 
			     (begin 
			       (if (and str (not (eq? txf-payer 'default))
					(txf-payer? (substring str 0 1)))
				   (let ((payer (case txf-payer
						  ((current) "<")
						  ((parent) "^")))
					 (len (string-length str)))
				     (set! str (string-append 
						payer 
						(substring str 1 len)))))
			       (txf-status acc tax-key tax-end-key str))))))
		(gnc:refresh-main-window)
		;; make "<" char html compatable
		(if str
		    (set! str (string-substitute str "<" "&lt;" 0)))
		(list fun str))
	      (list 'notIE "txf-account not of type income or expense")))
	(list 'noAcc "no txf-account")))
  
  ;; generate a feedback string for the txf function executed
  (define (txf-feedback-str fun-str full-name)
    (case (car fun-str)
      ((none none1 none2 notIE)
       (string-append "No TXF init function"
		      (if (cadr fun-str)
			  (string-append " because, " (cadr fun-str))
			  "")
		      " for account: \"" full-name "\""))
      ((noAcc)
       (string-append "No TXF init function because, " (cadr fun-str)))
      ((remove)
       (string-append "The TXF code was removed from account: \""
		      full-name "\""))
      ((replace)
       (string-append "The TXF code: \"" (cadr fun-str) "\", replaced the "
		      "existing code from account: \"" full-name "\""))
      ((add)
       (string-append "The TXF code: \"" (cadr fun-str)
		      "\", was added to account: \"" full-name "\""))
      ((both)
       (string-append "TAX status was set and the TXF code: \""
		      (cadr fun-str) "\", was added to account: \"" 
		      full-name "\""))))
  
  ;; check for duplicate txf codes
  (define (txf-check-dups account) 
    (let* ((code (string->symbol (gnc:account-get-txf-code account)))
	   (item (assoc-ref txf-dups-alist code))
	   (payer (gnc:account-get-txf-payer-source account)))
      (if (not (txf-payer? payer))
	  (set! txf-dups-alist (assoc-set! txf-dups-alist code
					   (if item
					       (cons account item)
					       (list account)))))))
  
  ;; Print error message for duplicate txf codes and accounts
  (define (txf-print-dups) 
    (let ((dups (apply append
		       (map (lambda (x)
			      (let ((cnt (length (cdr x))))
				(if (> cnt 1)
				    (let* ((acc (cadr x))
					   (txf (gnc:account-get-txf acc)))
				      (cons (string-append 
					     "Code \"" 
					     (gnc:account-get-txf-string acc)
					     "\" has duplicates in "
					     (number->string cnt) " accounts:")
					    (map gnc:account-get-full-name 
						 (cdr x))))
				    '())))
			    txf-dups-alist))))
      (if (not (null? dups))
	  (cons (html-para (html-blue (string-db 'lookup 'txf-dup)))
		(map html-para (map html-blue dups)))
	  '())))
  
  ;; some codes require special handling
  (define (txf-special-split? code)
    (member code '("N521")))	; only one for now
  
  (define (render-txf-account account account-value date)
    (let* ((print-info (gnc:account-value-print-info account #f))
	   (value (gnc:amount->string account-value print-info))
	   (txf? (gnc:account-get-txf account)))
      (if (and txf?
	       ;; (not (equal? account-value 0.0)) ; fails, round off, I guess
	       (not (equal? value (gnc:amount->string 0 print-info))))
	  (let* ((type (gnc:account-type->symbol (gnc:account-get-type
						  account)))
		 (code (gnc:account-get-txf-code account))
		 (date-str (if date
			       (strftime "%m/%d/%Y" (localtime (car date)))
			       #f))
		 ;; Only formats 1,3 implimented now! Others are treated as 1.
		 (format (gnc:account-get-txf-format account))
		 (payer-src (gnc:account-get-txf-payer-source account))
		 (account-name (if (equal? payer-src "^")
				   (gnc:account-get-name
				    (gnc:group-get-parent
				     (gnc:account-get-parent account)))
				   (gnc:account-get-name account))) 
		 (value (if (is-type-income? type) ; negate expenses
			    value
			    (string-append 
			     "$-" (substring value 1 (string-length value)))))
		 (l-value (if (txf-payer? payer-src)
			      (begin
				(set! txf-l-count 
				      (if (equal? txf-last-payer account-name)
					  txf-l-count
					  (+ 1 txf-l-count)))
				(set! txf-last-payer account-name)
				(number->string txf-l-count))
			      "1")))
	    (list (if date "\nTD" "\nTS") ; newlines are at the beginning
		  "\n" code		; because one is added at the end, and
		  "\nC1"		; TurboTax spits out an error msg for
		  "\nL" l-value		; a blank line at the end of the file.
		  (if date
		      (list "\nD" date-str)
		      '())
		  "\n" value
		  (case format
		    ((3) (list "\nP" account-name))
		    (else '()))
		  "\n^"))
	  "")))
  
  ;; Render any level
  (define (render-level-x-account level max-level account lx-value
				  suppress-0 full-names txf-date hierarchical?)
    (let* ((indent-1 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
	   (account-name (if txf-date	; special split
			     (strftime "%Y-%b-%d" (localtime (car txf-date)))
			     (if (or full-names (equal? level 1))
				 (gnc:account-get-full-name account)
				 (gnc:account-get-name account))))
	   (blue? (and (not hierarchical?) (gnc:account-get-txf account)))
	   (color (if (= level 1) white bg-color))
	   (print-info (gnc:account-value-print-info account #f))
	   (value (gnc:amount->string lx-value print-info))
	   (value-formatted (if blue?
				(html-blue value)
				(html-black value)))
	   (account-name (do ((i 1 (+ i 1))
			      (accum (if blue?
					 (html-blue account-name)
					 (html-black account-name))
				     (string-append indent-1 accum)))
			     ((>= i level) accum)))
           (nbsp-x-value (if (= max-level level)
			     (list value-formatted)
			     (append (vector->list (make-vector
						    (- max-level level)
						    "&nbsp;"))
				     (list value-formatted))))
	   (align-x (append (list "left")
	        	    (vector->list
			     (make-vector (- (+ max-level 1) level)
					  "right")))))
      (if (and blue? (not txf-date))	; check for duplicate txf codes
	  (txf-check-dups account))
      ;;(if (not (equal? lx-value 0.0)) ; this fails, round off, I guess
      (if (or (not suppress-0) (= level 1)
              (not (equal? value (gnc:amount->string 0 print-info))))
	  (html-table-row-align-color 
	   color 
	   (append (list account-name) nbsp-x-value) 
	   align-x)
	  '())))
  
  (define (is-type-income-or-expense? type)
    (member type '(INCOME EXPENSE)))
  
  (define (is-type-income? type)
    (member type '(INCOME)))
  
  ;; Recursivly validate children if parent is not a tax account.
  ;; Don't check children if parent is vaild.
  ;; Returns the Parent if a child or grandchild is valid.
  (define (validate accounts hierarchical?)
    (if hierarchical?
	accounts
	(apply append (map (lambda (a)
			     (if (gnc:account-get-tax a)
				 (list a)
				 ;; check children
				 (if (null? (validate
					     (gnc:group-ptr->list
					      (gnc:account-get-children a))
					     #f))
				     '()
				     (list a))))
			   accounts))))
  
  ;; Set or Reset key in account notes
  (define (key-status accounts set key end-key kids)
    (let ((key-len (string-length key)))
      (map (lambda (a)
	     (let* ((notes (gnc:account-get-notes a))
		    (notes (if notes notes ""))
		    (key-start (string-search notes key 0))
		    (notes-len (string-length notes)))
	       (if key-start
		   (if (not set)	; reset tax status
		       (let* ((end-start (string-search notes end-key 0))
			      (end (if end-start
				       (+ end-start (string-length end-key))
				       (+ key-start key-len))))
			 (gnc:account-set-notes a 
						(string-append
						 (substring notes 0 key-start)
						 (substring notes end
							    notes-len)))))
		   (if set		; set tax status
		       (gnc:account-set-notes a (string-append notes key))))
	       (if kids			; recurse to all sub accounta
		   (key-status 
		    (gnc:group-ptr->list (gnc:account-get-children a))
		    set key end-key #t))))
	   accounts)))
  
  (define (generate-tax-or-txf report-name
			       report-description
			       options
			       tax-mode-in)    

    ;; These are some helper functions for looking up option values.
    (define (get-op section name)
      (gnc:lookup-option options section name))
    
    (define (op-value section name)
      (gnc:option-value (get-op section name)))
    
    ;; the number of account generations: children, grandchildren etc.
    (define (num-generations account gen)
      (let ((children (gnc:account-get-children account)))
	(if (pointer-token-null? children)
	    (if (and (gnc:account-get-txf account)
		     (equal? "N521" (gnc:account-get-txf-code account)))
		(+ gen 1)		; Est Fed Tax has a extra generation
		gen)	       		; no kids, return input
	    (apply max (gnc:group-map-accounts
			(lambda (x) (num-generations x (+ 1 gen)))
			children)))))
        
    (let* ((hierarchical? (equal? (string-db 'lookup 'hierarchical-title)
				  report-name))
	   (tab-title (if hierarchical? hierarchical-tab-title tax-tab-title))
	   (from-value (gnc:date-option-absolute-time 
			(op-value tab-title "From")))
           (to-value (gnc:timepair-end-day-time
		      (gnc:date-option-absolute-time 		       
		       (op-value tab-title "To"))))
	   (alt-period (op-value tab-title "Alternate Period"))
	   (suppress-0 (op-value tab-title "Suppress $0.00 values"))
	   (full-names (op-value tab-title
				 "Print Full account names"))
	   (tax-mode tax-mode-in)	; these need to different later
	   (user-sel-accnts (op-value tab-title
				      "Select Accounts (none = all)"))
	   (valid-user-sel-accnts (validate user-sel-accnts hierarchical?))
	   ;; If no selected accounts, check all.
	   (selected-accounts (if (not (null? user-sel-accnts))
				  valid-user-sel-accnts
				  (validate (gnc:group-ptr->list
					     (gnc:get-current-group))
					    hierarchical?)))
	   (generations (if (pair? selected-accounts)
			    (apply max (map (lambda (x) (num-generations x 1))
					    selected-accounts))
			    0))
	   (max-level (min MAX-LEVELS (max 1 generations)))
	   
	   ;; Alternate dates are relative to from-date
	   (from-date (gnc:timepair->date from-value))
	   (from-value (gnc:timepair-start-day-time
			(let ((bdtm from-date))
			  (if (member alt-period 
				      '(last-year 1st-last 2nd-last
						  3rd-last 4th-last))
			      (set-tm:year bdtm (- (tm:year bdtm) 1)))
			  (set-tm:mday bdtm 1)
			  (if (< (gnc:date-get-year bdtm) 
				 tax-qtr-real-qtr-year)
			      (case alt-period
				((1st-est 1st-last last-year) ; Jan 1
				 (set-tm:mon bdtm 0))
				((2nd-est 2nd-last) ; Apr 1
				 (set-tm:mon bdtm 3))
				((3rd-est 3rd-last) ; Jun 1
				 (set-tm:mon bdtm 5))
				((4th-est 4th-last) ; Sep 1
				 (set-tm:mon bdtm 8)))
			      ;; Tax quaters equal Real quarters
			      (case alt-period
				((1st-est 1st-last last-year) ; Jan 1
				 (set-tm:mon bdtm 0))
				((2nd-est 2nd-last) ; Apr 1
				 (set-tm:mon bdtm 3))
				((3rd-est 3rd-last) ; Jul 1
				 (set-tm:mon bdtm 6))
				((4th-est 4th-last) ; Oct 1
				 (set-tm:mon bdtm 9))))
			  (cons (car (mktime bdtm)) 0))))
	   
	   (to-value (gnc:timepair-end-day-time
		      (let ((bdtm from-date))
			(if (member alt-period 
				    '(last-year 1st-last 2nd-last
						3rd-last 4th-last))
			    (set-tm:year bdtm (- (tm:year bdtm) 1)))
			;; Bug! Above subtracts two years, should only be one!
			;; The exact same code, in from-value, further above,
			;;   only subtraces one!  Go figure!
			;; So, we add one back below!
			(if (member alt-period 
				    '(last-year 1st-last 2nd-last
						3rd-last 4th-last))
			    (set-tm:year bdtm (+ (tm:year bdtm) 1)))
			(set-tm:mday bdtm 31)
			(if (< (gnc:date-get-year bdtm) tax-qtr-real-qtr-year)
			    (case alt-period
			      ((1st-est 1st-last) ; Mar 31
			       (set-tm:mon bdtm 2))
			      ((2nd-est 2nd-last) ; May 31
			       (set-tm:mon bdtm 4))
			      ((3rd-est 3rd-last) ; Aug 31
			       (set-tm:mon bdtm 7))
			      ((4th-est 4th-last last-year) ; Dec 31
			       (set-tm:mon bdtm 11))
			      (else (set! bdtm (gnc:timepair->date to-value))))
			    ;; Tax quaters equal Real quarters
			    (case alt-period
			      ((1st-est 1st-last) ; Mar 31
			       (set-tm:mon bdtm 2))
			      ((2nd-est 2nd-last) ; Jun 30
			       (set-tm:mday bdtm 30)
			       (set-tm:mon bdtm 5))
			      ((3rd-est 3rd-last) ; Sep 30
			       (set-tm:mday bdtm 30)
			       (set-tm:mon bdtm 8))
			      ((4th-est 4th-last last-year) ; Dec 31
			       (set-tm:mon bdtm 11))
			      (else 
			       (set! bdtm (gnc:timepair->date to-value)))))
			(cons (car (mktime bdtm)) 0))))

	   (txf-help (if hierarchical? #f
			 (op-value "TXF Export Init" "Print extended TXF HELP\
 messages")))
	   (txf-feedback-str-lst '()))
      
      (define (handle-txf-special-splits level account from-value to-value)
	(if (and (gnc:account-get-txf account)
		 (txf-special-split? (gnc:account-get-txf-code account)))
	    (let* 
		((full-year? 
		  (and (equal? (strftime "%Y" (localtime (car to-value)))
			       (strftime "%Y" (localtime (car from-value))))
		       (equal? (strftime "%m%d" (localtime (car from-value)))
			       "0101")
		       (equal? (strftime "%m%d" (localtime (car to-value)))
			       "1231")))
		 ;; Adjust dates so we get the final Estimated Tax
		 ;; paynemt from the right year
		 (from-est (if full-year?
			       (let ((bdtm (gnc:timepair->date
					    (gnc:timepair-canonical-day-time
					     from-value))))
				 (set-tm:mday bdtm 1)  ; 01
				 (set-tm:mon bdtm 2)   ; Mar
				 (cons (car (mktime bdtm)) 0))
			       from-value))
		 (to-est (if full-year?
			     (let* ((bdtm (gnc:timepair->date
					   (gnc:timepair-canonical-day-time
					    from-value))))
			       (set-tm:mday bdtm 28)  ; 28
			       (set-tm:mon bdtm 1)   ; Feb
			       (set-tm:year bdtm (+ (tm:year bdtm) 1))
			       (cons (car (mktime bdtm)) 0))
			     to-value))
		 (split-filter-pred (split-report-make-date-filter-predicate
				     from-est to-est))
		 (split-list (make-split-list account split-filter-pred))
		 (lev  (if (>= max-level (+ 1 level))
			   (+ 1 level)
			   level)))
	      (map (lambda (spl) 
		     (let* ((tmp-date (gnc:transaction-get-date-posted 
				       (gnc:split-get-parent spl)))
			    (value (gnc:numeric-to-double
				    (gnc:split-get-value spl)))
			    ;; TurboTax '99 ignores dates after 12/31/1999
			    (date (if (and full-year? 
					   (gnc:timepair-lt to-value tmp-date))
				      to-value
				      tmp-date)))
		       (if tax-mode
			   (render-level-x-account lev max-level account
						   value suppress-0 #f date #f)
			   (render-txf-account account value date))))
		   split-list))
	    '()))
      
      (define (handle-level-x-account level account)
	(let ((type (gnc:account-type->symbol (gnc:account-get-type account)))
	      (name (gnc:account-get-name account)))
	  (if (or hierarchical? (is-type-income-or-expense? type))
	      (let* ((children (gnc:account-get-children account))
		     (childrens-output (if (and (not children)
						(not hierarchical?))
					   (handle-txf-special-splits 
					    level account from-value
					    to-value)
					   (gnc:group-map-accounts
					    (lambda (x)
					      (if (>= max-level (+ 1 level))
						  (handle-level-x-account
						   (+ 1 level) x)
						  '()))
					    children)))
		     
		     (account-balance (if (or hierarchical?
					      (gnc:account-get-tax account))
					  (d-gnc:account-get-balance-interval
					   account from-value to-value #f)
					  0))) ; don't add non tax related
		
		(set! account-balance (+ (if (> max-level level)
					     (lx-collector (+ 1 level)
							   'total #f)
					     0)
					 ;; make positive
					 (if (is-type-income? type)
					     (- account-balance )
					     account-balance)))
		(lx-collector level 'add account-balance)
		(let ((level-x-output
		       (if tax-mode
			   (render-level-x-account level max-level account
						   account-balance
						   suppress-0 full-names #f
						   hierarchical?)
			   (render-txf-account account account-balance #f))))
		  (if (equal? 1 level)
		      (lx-collector 1 'reset #f))
		  (if (> max-level level)
		      (lx-collector (+ 1 level) 'reset #f))
		  (if (null? level-x-output)
		      '()
		      (if (null? childrens-output)
			  level-x-output
			  (if tax-mode
			      (list level-x-output
				    childrens-output
				    (html-table-row (list "&nbsp;")))
			      (if (not children) ; swap for txf special splt
				  (list childrens-output level-x-output)
				  (list level-x-output childrens-output)))))))
	      ;; Ignore
	      '())))
      
      (if (not hierarchical?)
	  (let* ((tax-stat (op-value tab-title "Set/Reset Tax Status"))
		 (txf-acc-lst (op-value "TXF Export Init" "Select Account"))
		 (txf-account (if (null? txf-acc-lst)
				  (begin (set! txf-acc-lst '(#f))
					 #f)
				  (car txf-acc-lst)))
		 (txf-income (op-value "TXF Export Init" "For INCOME accounts,\
 select here.   < ^ # see help"))
		 (txf-expense (op-value "TXF Export Init" "For EXPENSE\
 accounts, select here.   < ^ # see help"))
		 (txf-payer-source (op-value "TXF Export Init"
					     "< ^   Payer Name source"))
		 (txf-full-name-lst (if txf-account
					(map gnc:account-get-full-name
					     txf-acc-lst)
					'(#f)))
		 (not-used 
		  (case tax-stat
		    ((tax-set)
		     (key-status user-sel-accnts #t tax-key tax-end-key #f)
		     (gnc:refresh-main-window))
		    ((tax-reset)
		     (key-status user-sel-accnts #f tax-key tax-end-key #f)
		     (gnc:refresh-main-window))
		    ((tax-set-kids)
		     (key-status user-sel-accnts #t tax-key tax-end-key #t)
		     (gnc:refresh-main-window))
		    ((tax-reset-kids)
		     (key-status user-sel-accnts #f tax-key tax-end-key #t)
		     (gnc:refresh-main-window))))
		 
		 (txf-fun-str-lst (map (lambda (a) (txf-function 
						    a txf-income txf-expense
						    txf-payer-source))
				       txf-acc-lst)))
	    (set! txf-feedback-str-lst (map txf-feedback-str txf-fun-str-lst
					    txf-full-name-lst))))
      
      (let ((output '())
	    (from-date (strftime "%Y-%b-%d" (localtime (car from-value))))
	    (to-date (strftime "%Y-%b-%d" (localtime (car to-value))))
	    (today-date (strftime "D%m/%d/%Y" 
				  (localtime 
				   (car (gnc:timepair-canonical-day-time
					 (cons (current-time) 0))))))
	    (txf-last-payer "")
	    (txf-l-count 0)
	    (report-title (if txf-help 
			      (string-db 'lookup 'txf-title)
			      report-name))
	    (file-name "????"))
	
	;; Now, the main body
	;; Reset all the balance collectors
	(do ((i 1 (+ i 1)))
	    ((> i MAX-LEVELS) i)
	  (lx-collector i 'reset #f))
	(set! txf-dups-alist '())
	
	(if (not tax-mode-in)		; First do Txf mode, if set
	    (begin
	      (set! file-name		; get file name from user
		    (do ((fname (gnc:file-selection-dialog
				 "Select file for .TXF export" ""
				 "~/export.txf")
				(gnc:file-selection-dialog
				 "Select file for .TXF export" ""
				 "~/export.txf")))  
			((if (not fname)
			     #t		; no "Cancel" button, exit
			     (if (access? fname F_OK)
				 (if (gnc:verify-dialog
				      (string-append 
				       "File: \"" fname
				       "\" exists, Over Write?")
				      #f)
				     (begin (delete-file fname)
					    #t)
				     #f)
				 #t))
			 fname)))
	      
	      (if file-name		; cancel TXF if no file selected
		  (let* ((port (open-output-file file-name))    
			 (output (list
				  (map (lambda (x) (handle-level-x-account
						    1 x))
				       selected-accounts)))
			 (output-txf (list
				      "V035"
				      "\nAGnuCash 1.5.2"
				      "\n" today-date
				      "\n^"
				      output)))
		    
		    (gnc:display-report-list-item output-txf port
						  "taxtxf.scm - ")
		    (newline port)
		    (close-output-port port)))))
	
	(set! tax-mode #t)		; now do tax mode to display report
	(set! output (list 
		      (if txf-help
			  (append (map (lambda (x) (txf-print-help x #t))
				       txf-help-catagories)
				  (map (lambda (x) (txf-print-help x #t))
				       txf-income-catagories)
				  (map (lambda (x) (txf-print-help x #f))
				       txf-expense-catagories))
			  (map (lambda (x) (handle-level-x-account 1 x))
			       selected-accounts))))
	
	(list			; Tax
	 "<html>"
	 "<head>"
	 "<title>" report-title "</title>"
	 "</head>\n"
	 "<body bgcolor=" bg-color ">"
	 "<center>"
	 "<h1>" report-title "</h1>\n"
	 "<p>"
	 (if txf-help
	     ""
	     (html-black (string-append (string-db 'lookup 'tax-from)
					from-date
					(string-db 'lookup 'tax-to)
					to-date)))
	 "</p>\n"
	 "<p>"
	 (html-blue (if hierarchical?
			""
			(if tax-mode-in
			    (if txf-help 
				""
				(string-db 'lookup 'txf-may))
			    (if file-name
				(string-append (string-db 'lookup 'txf-was)
					       file-name "\"")
				(string-db 'lookup 'txf-not)))))
	 "</p>\n"
	 "</center>"
	 (if (or hierarchical? txf-help)
	     ""
	     (map html-para (map html-blue txf-feedback-str-lst)))
	 (txf-print-dups)
	 "<table " (if txf-help "border=1 " "border=0 ") "cellpadding=1>"
	 "<tr>"
	 "<th>"
	 (if txf-help
	     (list (string-db 'lookup 'txf-form-code) "<br>"
		   (string-db 'lookup 'txf-desc))
	     (string-db 'lookup 'account-name))
	 "</th>\n"
	 (if txf-help
	     ""
	     (do ((i (- max-level 1) (- i 1))
		  (head "" (string-append 
			    head "<th align=right>" (string-db 'lookup 'sub)
			    (number->string i) ")</th>")))
		 ((< i 1) head)))
	 (if txf-help
	     (list "<th>" (string-db 'lookup 'txf-help)
		   (html-blue " Income") (html-red " Expense"))
	     (list "<th align=right>" (string-db 'lookup 'balance)))
	 "</th>\n"
	 "</tr>\n"
	 output
	 "</table>\n"
	 (if (null? (car output))
	     (string-append "<p><b>" (string-db 'lookup (if hierarchical?
							    'no-hierarchical
							    'no-tax))
			    "</b></p>\n")
	     " ")
	 "</body>"
	 "</html>")
	)))
  
  (string-db 'store 'net "Net")
  (string-db 'store 'account-name "Account Name")
  (string-db 'store 'no-tax "No Tax Related accounts were found. Click \
\"Parameters\" to set some  with the \"Set/Reset Tax Status:\" parameter.") 
  (string-db 'store 'no-hierarchical "No accounts were found.") 
  (string-db 'store 'txf-may "Blue items are exportable to a TXF file")
  (string-db 'store 'txf-was "Blue items were exported to file: \"")
  (string-db 'store 'txf-not "Blue items were <b>NOT</b> exported to txf \
file!")
  (string-db 'store 'sub "(Sub ")
  (string-db 'store 'balance "Total")
  (string-db 'store 'hierarchical-title "Hierarchical Accounts Report")
  (string-db 'store 'txf-title "Detailed TXF Category Descriptions")
  (string-db 'store 'tax-title "Taxable Income / Deductible Expenses")
  (string-db 'store 'tax-from "Period From: ")
  (string-db 'store 'tax-to "  To: ")
  (string-db 'store 'tax-desc "This page shows your Taxable Income and \
Deductable Expenses.")
  (string-db 'store 'txf-form-code "Tax Form \\ TXF Code")
  (string-db 'store 'txf-desc "Description")
  (string-db 'store 'txf-help "Extended TXF Help messages")
  (string-db 'store 'txf-dup "ERROR: There are duplicate TXF codes assigned\
 to some accounts.  Only TXF codes prefixed with \"&lt;\" or \"^\" may be\
 repeated.")
  
  ;; copy help strings to catagory structures.
  (txf-help txf-income-catagories)
  (txf-help txf-expense-catagories)
  (txf-help txf-help-catagories)
  
  (gnc:define-report
   'version 1
   'name "Hierarchical"
   'options-generator hierarchical-options-generator
   'renderer (lambda (options)
               (generate-tax-or-txf 
                (string-db 'lookup 'hierarchical-title)
                (string-db 'lookup 'tax-desc)
                options
		#t)))
  
  (gnc:define-report
   'version 1
   'name "Tax"
   'options-generator tax-options-generator
   'renderer (lambda (options)
               (generate-tax-or-txf 
                (string-db 'lookup 'tax-title)
                (string-db 'lookup 'tax-desc)
                options
		#t)))
  
  (gnc:define-report
   'version 1
   'name "Export .TXF"
   'options-generator tax-options-generator
   'renderer (lambda (options)
               (generate-tax-or-txf 
                (string-db 'lookup 'tax-title)
                (string-db 'lookup 'tax-desc)
                options
		#f))))
