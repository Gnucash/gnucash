;; -*-scheme-*-
;;
;; This file was copied from the file txf.scm by  Richard -Gilligan- Uschold
;;
;; In part English text with German text replaced and completed
;; for GnuCash Vers. 2.4.0 in Dezember 2010 by FJSW - Franz Stoll
;;
;; Originally, these were meant to print Tax related accounts and
;; exports TXF files for import to TaxCut, TurboTax, etc.  for the US
;; tax TXF format. I modified this heavily so that it might become
;; useful for the German Umsatzsteuer-Voranmeldung.
;; 
;; The report in this file extracts the amounts that belong to the
;; Kennzahlen (from txf-de_DE.scm) as assigned to the different
;; accounts, and will write it to some XML file as required by
;; e.g. the Winston software
;; http://www.felfri.de/winston/schnittstellen.htm
;;
;; This file might still contain a lot of US-TXF related stuff. This
;; can surely be thrown out once someone was able to actually use this
;; report for his/her taxes.
;;
;;
;; Richard Gilligan Uschold's original comment continued here as follows:
;;
;; For this to work, the user has to segregate taxable and not taxable
;; income to different accounts, as well as deductible and non
;; deductible expenses.
;;
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
;; current-date dependant!  Actually, time of day dependant!  Just
;; after midnight gives different dates than just before!  Referencing
;; all times to noon seems to fix this.  Subtracting 1 year sometimes
;; subtracts 2!  see "(to-value"

;; depends must be outside module scope -- and should eventually go away.

(define-module (gnucash report taxtxf-de_DE))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (srfi srfi-1))
(use-modules (gnucash printf))
(use-modules (gnucash core-utils)) ; for gnc:version
(use-modules (gnucash gettext))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/tax/de_DE" 0)
(gnc:module-load "gnucash/report/report-system" 0)


(define reportname (N_ "Tax Report / TXF Export"))

(define (make-level-collector num-levels)
  (let ((level-collector (make-vector num-levels)))
    (do ((i 0 (+ i 1)))
        ((= i num-levels) i)
      (vector-set! level-collector i (gnc:make-commodity-collector)))
    level-collector))

(define MAX-LEVELS 16)			; Maximum Account Levels

(define levelx-collector (make-level-collector MAX-LEVELS))

(define today (timespecCanonicalDayTime
               (cons (current-time) 0)))

(define bdtm
  (let ((result (gnc:timepair->date today)))
    (set-tm:mday result 16)             ; 16
    (set-tm:mon result 3)               ; Apr
    (set-tm:isdst result -1)
    result))

(define tax-day (cons (car (mktime bdtm)) 0))

(define after-tax-day (gnc:timepair-later tax-day today))

(define (make-split-list account split-filter-pred)
  (reverse (filter split-filter-pred
                   (xaccAccountGetSplitList account))))

;; returns a predicate that returns true only if a split is
;; between early-date and late-date
(define (split-report-make-date-filter-predicate begin-date-tp
                                                 end-date-tp)
  (lambda (split) 
    (let ((tp
           (gnc-transaction-get-date-posted
            (xaccSplitGetParent split))))
      (and (gnc:timepair-ge-date tp begin-date-tp)
           (gnc:timepair-le-date tp end-date-tp)))))

;; This is nearly identical to, and could be shared with
;; display-report-list-item in report.scm. This adds warn-msg parameter
(define (gnc:display-report-list-item item port warn-msg)
  (cond
   ((string? item) (display item port))
   ((null? item) #t)
   ((list? item) (map (lambda (item)
                        (gnc:display-report-list-item item port warn-msg))
                      item))
   (else (gnc:warn warn-msg item " is the wrong type."))))

(define (lx-collector level action arg1 arg2)
  ((vector-ref levelx-collector (- level 1)) action arg1 arg2))

;; Unlike to the US the German tax quarters are real quarters.
;; To allow for easily incorporating changes from the US version
;; we simply set  tax-qtr-real-qtr-year to 0.
(define tax-qtr-real-qtr-year 0)

(define (tax-options-generator)
  (define options (gnc:new-options))
  (define (gnc:register-tax-option new-option)
    (gnc:register-option options new-option))

  ;; date at which to report 
  (gnc:options-add-date-interval!
   options gnc:pagename-general 
   (N_ "From") (N_ "To") "a")

  (gnc:register-tax-option
   (gnc:make-multichoice-option
    gnc:pagename-general (N_ "Alternate Period")
    "c" (N_ "Override or modify From: & To:.")
    (if after-tax-day 'from-to 'last-year)
    (list (list->vector
           (list 'from-to (N_ "Use From - To") (N_ "Use From - To period.")))
          (list->vector
           (list '1st-est (N_ "1st Est Tax Quarter") (N_ "Jan 1 - Mar 31.")))
          (list->vector
           (list '2nd-est (N_ "2nd Est Tax Quarter") (N_ "Apr 1 - May 31.")))
          (list->vector
	   ;; Translators: The US tax quarters are different from
	   ;; actual year's quarters! See the definition of
	   ;; tax-qtr-real-qtr-year variable above.
           (list '3rd-est (N_ "3rd Est Tax Quarter") (N_ "Jun 1 - Aug 31.")))
          (list->vector
           (list '4th-est (N_ "4th Est Tax Quarter") (N_ "Sep 1 - Dec 31.")))
          (list->vector
           (list 'last-year (N_ "Last Year") (N_ "Last Year.")))
          (list->vector
           (list '1st-last (N_ "Last Yr 1st Est Tax Qtr")
                 (N_ "Jan 1 - Mar 31, Last year.")))
          (list->vector
           (list '2nd-last (N_ "Last Yr 2nd Est Tax Qtr")
                 (N_ "Apr 1 - May 31, Last year.")))
          (list->vector
           (list '3rd-last (N_ "Last Yr 3rd Est Tax Qtr")
                 (N_ "Jun 1 - Aug 31, Last year.")))
          (list->vector
           (list '4th-last (N_ "Last Yr 4th Est Tax Qtr")
                 (N_ "Sep 1 - Dec 31, Last year."))))))

  (gnc:register-tax-option
   (gnc:make-account-list-option
    gnc:pagename-accounts (N_ "Select Accounts (none = all)")
    "d" (N_ "Select accounts.")
    (lambda () '())
    #f #t))
  
  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Suppress $0.00 values")
    "f" (N_ "$0.00 valued Accounts won't be printed.") #t))

  (gnc:register-tax-option
   (gnc:make-simple-boolean-option
    gnc:pagename-display (N_ "Print Full account names")
    "g" (N_ "Print all Parent account names.") #f))

  (gnc:options-set-default-section options gnc:pagename-general)

  options)

;; Render txf information
(define crlf (string #\return #\newline)) ; TurboTax seems to want these

(define txf-last-payer "")		; if same as current, inc txf-l-count
					; this only works if different
					; codes from the same payer are
					; grouped in the accounts list
(define txf-l-count 0)		; count repeated N codes

;; stores assigned txf codes so we can check for duplicates
(define txf-dups-alist '())

(define (txf-payer? payer)
  (member payer (list 'current 'parent)))

(define (gnc:account-get-txf account)
  (and (xaccAccountGetTaxRelated account)
       (not (equal? (gnc:account-get-txf-code account) 'N000))))

(define (gnc:account-get-txf-code account)
  (let ((code (xaccAccountGetTaxUSCode account)))
    (string->symbol (if (string-null? code) "N000" code))))

(define (gnc:get-txf-format code income?)
  (gnc:txf-get-format (if income?
                          txf-income-categories
                          txf-expense-categories)
                      code ""))

(define (gnc:account-get-txf-payer-source account)
  (let ((pns (xaccAccountGetTaxUSPayerNameSource account)))
    (string->symbol (if (string-null? pns) "keine" pns))))

;; check for duplicate txf codes
(define (txf-check-dups account) 
  (let* ((code (gnc:account-get-txf-code account))
         (item (assoc-ref txf-dups-alist code))
         (payer (gnc:account-get-txf-payer-source account)))
    (if (not (txf-payer? payer))
        (set! txf-dups-alist (assoc-set! txf-dups-alist code
                                         (if item
                                             (cons account item)
                                             (list account)))))))

;; Print error message for duplicate txf codes and accounts
(define (txf-print-dups doc)
  (let ((dups
         (apply append
                (map (lambda (x)
                       (let ((cnt (length (cdr x))))
                         (if (> cnt 1)
                             (let* ((acc (cadr x))
                                    (txf (gnc:account-get-txf acc)))
                               (cons (string-append 
                                      "Kennzahl \"" 
                                      (symbol->string
                                       (gnc:account-get-txf-code acc))
                                      "\" hat Duplikate in "
                                      (number->string cnt) " Konten:")
                                     (map gnc-account-get-full-name
                                          (cdr x))))
                             '())))
                     txf-dups-alist)))
        (text (gnc:make-html-text)))
    (if (not (null? dups))
        (begin
          (gnc:html-document-add-object! doc text)
          (gnc:html-text-append!
           text
           (gnc:html-markup-p
            (gnc:html-markup
             "blue"
             (_ "WARNING: There are duplicate TXF codes assigned\
 to some accounts. Only TXF codes with payer sources may be repeated."))))
          (map (lambda (s)
                 (gnc:html-text-append!
                  text
                  (gnc:html-markup-p
                   (gnc:html-markup "blue" s))))
               dups)))))

;; some codes require special handling
(define (txf-special-split? code)
  (member code (list 'N521)))            ; only one for now

(define (fill-clamp-sp str len)
  (string-append (substring (string-append str (make-string len #\space))
                            0 (- len 1)) " "))

(define (fill-clamp str len)
  (string-append (substring (string-append str (make-string len #\space))
                            0 len)))

(define (make-header-row table max-level)
  (gnc:html-table-prepend-row!
   table
   (append (list (gnc:make-html-table-header-cell/markup
                  "account-header" (_ "Account Name")))
           (make-sub-headers max-level)
           (list (gnc:make-html-table-header-cell/markup
                  "number-header" (_ "Total"))))))

(define (make-sub-headers max-level)
  (if (<= max-level 1)
      '()
      (cons (gnc:make-html-table-header-cell/markup
             "number-header"
             "Sub-"
             (number->string (- max-level 1)))
            (make-sub-headers (- max-level 1)))))

(define (render-txf-account account account-value d? date x? x-date)
  (let* ((print-info (gnc-account-print-info account #t))
         (txf? (gnc:account-get-txf account)))
    (if (and txf?
             (not (gnc-numeric-zero-p account-value)))
        (let* ((type (xaccAccountGetType account))
               (code (gnc:account-get-txf-code account))
               (date-str (if date
                             (strftime "%d.%m.%Y" (localtime (car date)))
                             #f))
               (x-date-str (if x-date
                               (strftime "%d.%m.%Y" (localtime (car x-date)))
                               #f))
               ;; Only formats 1,3 implemented now! Others are treated as 1.
               (format (gnc:get-txf-format code (eq? type ACCT-TYPE-INCOME)))
	       (value (string-append 
		       (if (eq? type ACCT-TYPE-INCOME) ;; negate expenses. FIXME: Necessary?
			   ""
			   "-")
		       (number->string 
			(gnc-numeric-num
			 (gnc-numeric-convert account-value (cond
							     ((eq? format 2) 1)
							     (else 100))
					      3))))) ;; 3 is the GNC_HOW_TRUNC truncation rounding
	       (payer-src (gnc:account-get-txf-payer-source account))
               (account-name (let* ((named-acct
				    (if (eq? payer-src 'parent)
					(gnc-account-get-parent account)
					account))
				    (name (xaccAccountGetName named-acct)))
			       (if (not (string-null? name))
				   name
				   (begin
				     (display
				      (string-append
				       "Failed to get name for account: "
				       (gncAccountGetGUID named-acct)
				       (if (not (eq? account named-acct))
					   (string-append
					    " which is the parent of "
					    (gncAccountGetGUID account)))
				       "\n"))
				     "<NONE> -- See the Terminal Output"))))
               (action (if (eq? type ACCT-TYPE-INCOME)
                           (case code
                             ((N286 N488) "ReinvD")
                             (else "Ertraege"))
                           "Aufwendungen"))
               (category-key (if (eq? type ACCT-TYPE-INCOME)
                                 (gnc:txf-get-category-key 
                                  txf-income-categories code "")
                                 (gnc:txf-get-category-key
                                  txf-expense-categories code "")))
               (value-name (if (equal? "ReinvD" action)
                               (string-append 
                                (substring value 1 (string-length value))
                                " " account-name)
                               account-name))
               (l-value (if (= format 3)
                            (begin
                              (set! txf-l-count 
                                    (if (equal? txf-last-payer account-name)
                                        txf-l-count
                                        (+ 1 txf-l-count)))
                              (set! txf-last-payer account-name)
                              (number->string txf-l-count))
                            "1")))
	  ;(display "render-txf-account \n")
	  ;(display-backtrace (make-stack #t) (current-output-port))

	  ;; FIXME: Here the actual rendering of one account entry is
	  ;; done. Use the German format here.
          (list "  <Kennzahl Nr=\""
		category-key
		"\">"
                value
		"</Kennzahl>" crlf))
;                (case format
;                  ((3) (list "P" account-name crlf))
;                  (else (if (and x? (txf-special-split? code))
;                            (list "P" crlf)
;                            '())))
;                (if x?
;                    (list "X" x-date-str " " (fill-clamp-sp account-name 31)
;                          (fill-clamp-sp action 7) 
;                          (fill-clamp-sp value-name 82)
;                          (fill-clamp category-key 15) crlf)
;                    '())
;                "^" crlf))
	"")))

;; Render any level
(define (render-level-x-account table level max-level account lx-value
                                suppress-0 full-names txf-date)
  (let* ((account-name (if txf-date	; special split
                           (strftime "%d.%m.%Y" (localtime (car txf-date)))
                           (if (or full-names (equal? level 1))
                               (gnc-account-get-full-name account)
                               (xaccAccountGetName account))))
         (blue? (gnc:account-get-txf account))
         (print-info (gnc-account-print-info account #f))
         (value (xaccPrintAmount lx-value print-info))
         (value-formatted (if (= 1 level)
                              (gnc:html-markup-b value)
                              value))
         (value-formatted (gnc:make-html-text
                           (if blue?
                               (gnc:html-markup "blue" value-formatted)
                               value-formatted)))
         (account-name (if blue?
                           (gnc:html-markup "blue" account-name)
                           ;; Note: gnc:html-markup adds an extra space
                           ;; before the " <FONT" tag, so we compensate.
                           (string-append " " account-name)))
         (blank-cells (make-list (- max-level level)
                                 (gnc:make-html-table-cell #f)))
         (end-cells (make-list (- level 1) (gnc:make-html-table-cell #f))))

    (if (and blue? (not txf-date))	; check for duplicate txf codes
        (txf-check-dups account))

    (if (or (not suppress-0) (= level 1)
            (not (gnc-numeric-zero-p lx-value)))
        (begin
          (gnc:html-table-prepend-row!
           table
           (append
            (list (gnc:make-html-table-cell
                   (apply gnc:make-html-text
                          (append (make-list (* 3 (- level 1)) "&nbsp; ")
                                  (list account-name)))))
            blank-cells
            (list (gnc:make-html-table-cell/markup "number-cell"
                                                   value-formatted))
            end-cells))
          (if (= level 1) (make-header-row table max-level))))))

;; Recursivly validate children if parent is not a tax account.
;; Don't check children if parent is valid.
;; Returns the Parent if a child or grandchild is valid.
(define (validate accounts)
  (filter (lambda (a)
            (if (xaccAccountGetTaxRelated a)
                #t
                ;; check children
                (if (null? (validate (gnc-account-get-descendants a)))
                    #f
                    #t)))
          accounts))

(define (generate-tax-or-txf report-name
                             report-description
                             report-obj
                             tax-mode?
                             file-name)

  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option 
      (gnc:report-options report-obj) pagename optname)))

  ;; the number of account generations: children, grandchildren etc.
  (define (num-generations account gen)
    (if (eq? (gnc-account-n-children account) 0)
	(if (and (xaccAccountGetTaxRelated account)
		 (txf-special-split? (gnc:account-get-txf-code account)))
	    (+ gen 1)		; Est Fed Tax has a extra generation
	    gen)	       		; no kids, return input
	(apply max (gnc:account-map-children
		    (lambda (x) (num-generations x (+ 1 gen)))
		    account))))

  (gnc:report-starting reportname)
  (let* ((from-value (gnc:date-option-absolute-time 
                      (get-option gnc:pagename-general "From")))
         (to-value (gnc:timepair-end-day-time
                    (gnc:date-option-absolute-time 		       
                     (get-option gnc:pagename-general "To"))))
         (alt-period (get-option gnc:pagename-general "Alternate Period"))
         (suppress-0 (get-option gnc:pagename-display 
                                 "Suppress $0.00 values"))
         (full-names (get-option gnc:pagename-display
                                 "Print Full account names"))
         (user-sel-accnts (get-option gnc:pagename-accounts
                                      "Select Accounts (none = all)"))
         (valid-user-sel-accnts (validate user-sel-accnts))
         ;; If no selected accounts, check all.
         (selected-accounts (if (not (null? user-sel-accnts))
                                valid-user-sel-accnts
                                (validate (reverse 
                                           (gnc-account-get-children-sorted
                                            (gnc-get-current-root-account))))))
         (generations (if (pair? selected-accounts)
                          (apply max (map (lambda (x) (num-generations x 1))
                                          selected-accounts))
                          0))
         (max-level (min MAX-LEVELS (max 1 generations)))
	 (work-to-do 0)
	 (work-done 0)

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
                        (set-tm:isdst bdtm -1)
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
                      (set-tm:isdst bdtm -1)
                      (cons (car (mktime bdtm)) 0))))

         (txf-feedback-str-lst '())
         (doc (gnc:make-html-document))
         (table (gnc:make-html-table)))

    ;; for quarterly estimated tax payments, we need a different period
    ;; return the sometimes changed (from-est to-est full-year?) dates
    (define (txf-special-splits-period account from-value to-value)
      (if (and (xaccAccountGetTaxRelated account)
               (txf-special-split? (gnc:account-get-txf-code account)))
          (let* 
              ((full-year?
                (let ((bdto (localtime (car to-value)))
                      (bdfrom (localtime (car from-value))))
                  (and (equal? (tm:year bdto) (tm:year bdfrom))
                       (equal? (tm:mon bdfrom) 0)
                       (equal? (tm:mday bdfrom) 1)
                       (equal? (tm:mon bdto) 11)
                       (equal? (tm:mday bdto) 31))))
               ;; Adjust dates so we get the final Estimated Tax
               ;; paymnent from the right year
               (from-est (if full-year?
                             (let ((bdtm (gnc:timepair->date
                                          (timespecCanonicalDayTime
                                           from-value))))
                               (set-tm:mday bdtm 1) ; 01
                               (set-tm:mon bdtm 2) ; Mar
                               (set-tm:isdst bdtm -1)
                               (cons (car (mktime bdtm)) 0))
                             from-value))
               (to-est (if full-year?
                           (let* ((bdtm (gnc:timepair->date
                                         (timespecCanonicalDayTime
                                          from-value))))
                             (set-tm:mday bdtm 28) ; 28
                             (set-tm:mon bdtm 1) ; Feb
                             (set-tm:year bdtm (+ (tm:year bdtm) 1))
                             (set-tm:isdst bdtm -1)
                             (cons (car (mktime bdtm)) 0))
                           to-value)))
            (list from-est to-est full-year?))
          #f))
    
    ;; for quarterly estimated tax payments, we need to go one level down
    ;; and get data from splits
    (define (handle-txf-special-splits level account from-est to-est 
                                       full-year? to-value)
      (let*
          ((split-filter-pred (split-report-make-date-filter-predicate
                               from-est to-est))
           (split-list (make-split-list account split-filter-pred))
           (lev  (if (>= max-level (+ 1 level))
                     (+ 1 level)
                     level)))
        (map (lambda (spl) 
               (let* ((date (gnc-transaction-get-date-posted
                             (xaccSplitGetParent spl)))
                      (amount (xaccSplitGetAmount spl))
                      ;; TurboTax 1999 and 2000 ignore dates after Dec 31
                      (fudge-date (if (and full-year? 
                                           (gnc:timepair-lt to-value date))
                                      to-value
                                      date)))
                 (if tax-mode?
                     (render-level-x-account table lev max-level account
                                             amount suppress-0 #f date)
                     (render-txf-account account amount
                                         #t fudge-date  #t date))))
             split-list)))
    
    (define (count-accounts level accounts)
      (if (< level max-level)
	  (let ((sum 0))
	    (for-each (lambda (x)
		   (if (gnc:account-is-inc-exp? x)
		       (set! sum (+ sum (+ 1 (count-accounts (+ 1 level)
							     (gnc-account-get-children x)))))
		       0))
		 accounts)
	    sum)
	  (length accounts)))

    (define (handle-level-x-account level account)
      (let ((type (xaccAccountGetType account)))
	(set! work-done (+ 1 work-done))
	(gnc:report-percent-done (* 100 (if (> work-to-do 0)
					    (/ work-done work-to-do)
					    1)))
        (if (gnc:account-is-inc-exp? account)
            (let* ((children (gnc-account-get-children-sorted account))
                   (to-special #f)	; clear special-splits-period
                   (from-special #f)
                   (childrens-output 
                    (if (null? children)
                        (let* ((splits-period (txf-special-splits-period
                                               account from-value to-value)))
                          (if splits-period
                              (let* ((full-year? (caddr splits-period)))
                                (set! from-special (car splits-period))
                                (set! to-special (cadr splits-period))
                                (handle-txf-special-splits level account
                                                           from-special
                                                           to-special
                                                           full-year?
                                                           to-value))
                              
                              '()))

                        (map (lambda (x)
                               (if (>= max-level (+ 1 level))
                                   (handle-level-x-account (+ 1 level) x)
                                   '()))
                             (reverse children))))

                   (account-balance 
                    (if (xaccAccountGetTaxRelated account)
                        (if to-special
                            (gnc:account-get-balance-interval
                             account from-special to-special #f)
                            (gnc:account-get-balance-interval
                             account from-value to-value #f))
                        (gnc-numeric-zero)))) ; don't add non tax related

              (set! account-balance
                    (gnc-numeric-add-fixed
                     (if (> max-level level)
                         (cadr
                          (lx-collector (+ 1 level)
                                        'getpair
                                        (xaccAccountGetCommodity account)
                                        #f))
                         (gnc-numeric-zero))
                       ;; make positive
                       (if (eq? type ACCT-TYPE-INCOME)
                           (gnc-numeric-neg account-balance)
                           account-balance)))

              (lx-collector level
                            'add
                            (xaccAccountGetCommodity account)
                            account-balance)

              (let ((level-x-output
                     (if tax-mode?
                         (render-level-x-account table level
                                                 max-level account
                                                 account-balance
                                                 suppress-0 full-names #f)
                         (list 
                          ;(if (not to-special)
                          ;    (render-txf-account account account-balance
                          ;                        #f #f #t from-value)
                          ;    '())
                          (render-txf-account account account-balance
                                              #f #f #f #f)))))
                (if (equal? 1 level)
                    (lx-collector 1 'reset #f #f))

                (if (> max-level level)
                    (lx-collector (+ 1 level) 'reset #f #f))

                (if (null? level-x-output)
                    '()
                    (if (null? childrens-output)
                        level-x-output
                        (if tax-mode?
                            (list level-x-output
                                  childrens-output)
                            (if (null? children) ; swap for txf special splt
                                (list childrens-output level-x-output)
                                (list level-x-output childrens-output)))))))
            ;; Ignore
            '())))

    (let ((from-date  (strftime "%d.%m.%Y" (localtime (car from-value))))
          (to-date    (strftime "%d.%m.%Y" (localtime (car to-value))))
	  (to-year    (strftime "%Y" (localtime (car to-value))))
          (today-date (strftime "%d.%m.%Y" 
                                (localtime 
                                 (car (timespecCanonicalDayTime
                                       (cons (current-time) 0))))))
	  (tax-nr (or 
		   (kvp-frame-get-slot-path-gslist
		    (qof-book-get-slots (gnc-get-current-book))
		    (append gnc:*kvp-option-path*
			    (list gnc:*tax-label* gnc:*tax-nr-label*)))
		   ""))
	  )

      ;; Now, the main body
      ;; Reset all the balance collectors
      (do ((i 1 (+ i 1)))
          ((> i MAX-LEVELS) i)
        (lx-collector i 'reset #f #f))

      (set! txf-last-payer "")
      (set! txf-l-count 0)
      (set! work-to-do (count-accounts 1 selected-accounts))

      (if (not tax-mode?)		; Do Txf mode
          (begin
            (if file-name		; cancel TXF if no file selected
                (let* ((port (open-output-file file-name))    
                       (output
                        (map (lambda (x) (handle-level-x-account 1 x))
                             selected-accounts))
		       ;; FIXME: Print the leading and trailing bits here
                       (output-txf (list
                                    "<WinstonAusgang>" crlf
				    "  <Formular Typ=\"UST\"></Formular>" crlf
				    ;; FIXME: Get this Ordnungsnummer somehow
				    "  <Ordnungsnummer>"
				    tax-nr
				    "</Ordnungsnummer>" crlf
                                    ;;"<software>GnuCash</software>" crlf
				    ;;"<version>" gnc:version "</version>" crlf
                                    ;; today-date crlf
				    "  <AnmeldeJahr>" to-year "</AnmeldeJahr>" crlf
				    ;; FIXME: Find out what this should mean
				    "  <AnmeldeZeitraum>" "1" "</AnmeldeZeitraum>" crlf
                                    output
				    "</WinstonAusgang>")))

                  (gnc:display-report-list-item output-txf port
                                                "taxtxf-de.scm - ")
                  (close-output-port port)
                  #t)
                #f))

          (begin			; else do tax report
            (gnc:html-document-set-style! 
             doc "blue"
             'tag "font"
             'attribute (list "color" "#0000ff"))
            
            (gnc:html-document-set-style! 
             doc "income"
             'tag "font"
             'attribute (list "color" "#0000ff"))
            
            (gnc:html-document-set-style! 
             doc "expense"
             'tag "font"
             'attribute (list "color" "#ff0000"))
            
            (gnc:html-document-set-style!
             doc "account-header"
             'tag "th"
             'attribute (list "align" "left"))
            
            (gnc:html-document-set-title! doc report-name)
            
            (gnc:html-document-add-object! 
             doc (gnc:make-html-text         
                  (gnc:html-markup 
                   "center"
                   (gnc:html-markup-p
                    (gnc:html-markup/format
                     (_ "Period from %s to %s") from-date to-date)))))
            
            (gnc:html-document-add-object!
             doc (gnc:make-html-text
                  (gnc:html-markup 
                   "center"
                   (gnc:html-markup
                    "blue"
                    (gnc:html-markup-p
                     "Blaue Posten können in eine XML-Datei und diese mit der Software \"Winston\" zu ELSTER exportiert werden.<br>
Diese XML-Datei enthält dann die geschlüsselten USt-Kennzahlen und zu diesen die summierten Werte für den ELSTER-Export.<br>
Bei Umsätzen werden nur voll Beträge ausgewiesen, bei Steuerkennzahlen auch die Dezimalstellen, aber ohne Komma.<br>
Klicken Sie auf »Exportieren« , um den Export durchzuführen.")))))
            
            (txf-print-dups doc)
            
            (gnc:html-document-add-object! doc table)
            
            (set! txf-dups-alist '())
            (map (lambda (x) (handle-level-x-account 1 x))
                 selected-accounts)
            
            (if (null? selected-accounts)
                (gnc:html-document-add-object!
                 doc
                 (gnc:make-html-text
                  (gnc:html-markup-p
         "Keine Steuer-relevanten Konten gefunden.<br>
Gehen Sie zu Bearbeiten -> Optionen Steuerbericht, um Konten entsprechend einzurichten."))))

	    (gnc:report-finished)
            doc)))))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "758b125c05e54531a7dec5f1ef0ef9c8"
 'menu-name (N_ "Tax Report & XML Export")
 ;;'menu-path (list gnc:menuname-taxes)
 'menu-tip (N_ "Taxable Income / Deductible Expenses / Export to .XML file")
 'options-generator tax-options-generator
 'renderer (lambda (report-obj)
             (generate-tax-or-txf
              (_ "Taxable Income / Deductible Expenses")
              (_ "This report shows your Taxable Income and \
Deductible Expenses.")
              report-obj
              #t
              #f))
 'export-types (list (cons (_ "XML") 'txf))
 'export-thunk (lambda (report-obj choice file-name)
                 (generate-tax-or-txf
                  (_ "Taxable Income / Deductible Expenses")
                  (_ "This page shows your Taxable Income and \
Deductible Expenses.")
                  report-obj
                  #f
                  file-name)))
