;; -*-scheme-*-
;; average-balance.scm
;; Report history of account balance and other info
;; Plots the information with gnuplot
;;
;; Author makes no implicit or explicit guarantee of accuracy of 
;;  these calculations and accepts no responsibility for direct
;;  or indirect losses incurred as a result of using this software.
;;
;; Note that this code uses functions defined in "transaction-report.scm"
;; Matt Martin <matt.martin@ieee.org>

(use-modules (ice-9 regex))
(require 'hash-table)

(gnc:depend "structure.scm")

;; Modify a date
(define (moddate op adate delta)
  (let ((newtm (localtime (car adate))))
    (begin
      (set-tm:sec newtm (op (tm:sec newtm) (tm:sec delta)))
      (set-tm:min newtm (op (tm:min newtm) (tm:min delta)))
      (set-tm:hour newtm (op (tm:hour newtm) (tm:hour delta)))
      (set-tm:mday newtm (op (tm:mday newtm) (tm:mday delta)))
      (set-tm:mon newtm (op (tm:mon newtm) (tm:mon delta)))
      (set-tm:year newtm (op (tm:year newtm) (tm:year delta)))

       (let ((time (car (mktime newtm))))
	 (cons time 0))
       ))
  )

;; Add or subtract time from a date
(define (decdate adate delta)(moddate - adate delta ))
(define (incdate adate delta)(moddate + adate delta ))

;; Time comparison, true if t2 is later than t1
(define (gnc:timepair-later t1 t2)
    (< (car t1) (car t2)))

;; Build a list of time intervals 
(define (dateloop curd endd incr) 
  (cond ((gnc:timepair-later curd endd)
	 (let ((nextd (incdate curd incr)))
	 (cons (list curd (decdate nextd SecDelta) '())
	       (dateloop nextd endd incr))))
	(else '())
	)
  )


;; Options
(define (runavg-options-generator)
  (define gnc:*runavg-track-options* (gnc:new-options))

  ;; register a configuration option for the report
  
  (define (gnc:register-runavg-option new-option)
    (gnc:register-option gnc:*runavg-track-options* new-option))

  ;; from date  
  (gnc:register-runavg-option
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
  (gnc:register-runavg-option
   (gnc:make-date-option
    "Report Options" "To"
    "c" "Report items up to and including this date"
    (lambda () (cons (current-time) 0))
    #f))

  ;; account(s) to do report on

  (gnc:register-runavg-option
   (gnc:make-account-list-option
    "Report Options" "Account"
    "d" "Do transaction report on this account"
    (lambda ()
      (let ((current-accounts (gnc:get-current-accounts))
            (num-accounts 
	     (gnc:group-get-num-accounts (gnc:get-current-group))))

        (cond ((not (null? current-accounts)) current-accounts)
              (else 
	       (let ((acctlist '()))
		 (gnc:for-loop 
		  (lambda(x)
		    (set! acctlist 
		       (append! 
			acctlist
			(list (gnc:group-get-account 
			       (gnc:get-current-group) x)))))
		  0 (eval num-accounts) 1)
	    acctlist
		    )
	       ))))
    #f #t))

  (gnc:register-runavg-option
   (gnc:make-multichoice-option
    "Report Options" "Step Size"
    "b" "Get number at each one of these" 'WeekDelta
    (list #(DayDelta "Day" "Day")
	  #(WeekDelta "Week" "Week")
	  #(TwoWeekDelta "2Week" "Two Week")
	  #(MonthDelta "Month" "Month")
	  #(YearDelta "Year" "Year")
	  )))

  (gnc:register-runavg-option
   (gnc:make-simple-boolean-option
    "Report Options" "Sub-Accounts"
    "e" "Add in sub-accounts of each selected" #f))

  (gnc:register-runavg-option
   (gnc:make-multichoice-option
    "Report Options" "Plot Type"
    "f" "Get number at each one of these" 'NoPlot
    (list #(NoPlot "Nothing" "Make No Plot")
	  #(AvgBalPlot "Average" "Average Balance")
	  #(GainPlot "Net Gain" "Net Gain")
	  #(GLPlot "Gain/Loss" "Gain And Loss")
	  )))
  
  gnc:*runavg-track-options*)

; A reference zero date
(define zdate (let ((zd (localtime 0)))
		(set-tm:hour zd 0)
		(set-tm:min zd 0)
		(set-tm:sec zd 0)
		(set-tm:mday zd 0)
		(set-tm:mon zd 0)
		(set-tm:year zd 0)
		(set-tm:yday zd 0)
		(set-tm:wday zd 0)
                zd
		))

(define SecDelta (let ((ddt (eval zdate)))
		   (set-tm:sec ddt 1)
		   ddt))
(define YearDelta (let ((ddt (eval zdate)))
		   (set-tm:year ddt 1)
		   ddt))

(define DayDelta (let ((ddt (eval zdate)))
		   (set-tm:mday ddt 1)
		   ddt))
(define WeekDelta (let ((ddt (eval zdate)))
		   (set-tm:mday ddt 7)
		   ddt))
(define TwoWeekDelta (let ((ddt (eval zdate)))
		   (set-tm:mday ddt 14)
		   ddt))

(define MonthDelta (let ((ddt (eval zdate)))
		   (set-tm:mon ddt 1)
		   ddt))

;; Plot strings
(define AvgBalPlot "using 2:3:4:5 t 'Average Balance' with errorbars, '' using 2:3 smooth sbezier t '' with lines")
(define GainPlot "using 2:6 t 'Net Gain' with linespoints, '' using 2:6 smooth sbezier t '' with lines" )
(define GLPlot "using 2:8 t 'Losses' with lp, '' using 2:7 t 'Gains' with lp")
(define NoPlot "")

;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) (thunk (gnc:account-get-split account x)))
		0 (gnc:account-get-split-count account) 1))

;; get transactions date from split - needs to be done indirectly
;; as it's stored in the parent transaction

(define (gnc:split-get-transaction-date split)
  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))

;; ditto descriptions
(define (gnc:split-get-description-from-parent split)
  (gnc:transaction-get-description (gnc:split-get-parent split)))

;; get the account name of a split
(define (gnc:split-get-account-name split)  
  (gnc:account-get-name (gnc:split-get-account split)))

(define (gnc:timepair-to-ldatestring tp)
  (let ((bdtime (localtime (car tp))))
    (strftime "%m/%d/%Y" bdtime)))


;; Find difference in seconds (?) between time 1 and time2
(define (gnc:timepair-delta t1 t2)
    (- (car t2) (car t1)))

; Convert to string
(define (tostring val) 
  (cond ((number? val) (sprintf #f "%.2f" val))
	(else (call-with-output-string (lambda (p)(display val p))))
	))

;;;;;;;;;;;;;;;;;;;;
;; HTML Table
;;;;;;;;;;;;;;;;;;;;

; Create a column entry
(define (html-table-col val)
  (sprintf #f "<TD align=right> %s </TD>" (tostring val))
  )

; Create an html table row from a list of entries
(define (html-table-row lst)
  (cond ((string? lst) lst)
	(else
	 (string-append
	  (sprintf #f "<TR>")
	  (apply string-append (map html-table-col lst))
	  (sprintf #f "</TR>\n")
	  )))
  )

; Create an html table from a list of rows, each containing 
;   a list of column entries
(define (html-table hdrlst llst)
  (string-append
   (html-table-header hdrlst)
   (apply string-append (map html-table-row llst))
   (html-table-footer)
   )
  )

(define (html-table-headcol val)
  (sprintf #f "<TH justify=center> %s </TH>" (tostring val))
  )

(define (html-table-header vec)
   (apply string-append "<TABLE cellspacing=10 rules=\"rows\">\n" (map html-table-headcol vec))
  )

(define (html-table-footer)
   (sprintf #f "</TABLE>")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text table 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Create an text table row from a list of entries
(define (text-table-row lst)
  (string-append 
   (tostring (car lst))

   (apply string-append (map (lambda (val)
			       (string-append "\t" (tostring val)))
			     (cdr lst)))
   "\n"
   )
  )

(define (text-table-header lst)
  (string-append
   "# " 
   (text-table-row lst)
  ))

(define (text-table hdrlst llst)
  (string-append
   (text-table-header hdrlst)
   (apply string-append (map text-table-row llst))
   )
  )

; Quick and dirty until there is REAL plot support
(define (data-to-gpfile hdrlst llst fn plotcmd)
  (let ((oport (open-output-file fn)))
       (display  
	   (text-table hdrlst llst)
	 oport)
	(close-output-port oport)
	)
)

;; Returns sum of all vector elements after the first
(define (vector-sum v)
  (let ((sum 0)) 
    (gnc:for-loop (lambda(i) (set! sum (+ sum (car (vector-ref v i))))) 
		  1 (vector-length v) 1)
       sum))

; Datelist entry operators
(define (dl:begin dl) (car dl))
(define (dl:end dl) (car (cdr dl)))
	
(define (reduce-split-list dl tl pt av)
  (let ((avgaccum 0) 
	(bals av) 
	(prevdate 0) 
	(balmin 10E9) 
	(balmax -10E9)
	(gains 0)
	(losses 0))

    (define (procvals)
      (let ((curbal (vector-sum (car (cdr (av 'x 0))))))
	   (set! balmin (min balmin curbal))
	   (set! balmax (max balmax curbal))))

       (define (accbal beg end)
	 (let ((curbal (vector-sum (car (cdr (av 'x 0))))))
	 (set! avgaccum (+ avgaccum
			   (* curbal
			      (gnc:timepair-delta beg end)))))
	 )


    (define (calc-in-interval d tl)
      (cond ((not (null? tl))

	     (let* ((bd (dl:begin d)) ; begin date
		    (ed (dl:end d)) ; end date
		    (cs (car tl)) ; current split
		    (cd (gnc:split-get-transaction-date cs)) ;current date
		    (an (gnc:split-get-account-name cs)) ; account name
		    (prevbal (vector-sum (car (cdr (av 'x 0))))))

	       (cond ((gnc:timepair-later cd bd) ;split before interval
		      (bals 'put an (gnc:split-get-balance cs))
		      (calc-in-interval d (cdr tl))
		      )

		     ((gnc:timepair-later cd ed) ;split is in the interval
		      (accbal prevdate cd)
		      (procvals)
		      (bals 'put an (gnc:split-get-balance cs))

		      (let ((val (gnc:split-get-value cs)))
			(cond ((< 0 val) (set! gains (+ gains val)))
			      (else (set! losses (- losses val)))))

		      (procvals) ; catch all cases
		      (set! prevdate cd)
		      (calc-in-interval d (cdr tl))
		      )

		     (else  ; Past interval, nothing to do?
		      (accbal prevdate ed)
		      (procvals)
		      tl
		      )
		     )))

	    (else ; Out of data !
	     (accbal prevdate (dl:end d))
	     (procvals)
	     tl
	     )
	    )
      )
       
       ;; Actual routine
       (cond ((null? dl) '()) ;; End of recursion
	     (else 
	      (let* ((bd (dl:begin (car dl))) 
		     (ed (dl:end (car dl))) )
		
		;; Reset valaccumulator values
		(set! prevdate bd)
		(set! avgaccum 0)
		(set! gains 0)
		(set! losses 0)

		(let* ((rest (calc-in-interval (car dl) tl)))
		  ;; list of values for report
		  (cons
		   (list
		    (gnc:timepair-to-ldatestring bd)
		    (gnc:timepair-to-ldatestring ed)
		    (/ avgaccum 
		       (gnc:timepair-delta bd ed))
		   balmin balmax (- gains losses) gains losses)

		   (reduce-split-list (cdr dl) rest pt av)))
		)
	     )
       )))

;; Pull a scheme list of splits from a C array
(define (gnc:convert-split-list slist)
      (let ((numsplit 0)
	     (schsl '()))
       (while  
	(let ((asplit (gnc:ith-split slist numsplit)))
	  (cond 
	   ((pointer-token-null? asplit ) #f)
	   (else 
	    (set! schsl (append! schsl (list asplit)) )
	    (set! numsplit (+ numsplit 1))
	    #t))) ())
	   schsl
	   )
)

;; Pull a scheme list of accounts (including subaccounts) from group grp
(define (gnc:group-get-account-list grp)
  (cond ((pointer-token-null? grp) '())
	(else
	 (let ((numacct 0)
	       (acctar (gnc:get-accounts grp))
	       (schal '()))
	   
	   (while  
	    (let ((anact (gnc:account-nth-account acctar numacct)))
	      (cond 
	       ((pointer-token-null? anact ) #f)
	       (else 
		(set! schal (append! schal (list anact)) )
		(set! numacct (+ numacct 1))
		#t))) ())
	      schal
	      )))
      )

(define (accumvects x y)
  (cond 
   ((null? x) '())
   ((number? (car x)) 
    (cons  (+ (car x) (car y)) (accumvects (cdr x) (cdr y))))
   (else (cons "x" (accumvects (cdr x) (cdr y)))))
  )

;; Add x to list lst if it is not already in there
(define (addunique lst x)
  (cond 
   ((null? lst)  (list x)) ; all checked add it
   (else (cond 
	  ((equal? x (car lst)) lst) ; found, quit search and don't add again
	  (else (cons (car lst) (addunique (cdr lst) x))) ; keep searching
	  ))))

;; Calculate averages of each column
(define (get-averages  indata)
  (let* ((avglst '()))

    (map (lambda (x) (set! avglst (append avglst (list 0.0)))) (car indata))

    (map (lambda (x) 

	   (set! avglst (accumvects x avglst)))
	 indata)
    (map (lambda (x) 
		(cond ((number? x) (/ x (length indata)))
		      (else "")))
		avglst)
    ))

;; Turn a C array of accounts into a scheme list of account names
(define (gnc:acctnames-from-list acctlist)
  (let ((anlist '()))
    (map (lambda(an) 
	   (set! anlist (append! anlist 
		   (list (gnc:account-get-name an))))) acctlist)
       anlist))

(define acctcurrency "USD")
(define acctname "")

(define (allsubaccounts accounts)
  (cond ((null? accounts) '())
	(else 
;	 (display (gnc:account-get-name (car accounts)))(newline)
	 (append 
	       (gnc:group-get-account-list 
		 (gnc:account-get-children (car accounts)))
	       (allsubaccounts (cdr accounts))))))

(gnc:define-report
 ;; version
 1
 ;; Name
 "Account Balance Tracker"
 ;; Options
 runavg-options-generator
 ;; renderer
 (lambda (options)
   (let* (
	  (begindate (gnc:option-value  
		      (gnc:lookup-option options "Report Options" "From")))
          (enddate (gnc:option-value
		    (gnc:lookup-option options "Report Options" "To")))
	  (stepsize (gnc:option-value
		     (gnc:lookup-option options "Report Options" "Step Size")))

	  (plotstr (gnc:option-value
		    (gnc:lookup-option options "Report Options" "Plot Type")))

	  (accounts (gnc:option-value  
		    (gnc:lookup-option options
				       "Report Options" "Account")))

	  (dosubs (gnc:option-value  
		    (gnc:lookup-option options
				       "Report Options" "Sub-Accounts")))

	  (prefix  (list "<HTML>" "<BODY>"))
	  (suffix  (list "</BODY>" "</HTML>"))
	  (collist
	   (list "Beginning" "Ending" "Average" "Max" "Min" "Net Gain" "Gain" "Loss"))

	  (report-lines '())
	  (rept-data '())
	  (sum-data '())
	  (tempstruct '())
	  (rept-text "")
	  (gncq (gnc:malloc-query))

	  (slist '())

	  )

     (gnc:init-query gncq)

     (if (null? accounts)
         (set! report-lines
               (list "<TR><TD>You have not selected an account.</TD></TR>"))
	 (begin
	   ; Grab account names
	   (set! acctname (gnc:account-get-name (car accounts)))
	   (map (lambda(an) 
		  (set! acctname
			(string-append 
			 acctname
			 " , "
			 (gnc:account-get-name an))))
		      (cdr accounts) )
	   
	   (cond ((equal? dosubs #t)
		  (map (lambda (a)
			 (set! accounts (addunique accounts a)))
		       (allsubaccounts accounts))

	       (set! acctname (string-append acctname " and sub-accounts"))
	       ))

	   (map (lambda(acct) (gnc:query-add-account gncq acct)) accounts)

     (set! tempstruct 
	   (build-mystruct-instance 
	    (define-mystruct 
		(gnc:acctnames-from-list accounts))))

     (set! acctcurrency (gnc:account-get-currency (car accounts)))

     (set! report-lines 
	   (gnc:convert-split-list (gnc:query-get-splits gncq)))))

     (gnc:free-query gncq)
	    
     (display (length report-lines))
     (display " Splits\n")

     ; Set initial balances to zero
     (map (lambda(an) (tempstruct 'put an 0)) 
	  (gnc:acctnames-from-list accounts))

	    (dateloop begindate
		       enddate
		       (eval stepsize))
     (set! rept-data 
	   (reduce-split-list
	    (dateloop begindate
		       enddate
		       (eval stepsize))
	    report-lines zdate tempstruct))

     (set! sum-data (get-averages rept-data))


     ;; Create HTML
     (set! rept-text 
	   (html-table 
	    collist
	    (append rept-data 
		    (list "<TR cellspacing=0><TD><TD><TD colspan=3><HR size=2 noshade><TD colspan=3><HR size=2 noshade></TR>" sum-data))))


     ;; Do a plot
     (if (not (equal? NoPlot (eval plotstr)))
	 (let* ((fn "/tmp/gncplot.dat")
	   (preplot (string-append
		     "set xdata time\n"
		     "set timefmt '%m/%d/%Y'\n"
		     "set pointsize 2\n"
		     "set title '" acctname "'\n"
		     "set ylabel '" acctcurrency "'\n"
		     "set xlabel 'Period Ending'\n"
		     )
		    )
	   )

       (data-to-gpfile collist  rept-data fn (eval plotstr))
	  (system 
	   (string-append "echo \"" preplot "plot '"  fn "'" (eval plotstr) 
			  "\"|gnuplot -persist " )))
	 )
     (append prefix (list "Report for " acctname "<p>\n" )
	     (list rept-text) suffix)))
)
