;; -*-scheme-*-
;; average-balance.scm
;; Report history of account balance and other info
;; Also graphs the information with gnuplot
;; Matt Martin <mgmartin@abacusnet.net>

(use-modules (ice-9 slib)(ice-9 regex))
(require 'hash-table)
(require 'printf)

;hack alert - is this line necessary?
(gnc:depend "text-export.scm")

(define datelist '())

;;  Add delta to date, return result
(define (incdate adate delta)
  (let ((newtm (localtime (car adate))))
    (begin
      (set-tm:mday newtm (+ (tm:mday newtm) (tm:mday delta)))
      (set-tm:mon newtm (+ (tm:mon newtm) (tm:mon delta)))
      (set-tm:year newtm (+ (tm:year newtm) (tm:year delta)))

       (let ((time (car (mktime newtm))))
	 (cons time 0))
       ))
  )

;; actual recursion for date list building
(define (dateloop curd endd incr) 
  (cond ((gnc:timepair-later-date curd endd)
	  (cons curd (dateloop (incdate curd incr) endd incr)))
	(else (list curd))
	)
  )

;; Create list of dates to report on
(define (generate-datelist beg nd stp)
  (set! datelist (dateloop beg nd (eval stp))))

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
            (num-accounts (gnc:group-get-num-accounts (gnc:get-current-group)))
            (first-account (gnc:group-get-account (gnc:get-current-group) 0)))
        (cond ((not (null? current-accounts)) (list (car current-accounts)))
              ((> num-accounts 0) (list first-account))
              (else ()))))
    #f #f))

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
   (gnc:make-multichoice-option
    "Report Options" "Plot Type"
    "b" "Get number at each one of these" 'NoPlot
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

(define AvgBalPlot "using 1:2:3:4 t 'Average Balance' with errorbars")
(define GainPlot "using 1:5 t 'Net Gain' with linespoints")
(define GLPlot "using 1:7 t 'Losses' with lp, '' using 1:6 t 'Gains' with lp")
(define NoPlot "")

;;; applies thunk to each split in account account
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

;; timepair manipulation functions
;; hack alert  - these should probably be put somewhere else
;; and be implemented PROPERLY rather than hackily

(define (gnc:timepair-to-datestring tp)
  (let ((bdtime (localtime (car tp))))
    (strftime "%m/%d/%Y" bdtime)))


;; Find difference in seconds (?) between time 1 and time2
(define (gnc:timepair-delta t1 t2)
  (let ((time1 (car (gnc:timepair-canonical-day-time t1))) 
        (time2 (car (gnc:timepair-canonical-day-time t2))))
    (- time2 time1)))

;; Don't know if these can be local+static to reduce-split-list
(define tempmax -1E10)
(define tempmin 1E10)
(define last 0)
(define zdate (cons 0 0))
(define prevdate zdate)
(define avgaccum 0)
(define lossaccum 0)
(define gainaccum 0)

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
  (string-append
   (sprintf #f "<TR>")
   (apply string-append (map html-table-col lst))
   (sprintf #f "</TR>\n")
   )
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
   (apply string-append "<TABLE cellspacing=10>" (map html-table-headcol vec))
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

;; Reset accumulators between time intervals
(define (resetvals bal curdate)
  (set! tempmax bal)
  (set! tempmin bal)
  (set! prevdate curdate)
  (set! avgaccum 0)
  (set! lossaccum 0)
  (set! gainaccum 0)
) 

;
; Reduce a list of splits tl to a list of values at each date in datelist dl
;
(define (reduce-split-list dl tl pt)
    (cond ((null? dl) '()) ;; End of recursion

	(else (let* ((bal last) ;; get Balance and datelist "time"
		     (ct (car dl))
		     (val 0))

		(begin
		  (cond ( (not (null? tl))
			  ;; Get latest split values if any remain
			  (set! bal (gnc:split-get-balance (car tl)))
			  (set! val (gnc:split-get-value (car tl)))
			  (set! ct (gnc:split-get-transaction-date (car tl)))
			))

		  (cond        ; past time interval bound ?
		   ((or(gnc:timepair-later-date (car dl) ct ) (null? tl))

		    (cond	; Is this the first interval ?
		     ((= (car zdate) (car prevdate) )
					; Start first date interval
		      (begin
			(resetvals bal (car dl))
			(reduce-split-list (cdr dl) tl (car dl))
			)
		      )
		     (else
		       (set! avgaccum ; sum up to now
			     (+ avgaccum 
				(* last (gnc:timepair-delta pt (car dl)))))

		      (cons	; form list of values for one "row"
			 (list 
			  (gnc:timepair-to-datestring (car dl))
			  (let ((dlta (gnc:timepair-delta prevdate (car dl))))
			    (cond ((= dlta 0) 0); Should never happen !
				  ((= avgaccum 0) bal)
				  (else  (/ avgaccum dlta))))
			  tempmax
			  tempmin
			  (- gainaccum lossaccum)
			  gainaccum
			  lossaccum
			 )
			(begin
			  (resetvals last (car dl))
			  (reduce-split-list (cdr dl) tl (car dl))
			  )
			)
		       )))
		    (else  ; mid-interval
		     (begin
		       (set! tempmax (max tempmax bal))
		       (set! tempmin (min tempmin bal))
		       (set! avgaccum 
			     (+ avgaccum 
				(* last (gnc:timepair-delta pt ct))))
		       (cond ((> val 0) (set! gainaccum (+ gainaccum val)))
			     (else (set! lossaccum (- lossaccum val)))
			     )
		       (set! last bal)
		       (reduce-split-list dl (cdr tl) ct)
		       ))
		    )
	)))))


(define acctcurrency "USD")
(define acctname "")

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
          (enddate (gnc:lookup-option options "Report Options" "To"))
	  (stepsize (gnc:lookup-option options "Report Options" "Step Size"))

	  (plotstr (gnc:option-value
		    (gnc:lookup-option options "Report Options" "Plot Type")))

	  (accounts (gnc:option-value  
		    (gnc:lookup-option options
				       "Report Options" "Account")))
	  (prefix  (list "<HTML>" "<BODY>"))
	  (suffix  (list "</BODY>" "</HTML>"))
	  (collist
	   (list "Ending" "Average" "Max" "Min" "Net Gain" "Gain" "Loss"))

	  (report-lines '())
	  (rept-data '())
	  (rept-text "")

	  )


     (generate-datelist 
      begindate
      (gnc:option-value enddate) 
      (gnc:option-value stepsize))

     (if (null? accounts)
         (set! report-lines
               (list "<TR><TD>You have not selected an account.</TD></TR>"))
	 (begin
	   (set! acctname (gnc:account-get-name (car accounts)))
	   (set! acctcurrency (gnc:account-get-currency (car accounts)))
           (gnc:for-each-split-in-account
            (car accounts)
            (lambda (split)		
	      (set! report-lines 
                    (append! report-lines (list split)))))))

     (display (length report-lines))
     (display " Splits\n")
     (set! prevdate zdate)
     (set! rept-data (reduce-split-list datelist report-lines (car datelist)))
     (set! rept-text 
	   (html-table 
	    collist
	    rept-data))


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

     (append prefix (list rept-text) suffix)))
)
