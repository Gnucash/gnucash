;; -*-scheme-*-
;; average-balance.scm
;; Report history of account balance and other info
;; Plots the information with gnuplot
;;
;; Author makes no implicit or explicit guarantee of accuracy of 
;;  these calculations and accepts no responsibility for direct
;;  or indirect losses incurred as a result of using this software.

(gnc:support "report/average-balance.scm")
(use-modules (ice-9 regex))
(require 'hash-table)

(gnc:depend "structure.scm")
(gnc:depend "report-utilities")
(gnc:depend "html-generator.scm")
(gnc:depend "date-utilities.scm")

;; Plot strings

(define AvgBalPlot
  (string-append
   "using 2:3:4:5 t 'Average Balance' with errorbars, "
   "'' using 2:3 smooth sbezier t '' with lines"))

(define GainPlot
  (string-append
   "using 2:6 t 'Net Gain' with linespoints, "
   "'' using 2:6 smooth sbezier t '' with lines" ))

(define GLPlot
  (string-append "using 2:8 t 'Losses' with lp, "
                 "'' using 2:7 t 'Gains' with lp"))

(define NoPlot "")

(let ()

  ;; Options
  (define (runavg-options-generator)
    (let*
        ((gnc:*runavg-track-options* (gnc:new-options))
         ;; register a configuration option for the report
         (gnc:register-runavg-option 
          (lambda (new-option)
            (gnc:register-option gnc:*runavg-track-options* 
                                 new-option))))

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
            (cons 'absolute (cons (car (mktime bdtime)) 0))))
        #f 
	'absolute #f))

      ;; to-date
      (gnc:register-runavg-option
       (gnc:make-date-option
        "Report Options" "To"
        "c" "Report items up to and including this date"
        (lambda ()
          (let ((bdtime (localtime (current-time))))
            (set-tm:sec bdtime 59)
            (set-tm:min bdtime 59)
            (set-tm:hour bdtime 23)
            (cons 'absolute (cons (car (mktime bdtime)) 0))))
        #f 'absolute #f))

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
                   (gnc:group-get-account-list (gnc:get-current-group))))))
        #f #t))

      (gnc:register-runavg-option
       (gnc:make-multichoice-option
        "Report Options" "Step Size"
        "b" "The amount of time between data points" 'WeekDelta
        (list #(DayDelta "Day" "Day")
              #(WeekDelta "Week" "Week")
              #(TwoWeekDelta "2Week" "Two Week")
              #(MonthDelta "Month" "Month")
              #(YearDelta "Year" "Year")
              )))

      (gnc:register-runavg-option
       (gnc:make-simple-boolean-option
        "Report Options" "Sub-Accounts"
        "e" "Include sub-accounts of all selected accounts" #f))

      (gnc:register-runavg-option
       (gnc:make-multichoice-option
        "Report Options" "Plot Type"
        "f" "The type of graph to generate" 'NoPlot
        (list #(NoPlot "Nothing" "Make No Plot")
              #(AvgBalPlot "Average" "Average Balance")
              #(GainPlot "Net Gain" "Net Gain")
              #(GLPlot "Gain/Loss" "Gain And Loss"))))
      gnc:*runavg-track-options*))

  ;; Text table 

  ; Create an text table row from a list of entries
  (define (text-table-row lst)
    (string-append 
     (tostring (car lst))

     (apply string-append (map (lambda (val)
                                 (string-append "\t" (tostring val)))
                               (cdr lst)))
     "\n"))

  (define (text-table-header lst)
    (string-append
     "# " 
     (text-table-row lst)))

  (define (text-table hdrlst llst)
    (string-append
     (text-table-header hdrlst)
     (apply string-append (map text-table-row llst))))

  ; Quick and dirty until there is REAL plot support
  (define (data-to-gpfile hdrlst llst fn plotcmd)
    (let ((oport (open-output-file fn)))
      (display  
       (text-table hdrlst llst)
       oport)
      (close-output-port oport)))

  ;; Returns sum of all vector elements after the first
  (define (vector-sum v)
    (let ((sum 0)) 
      (gnc:for-loop 
       (lambda(i) (set! sum (+ sum (car (vector-ref v i))))) 
       1 (vector-length v) 1)
      sum))

  (define (reduce-split-list deltas splits balance)

    ;; A hash table to store account balances
    (define balances (make-hash-table 313))

    (define (reduce-splits deltas splits)
      (let ((stat-accumulator (make-stats-collector))
            (min-max-accumulator (make-stats-collector))
            (gl-accumulator (makedrcr-collector))
            (prevdate 0))

        ;; accbal runs the accumulator
        (define (accbal start end)
          (stat-accumulator 'add
                            (* (gnc:timepair-delta start end) balance))
          (min-max-accumulator 'add balance))

        (define (update-balance split)
          (let* ((account       (gnc:split-get-account split))
                 (split-balance (d-gnc:split-get-balance split))
                 (last-balance  (hash-ref balances account)))
            (hash-set! balances account split-balance)
            (if last-balance
                (set! split-balance (- split-balance last-balance)))
            (set! balance (+ balance split-balance))
            split-balance))

        (define (calc-in-interval delta splits)
          (let ((start (car delta))
                (end   (cadr delta)))
            (if (null? splits)
                (begin
                  (accbal prevdate end)
                  '())
                (let* ((split (car splits))
                       (now (gnc:split-get-transaction-date split)))
                  (cond ((gnc:timepair-lt now start) ;split before interval
                         (update-balance split)
                         (calc-in-interval delta (cdr splits)))

                        ((gnc:timepair-lt now end) ;split is in the interval
                         (accbal prevdate now)
                         (set! prevdate now)
                         (gl-accumulator 'add (update-balance split))
                         (calc-in-interval delta (cdr splits)))

                        (else           ; split is past interval
                         (accbal prevdate end)
                         splits))))))

        ;; Actual routine
        (if (null? deltas)
            '()                         ; end of recursion
            (let* ((delta (car deltas))
                   (start (car delta))
                   (end   (cadr delta)))

              ;; Reset accumulator values
              (set! prevdate start)
              (stat-accumulator 'reset #f)
              (min-max-accumulator 'reset #f)
              (gl-accumulator 'reset #f)

              (let ((rest (calc-in-interval delta splits)))
                ;; list of values for report
                (cons
                 (list start
                       end
                       (/ (stat-accumulator 'total #f)
                          (gnc:timepair-delta start end))
                       (min-max-accumulator 'getmax #f)
                       (min-max-accumulator 'getmin #f)
                       (- (gl-accumulator 'debits #f)
                          (gl-accumulator 'credits #f))
                       (gl-accumulator 'debits #f) 
                       (gl-accumulator 'credits #f))
                 (reduce-splits (cdr deltas) rest)))))))

    (reduce-splits deltas splits))

  (define (format-numbers-in-list list)
    (define print-info (gnc:default-print-info #f))
    (define (format-internal list)
      (cond ((null? list) '())
            ((number? (car list))
             (cons (gnc:amount->string (car list) print-info)
                   (format-internal (cdr list))))
            (else
             (cons (car list)
                   (format-internal (cdr list))))))
    (format-internal list))

  (define (format-reduced-list list)
    (define (reduce-line line)
      (let ((start (car  line))
            (end   (cadr line))
            (rest  (cddr line)))
        (cons (gnc:print-date start)
              (cons (gnc:print-date end)
                    (format-numbers-in-list rest)))))
    (if (null? list)
        '()
        (cons (reduce-line (car list))
              (format-reduced-list (cdr list)))))

  (define (gnc:timepair-to-gnuplot-string tp)
    (let ((bdtime (localtime (car tp))))
      (strftime "%m/%d/%Y" bdtime)))

  (define (gnuplot-reduced-list list)
    (define (reduce-line line)
      (let ((start (car  line))
            (end   (cadr line))
            (rest  (cddr line)))
        (cons (gnc:timepair-to-gnuplot-string start)
              (cons (gnc:timepair-to-gnuplot-string end) rest))))
    (if (null? list)
        '()
        (cons (reduce-line (car list))
              (gnuplot-reduced-list (cdr list)))))

  (define (accumvects x y)
    (cond 
     ((null? x) '())
     ((number? (car x)) 
      (cons  (+ (car x) (car y)) (accumvects (cdr x) (cdr y))))
     (else (cons "x" (accumvects (cdr x) (cdr y))))))

  ;; Add x to list lst if it is not already in there
  (define (addunique lst x)
    (cond 
     ((null? lst)  (list x))           ; all checked add it
     (else (cond 
            ((equal? x (car lst)) lst) ; found, quit search and don't add again
            (else (cons (car lst) (addunique (cdr lst) x))) ; keep searching
            ))))

  ;; Calculate averages of each column
  (define (get-averages indata)
    (let ((avglst '()))
      (set! avglst (map (lambda (x) 0.0) (car indata)))
      (for-each (lambda (x)
                  (set! avglst (accumvects x avglst)))
                indata)
      (map (lambda (x) 
             (cond ((number? x) (/ x (length indata)))
                   (else "")))
           avglst)))

  (define (allsubaccounts accounts)
    (cond ((null? accounts) '())
          (else 
           (append 
            (or
             (gnc:group-get-subaccounts
              (gnc:account-get-children (car accounts)))
             '())
            (allsubaccounts (cdr accounts))))))

  (define string-db (gnc:make-string-database))

  (define (column-list)
    (map (lambda (key) (string-db 'lookup key))
         (list 'beginning 'ending 'average 'max 'min 'net-gain 'gain 'loss)))

  (define (average-balance-renderer options)
    (let ((gov-fun (lambda (value)
                     (gnc:option-value (gnc:lookup-option 
                                        options "Report Options"
                                        value)))))
      (let ((acctcurrency "USD")
            (acctname "")
            (begindate (gnc:date-option-absolute-time (gov-fun "From")))
            (enddate (gnc:timepair-end-day-time (gnc:date-option-absolute-time(gov-fun "To"))))
            (stepsize (eval (gov-fun "Step Size")))
            (plotstr (eval (gov-fun "Plot Type")))
            (accounts (gov-fun "Account"))
            (dosubs (gov-fun "Sub-Accounts"))
            (prefix  (list "<HTML>" "<BODY bgcolor=#fff8c7>"))
            (suffix  (list "</BODY>" "</HTML>"))
            (columns (column-list))
            (report-lines '())
            (rept-data '())
            (sum-data '())
            (rept-text "")
            (gncq (gnc:malloc-query))
            (slist '()))

        (cond ((null? accounts)
	       (set! rept-text
                  (list "<TR><TD>"
                        (string-db 'lookup 'no-account)
                        "</TD></TR>")))
	      ((gnc:timepair-le enddate begindate)
	       (set! rept-text
		    (list "<TR><TD><EM>"
			  (string-db 'lookup 'dates-reversed)
			  "</EM></TD></TR>")))
            (else (begin

              ; Grab account names
              (set! acctname
                    (string-join (map gnc:account-get-name accounts) " , "))

              (if dosubs
                  (map (lambda (a)
                         (set! accounts (addunique accounts a)))
                       (allsubaccounts accounts)))

              (gnc:query-set-group gncq (gnc:get-current-group))

              (map (lambda (acct)
                     ;; FIXME - the '1' below is hard-coded and should
                     ;;         be abstracted. Just a temp fix while
                     ;;         the query api gets fully wrapped.
                     (gnc:query-add-single-account-match gncq acct
                                                         (cons 'query-op 1)))
                   accounts)

              (set! acctcurrency 
                    (gnc:commodity-get-printname 
                     (gnc:account-get-currency (car accounts))))

              (set! report-lines 
                    (gnc:convert-split-list (gnc:query-get-splits gncq)))

              (gnc:free-query gncq)

              (set! rept-data
                    (reduce-split-list (dateloop begindate enddate stepsize)
                                       report-lines 0))

              (set! sum-data (get-averages rept-data))

              ;; Create HTML
              (set! rept-text 
                    (html-table #f
                     columns
                     (append (format-reduced-list rept-data)
                             (list "<TR cellspacing=0><TD><TD><TD colspan=3><HR size=2 noshade><TD colspan=3><HR size=2 noshade></TR>"
                                   (format-numbers-in-list sum-data)))))

              ;; Do a plot
              (if (not (string=? NoPlot plotstr))
                  (let ((fn "/tmp/gncplot.dat")
                        (preplot (string-append
                                  "set xdata time\n"
                                  "set timefmt '%m/%d/%Y'\n"
                                  "set pointsize 2\n"
                                  "set title '" acctname "'\n"
                                  "set ylabel '" acctcurrency "'\n"
                                  "set xlabel '"
                                  (string-db 'lookup 'period-ending)
                                  "'\n")))

                    (data-to-gpfile columns (gnuplot-reduced-list rept-data)
                                    fn plotstr)
                    (system 
                     (string-append "echo \"" preplot "plot '"
                                    fn "'" plotstr
                                    "\"|gnuplot -persist " )))))))

        (append prefix
                (if (null? accounts)
                    '()
                    (list (sprintf #f
                                   (string-db 'lookup
                                              (if dosubs
                                                  'report-for-and
                                                  'report-for))
                                   acctname)
                          "<p>\n"))
                (list rept-text)
                suffix))))
  
  ;; Define the strings
  (string-db 'store 'beginning "Beginning")
  (string-db 'store 'ending "Ending")
  (string-db 'store 'average "Average")
  (string-db 'store 'max "Max")
  (string-db 'store 'min "Min")
  (string-db 'store 'net-gain "Net Gain")
  (string-db 'store 'gain "Gain")
  (string-db 'store 'loss "Loss")
  (string-db 'store 'no-account "You have not selected an account.")
  (string-db 'store 'dates-reversed "Please choose appropriate dates - the \"To\" date should be *after* the \"From\" date.")
  (string-db 'store 'period-ending "Period Ending")
  (string-db 'store 'report-for "Report for %s.")
  (string-db 'store 'report-for-and "Report for %s and all subaccounts.")

  (gnc:define-report
   'version 1
   'name "Account Balance Tracker"
   'options-generator runavg-options-generator
   'renderer average-balance-renderer))
