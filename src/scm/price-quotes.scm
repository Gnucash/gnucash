;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; price-quotes.scm - manage sub-processes.
;;; Copyright 2001 Rob Browning <rlb@cs.utexas.edu>
;;; 
;;; This program is free software; you can redistribute it and/or    
;;; modify it under the terms of the GNU General Public License as   
;;; published by the Free Software Foundation; either version 2 of   
;;; the License, or (at your option) any later version.              
;;;                                                                  
;;; This program is distributed in the hope that it will be useful,  
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;;; GNU General Public License for more details.                     
;;;                                                                  
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, contact:
;;;
;;; Free Software Foundation           Voice:  +1-617-542-5942
;;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash price-quotes))

(export yahoo-get-historical-quotes)
(export gnc:session-add-quotes)
(export gnc:add-quotes-to-book-at-url)

(use-modules (gnucash process))
(use-modules (www main))
(use-modules (srfi srfi-1))
(use-modules (gnucash bootstrap) (g-wrapped gw-gnc)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/app-utils" 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Yahoo-based Historical Quotes
;;

(use-modules (www main))
(use-modules (srfi srfi-1))

;; (use-modules (srfi srfi-19)) when available (see below).

(define (yahoo-get-historical-quotes symbol
                                     start-year start-month start-day
                                     end-year end-month end-day)
  ;; symbol must be a string naming the item of interest
  ;; (i.e. "LNUX"), and all the other arguments must be integers.
  ;; Abbreviated 2 digit years are not allowed, and months and days
  ;; are numbered starting with 1.
  ;;
  ;; This function returns a list of alists containing the quote data,
  ;; or #f on failure.  Each alist will look like this:
  ;;
  ;; ((date . "21-Dec-1999")
  ;;  (open . 108.5)
  ;;  (high . 110.12)
  ;;  (low . 108.06)
  ;;  (close . 110.12)
  ;;  (volume . 4775500))
  ;;
  ;; Note that the dates are left as strings, but the years will
  ;; always be 4 digits.  The dates are left as strings for now
  ;; because without additional timezone information, it is impossible
  ;; to perform the conversion to UTC here.  Further, it's not at all
  ;; clear which UTC instant each price should represent...
  ;;
  ;; NOTE: right now, we can't handle dates before 1970.  That's
  ;; because to properly handle the fact that yahoo returns lame-ass 2
  ;; digit years, we need to be able to compare the dates it returns
  ;; using (+ year 1900) and (+ year 2000) to see which one is within
  ;; the star-end range requested (at least that's one of the easiest
  ;; ways to handle the problem).  The most straightforward way to do
  ;; this is via mktime, but it can't handle anything before the
  ;; epoch.  However, I believe SRFI-19 can
  ;; (http://srfi.schemers.org/srfi-19/srfi-19.html), so as soon as we
  ;; have a working implementation in guile, we can switch to that and
  ;; handle essentially arbitrary ranges.
  ;;
  ;; For now we'll leave in the mktime based conversion code
  ;; (commented out) so it'll be easy to switch later, but we'll
  ;; actually use a simpler (and more broken) approach -- any 2 digit
  ;; date >= 70 gets 1900 added, and any 2 digit date < 70 gets 2000
  ;; added.

;    (define (str->month month-str)
;     (cond
;      ((string-ci=? "Jan" month-str) 1)
;      ((string-ci=? "Feb" month-str) 2)
;      ((string-ci=? "Mar" month-str) 3)
;      ((string-ci=? "Apr" month-str) 4)
;      ((string-ci=? "May" month-str) 5)
;      ((string-ci=? "Jun" month-str) 6)
;      ((string-ci=? "Jul" month-str) 7)
;      ((string-ci=? "Aug" month-str) 8)
;      ((string-ci=? "Sep" month-str) 9)
;      ((string-ci=? "Oct" month-str) 10)
;      ((string-ci=? "Nov" month-str) 11)
;      ((string-ci=? "Dec" month-str) 12)
;      (else #f)))

;   (define (ymd->secs year month day)
;     (let ((timevec (localtime 0)))
;       (display (list 'foo year month day)) (newline)
;       (set-tm:sec timevec 59)
;       (set-tm:min timevec 59)
;       (set-tm:hour timevec 23)
;       (set-tm:mday timevec day)
;       (set-tm:mon timevec (- month 1))
;       (set-tm:year timevec (- year 1900))
;       (set-tm:wday timevec -1)
;       (set-tm:yday timevec -1)
;       (set-tm:isdst timevec -1)
;       (display timevec) (newline)
;       (car (mktime timevec))))

;   (define (fix-lame-ass-date-if-needed! quote)
;     (let* ((date-str (vector-ref quote 0))
;            (date-list (and date-str (string-split date-str #\-)))
;            (year-str (and date-list (caddr date-list))))
;       (if (= (string-length year-str) 2)
;           (let* ((day (string->number (car date-list)))
;                  (month (str->month (cadr date-list)))
;                  (year (string->number year-str))
;                  (start-secs (ymd->secs start-year start-month start-day))
;                  (end-secs (ymd->secs end-year end-month end-day))
;                  (guess-1900 (ymd->secs (+ year 1900) month day)))
            
;             (if (and (>= guess-1900 start-secs)
;                      (<= guess-1900 end-secs))
;                 (vector-set! quote 0 (string-append (car date-list) "-"
;                                                     (cadr date-list) "-"
;                                                     (number->string
;                                                      (+ year 1900))))
;                 (let ((guess-2000 (ymd->secs (+ year 2000) month day)))
                  
;                   (if (and (>= guess-2000 start-secs)
;                            (<= guess-2000 end-secs))
;                       (vector-set! quote 0 (string-append (car date-list) "-"
;                                                           (cadr date-list) "-"
;                                                           (number->string
;                                                            (+ year 2000))))
;                       (vector-set! quote 0 #f))))))))

  (define (fix-lame-ass-date-if-needed! quote-data)
    (let* ((date-str (assq-ref quote-data 'date))
           (date-list (and date-str (string-split date-str #\-)))
           (year-str (and date-list (caddr date-list))))

      (if (= (string-length year-str) 2)
          (let* ((day (car date-list))
                 (month (cadr date-list))
                 (year (string->number year-str)))
            (assq-set!
             quote-data
             'date
             (string-append (car date-list) "-"
                            (cadr date-list) "-"
                            (number->string
                             (+ year (if (>= year 70)
                                         1900
                                         2000)))))))))

  (define (quote-line->quote-alist line)
    (let ((fields (string-split line #\,)))
      (cond 
       ((= 6 (length fields))
        (map
         (lambda (name value) (cons name value))
         '(date open high low close volume)
         (cons (car fields) (map string->number (cdr fields)))))
       ((zero? (string-length line))
        'ignore)
       (else
        #f))))
  
  (define (csv-str->quotes str)
    (let ((lines (string-split str #\newline)))
      (if (string=? (car lines) "Date,Open,High,Low,Close,Volume")
          (let ((result (map quote-line->quote-alist (cdr lines))))
            (if (any not result)
                #f
                (begin
                  (set! result
                        (filter (lambda (x) (not (eq? 'ignore x))) result))
                  (for-each fix-lame-ass-date-if-needed! result)
                  result)))
          #f)))
  
  (if (< start-year 1970)
      #f
      (let* ((request (string-append
                       "http://chart.yahoo.com/table.csv?"
                       "s=" symbol
                       "&a=" (number->string start-month)
                       "&b=" (number->string start-day)
                       "&c=" (number->string start-year)
                       "&d=" (number->string end-month)
                       "&e=" (number->string end-day)
                       "&f=" (number->string end-year)
                       "&g=d&q=q&y=0"))
             (result (www:get request)))
        
        (if result
            (or (csv-str->quotes result)
                result)
            #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finance::Quote based instantaneous quotes -- used by the
;; --add-price-quotes command line option, etc.
;;
;; Note From: Dave Peticolas <dave@krondo.com> Date: Sun, 01 Apr 2001
;; Those aren't pricedb functions, those are online quote functions,
;; i.e., low-level functions for getting online-quotes and putting
;; them into the price db.  Reports should not be using those
;; functions, they should be using the price db. See
;; src/engine/gnc-pricedb.h

(define gnc:*finance-quote-helper*
  (string-append (gnc:config-var-value-get gnc:*share-dir*)
                 "/finance-quote-helper"))

(define (gnc:fq-get-quotes requests)
  ;; requests should be a list where each item is of the form
  ;;
  ;; (<fq-method> sym sym ...)
  ;;
  ;; i.e. (yahoo "RHAT" "LNUX" "IBM")
  ;;
  ;; for currencies, we have
  ;;
  ;; (currency "USD" "AUD") for the conversion from USD to AUD,
  ;;                        i.e., the price of USD in AUD.
  ;;
  ;; This function will return #f on catastrophic failure or a list
  ;; where, for each element in requests, the output list will contain
  ;; a quote-result element. This element will be #f or an error
  ;; symbol if the corresponding method call fails, or a list
  ;; otherwise. A quote-result list will contain the symbol
  ;; representing the item being quoted, followed by an alist
  ;; detailing the quote data from finance-quote-helper.
  ;;
  ;; Possible error symbols and their meanings are:
  ;;   missing-lib    One of the required perl libs is missing
  ;;
  ;; So for the example method call above, the resulting item in the
  ;; output list might look like this:
  ;;
  ;; (("RHAT" (symbol . "RHAT") (gnc:time-no-zone . "...")
  ;;   (last . 6.59375) (currency . "USD"))
  ;;  ("LNUX" (symbol . "LNUX") (gnc:time-no-zone . "...")
  ;;   (last . 3.5) (currency . "USD"))
  ;;  ("IBM" (symbol . "IBM") (gnc:time-no-zone . "...")
  ;;   (last . 104.42) (currency . "USD")))
  ;;
  ;; Also note that any given value in the alist might be
  ;; 'failed-conversion if the Finance::Quote result for that field
  ;; was unparsable.  See the finance-quote-helper for more details
  ;; about it's output.

  (let ((quoter #f))

    (define (start-quoter)
      (set! quoter (gnc:run-sub-process #f
                                        gnc:*finance-quote-helper*
                                        gnc:*finance-quote-helper*)))

    (define (get-quotes)
      (and quoter
           (let ((to-child (caddr quoter))
                 (from-child (cadr quoter)))
             (map
              (lambda (request)
                (write (list 'handling-request request))
                (newline)
                ;; we need to display the first element (the method, so it
                ;; won't be quoted) and then write the rest
                (display #\( to-child)
                (display (car request) to-child)
                (display " " to-child)
                (for-each (lambda (x) (write x to-child)) (cdr request))
                (display #\) to-child)
                (newline to-child)
                (force-output to-child)
                (read from-child))
             requests))))

    (define (kill-quoter)
      (and quoter
           (let ((pid (car quoter)))
             (close-input-port (cadr quoter))
             (close-output-port (caddr quoter))
             (gnc:cleanup-sub-process (car quoter) 1))))

    (dynamic-wind
        start-quoter
        get-quotes
        kill-quoter)))

(define (gnc:session-add-quotes session)

  (define (find-quotables group)
    ;; Return a list of accounts for whose commodities we should get
    ;; quotes.
    (define (quotable-currency-account? a)
      (let ((commodity (gnc:account-get-commodity a)))
        (equal? (gnc:commodity-get-namespace commodity) "ISO4217")))

    (define (quotable-account? a)
      (let ((type (gw:enum-<gnc:AccountType>-val->sym (gnc:account-get-type a)
                                                      #f))
	    (src (gnc:account-get-price-src a)))

        (if (not type) (set! type '()))
        (if (symbol? type) (set! type (list type)))
        (if (and src
		 (or (memq 'stock type)
		     (memq 'mutual-fund type)
		     (and (memq 'currency type)
                          (quotable-currency-account? a))))
	    a
	    #f)))

    (filter quotable-account? (gnc:group-get-subaccounts group)))

  (define (accounts->fq-call-data account-list)
    ;; Take a list of accounts that should be "quotable" -- i.e. they
    ;; have a price-source, and are of the right type, and return a
    ;; list of info needed for the relevant Finance::Quote calls, and
    ;; a list of the corresponding commodities.  Also perform a bit of
    ;; optimization, merging calls for symbols to the same
    ;; Finance::Quote method.
    ;; 
    ;; Returns a list of the info needed for a set of calls to
    ;; finance-quote-helper.  Each item will of the list will be of the
    ;; form:
    ;;
    ;; (("yahoo" (commodity-1 currency-1 tz-1)
    ;;           (commodity-2 currency-2 tz-2) ...)
    ;;  ("fidelity_direct" (commodity-3 currency-3 tz-3)
    ;;                     (commodity-4 currency-4 tz-4) ...)
    ;;  ...)

  (define (src->fq-method-sym src)
    (cond
     ((string=? "YAHOO" src) 'yahoo)
     ((string=? "YAHOO_EUROPE" src) 'yahoo-europe)
     ((string=? "FIDELITY" src) 'fidelity)
     ((string=? "TRPRICE" src) 'troweprice)
     ((string=? "VANGUARD" src) 'vanguard)
     ((string=? "ASX" src) 'asx)
     ((string=? "TIAACREF" src) 'tiaacref)
     ((string=? "TRUSTNET" src) 'trustnet)
     ((string=? "CURRENCY" src) 'currency)
     (else #f)))

  ;; NOTE: If you modify this, please update finance-quote-helper.in as well.
  (define (fq-method-sym->str src-sym)
    (case src-sym
     ((yahoo) "yahoo")
     ((yahoo-europe) "yahoo_europe")
     ((fidelity) "fidelity_direct")
     ((troweprice) "troweprice_direct")
     ((vanguard) "vanguard")
     ((asx) "asx")
     ((tiaacref) "tiaacref")
     ((trustnet) "trustnet")
     ((currency) "currency")
     (else #f)))

  (define (account->fq-cmd account)
    ;; Returns (cons fq-method-sym
    ;;               (list commodity currency assumed-timezone-str))
    (let* ((commodity (gnc:account-get-commodity account))
           (currency (gnc:default-currency))
           (src (and account (gnc:account-get-price-src account)))
           (tz (gnc:account-get-quote-tz account))
	   (fq-method-sym (and src (src->fq-method-sym src)))
           (mnemonic (and commodity (gnc:commodity-get-mnemonic commodity))))
      (and
       commodity
       currency
       fq-method-sym
       mnemonic
       (list fq-method-sym commodity currency tz))))

  (let* ((big-list (delete #f (map account->fq-cmd account-list)))
         (cmd-list #f)
         (currency-cmd-list (call-with-values 
                             (lambda () (partition!
                                         (lambda (cmd)
                                           (not (eq? (car cmd) 'currency)))
                                         big-list))
                             (lambda (a b) (set! cmd-list a) b)))
         (cmd-hash (make-hash-table 31)))

    ;; Now collect symbols going to the same backend.
    (item-list->hash! cmd-list cmd-hash car cdr hashq-ref hashq-set! #t)

    ;; Now translate to just what finance-quote-helper expects.
    (append
     (hash-fold
      (lambda (key value prior-result)
        (cons (cons (fq-method-sym->str key) value)
              prior-result))
      '()
      cmd-hash)
     (map (lambda (cmd) (cons (fq-method-sym->str (car cmd)) (list (cdr cmd))))
          currency-cmd-list))))

  (define (fq-call-data->fq-calls fq-call-data)
    ;; take an output element from accounts->fq-call-data and return a
    ;; list where the gnc_commodities have been converted to their
    ;; fq-suitable symbol strings.  i.e. turn the former into the
    ;; latter:
    ;;
    ;; ("yahoo" (commodity-1 currency-1 tz-1)
    ;;          (commodity-2 currency-2 tz-2) ...)
    ;; 
    ;; ("yahoo" "IBM" "AMD" ...)
    ;;

    (if (equal? (car fq-call-data) "currency")
        (map (lambda (quote-item-info)
               (list (car fq-call-data)
                     (gnc:commodity-get-mnemonic (car quote-item-info))
                     (gnc:commodity-get-mnemonic (cadr quote-item-info))))
             (cdr fq-call-data))
        (list
         (cons (car fq-call-data)
               (map
                (lambda (quote-item-info)
                  (gnc:commodity-get-mnemonic (car quote-item-info)))
                (cdr fq-call-data))))))

  (define (fq-results->commod-tz-quote-triples fq-call-data fq-results)
    ;; Change output of gnc:fq-get-quotes to a list of (commod
    ;; timezone quote) triples using the matching commodity items from
    ;; fq-call-data.
    ;;
    ;; This function presumes that fq-call-data is "correct" -- it
    ;; contains the correct number of calls, and has the commodity
    ;; pointers in all the right places.  If not, then the results of
    ;; this function are undefined.
    ;;
    ;; If there's a catatstrophic error, this function might return
    ;; #f.  If there's an error for any given input element, there
    ;; will be a pair like this in the output (#f . <commodity>)
    ;; indicating the commodity for which the quote failed.
    ;;
    ;; If this function doesn't return #f, it will return a list with
    ;; as many elements as there were commodities in the fq-call-data.
    ;;
    ;; We might want more sophisticated error handling later, but this
    ;; will do for now .
    (let ((result-list '()))

      (define (process-a-quote call-data call-result)
        ;; data -> (commod-1 currency-1 tz-1)
        ;; result -> (commod-1-sym . result-alist) or some kind of garbage.
        (if (and (list? call-result)
                 (not (null? call-result))
                 (list? (cdr call-result))
                 (every
                  (lambda (alist-item)
                    (and (pair? alist-item)
                         (not (eq? 'failed-conversion (cdr alist-item)))))
                  (cdr call-result)))
            ;; OK, data is good (as far as we can tell.
            (set! result-list
                  (cons (list (car call-data)
                              (caddr call-data)
                              (cdr call-result))
                        result-list))
            (set! result-list
                  (cons (cons #f (car call-data))
                        result-list))))

      (define (process-call-result-pair call-data call-result)
        (if (and (list? call-result)
                 (= (length call-data) (+ 1 (length call-result))))

            ;; OK, continue.
            (for-each process-a-quote (cdr call-data) call-result)

            ;; else badly formed result, must assume all garbage.
            (for-each
             (lambda (call-item)
               (set! result-list (cons (cons #f (car call-item)) result-list)))
             (cdr call-data))))

      (and (list? fq-call-data)
           (list? fq-results)
           (= (length fq-call-data) (length fq-results))
           (begin
             (for-each process-call-result-pair
                       fq-call-data
                       fq-results)
             (reverse result-list)))))

  (define (timestr->time-pair timestr time-zone)
    (let ((broken-down (strptime "%Y-%m-%d %H:%M:%S" timestr)))
      (if (not (= (string-length timestr) (cdr broken-down)))
          #f
          (cons (car (if time-zone
                         (mktime (car broken-down) time-zone)
                         (mktime (car broken-down))))
                0))))

  (define (commodity-tz-quote-triple->price book c-tz-quote-triple)
    ;; return a string like "NASDAQ:CSCO" on error, or a price on
    ;; success.  Presume that F::Q currencies are ISO4217 currencies.
    (let* ((commodity (first c-tz-quote-triple))
           (time-zone (second c-tz-quote-triple))
           (quote-data (third c-tz-quote-triple))
           (gnc-time (assq-ref quote-data 'gnc:time-no-zone))
           (price #f)
           (price-type #f)
           (currency-str (assq-ref quote-data 'currency))
           (commodity-table (gnc:book-get-commodity-table book))
           (currency
            (and commodity-table
                 (string? currency-str)
                 (gnc:commodity-table-lookup commodity-table
                                             "ISO4217"
                                             (string-upcase currency-str)))))

      (or-map (lambda (price-sym)
                (let ((p (assq-ref quote-data price-sym)))
                  (if p
                      (begin (set! price p)
                             (set! price-type price-sym)
                             #t)
                      #f)))
              '(last nav price))

      (set! price-type
            (case price-type
              ((last) "last")
              ((nav) "nav")
              ((price) "unknown")
              (else #f)))

      ;; FIXME: SIGFIGS is not what we want here...
      (if price
          (set! price
                (gnc:double-to-gnc-numeric price
                                           GNC-DENOM-AUTO
                                           (logior (GNC-DENOM-SIGFIGS 9)
                                                   GNC-RND-ROUND))))

      (if gnc-time
          (set! gnc-time (timestr->time-pair gnc-time time-zone))
          (set! gnc-time (gnc:get-today)))

      (if (not (and commodity currency gnc-time price price-type))
          (string-append
           currency-str ":" (gnc:commodity-get-mnemonic commodity))
          (let ((gnc-price (gnc:price-create session)))
            (if (not gnc-price)
                (string-append
                 currency-str ":" (gnc:commodity-get-mnemonic commodity))
                (begin
                  (gnc:price-set-commodity gnc-price commodity)
                  (gnc:price-set-currency gnc-price currency)
                  (gnc:price-set-time gnc-price gnc-time)
                  (gnc:price-set-source gnc-price "Finance::Quote")
                  (gnc:price-set-type gnc-price price-type)
                  (gnc:price-set-value gnc-price price)
                  gnc-price))))))

  (define (book-add-prices! book prices)
    (let ((pricedb (gnc:book-get-pricedb book)))
      (for-each
       (lambda (price)
         (gnc:pricedb-add-price pricedb price)
         (gnc:price-unref price))
       prices)))

  ;; FIXME: uses of gnc:warn in here need to be cleaned up.  Right
  ;; now, they'll result in funny formatting.

  (let* ((book (gnc:session-get-book session))
         (group (gnc:book-get-group book))
         (quotables (and group (find-quotables group)))
         (fq-call-data (and quotables (accounts->fq-call-data quotables)))
         (fq-calls (and fq-call-data
                        (apply append
                               (map fq-call-data->fq-calls fq-call-data))))
         (fq-results (and fq-calls (gnc:fq-get-quotes fq-calls)))
         (commod-tz-quote-triples
          (and fq-results (not (member 'missing-lib fq-results))
               (fq-results->commod-tz-quote-triples fq-call-data fq-results)))
         ;; At this point commod-tz-quote-triples will either be #f or a
         ;; list of items. Each item will either be (commodity
         ;; timezone quote-data) or (#f . problem-commodity)
         (problem-syms
          (and commod-tz-quote-triples
               (filter-map (lambda (cq-pair)
                             (if (car cq-pair)
                                 #f
                                 (string-append
                                  (gnc:commodity-get-namespace (cdr cq-pair))
                                  ":"
                                  (gnc:commodity-get-mnemonic (cdr cq-pair)))))
                           commod-tz-quote-triples)))
         ;; strip out the "bad" ones from above.
         (ok-syms
          (and commod-tz-quote-triples
               (filter car commod-tz-quote-triples)))
         (keep-going? #t))

    (cond
     ((member 'missing-lib fq-results)
      (set! keep-going? #f)
      (if (gnc:ui-is-running?)
          (gnc:error-dialog
           (_ "You are missing some needed Perl libraries.
Run 'update-finance-quote' as root to install them."))
          (gnc:warn (_ "You are missing some needed Perl libraries.
Run 'update-finance-quote' as root to install them.") "\n")))
     ((and (not commod-tz-quote-triples) (gnc:ui-is-running?))
      (gnc:error-dialog
       (_ "Unable to get quotes or diagnose the problem."))
       (set! keep-going? #f))
     ((not commod-tz-quote-triples)
      (gnc:warn (_ "Unable to get quotes or diagnose the problem."))
      (set! keep-going? #f))
     ((not (null? problem-syms))
      (if (gnc:ui-is-running?)
          (if (and ok-syms (not (null? ok-syms)))
              (set!
               keep-going?
               (gnc:verify-dialog
                (call-with-output-string
                 (lambda (p)
                   (display (_ "Unable to retrieve quotes for these items:") p)
                   (newline p)
                   (display "  " p)
                   (display (string-join problem-syms "\n  ") p)
                   (newline p)
                   (display (_ "Continue using only the good quotes?") p)))
                #t))
              (begin
                (gnc:error-dialog
                 (call-with-output-string
                  (lambda (p)
                    (display
                     (_ "Unable to retrieve quotes for these items:") p)
                    (newline p)
                    (display "  " p)
                    (display (string-join problem-syms "\n  ") p))))
                (set! keep-going? #f)))
          (gnc:warn
           (call-with-output-string
            (lambda (p)
              (display (_ "Unable to retrieve quotes for these items:") p)
              (newline p)
              (display "  " p)
              (display (string-join problem-syms "\n  ") p)
              (newline p)
              (display (_ "Continuing with good quotes.") p)
              (newline p)))))))

    (if
     keep-going?
     (let ((prices (map (lambda (triple)
                          (commodity-tz-quote-triple->price book triple))
                        ok-syms)))
       (if (any string? prices)
           (if (gnc:ui-is-running?)
               (set!
                keep-going?
                (gnc:verify-dialog
                 (call-with-output-string
                  (lambda (p)
                    (display (_ "Unable to create prices for these items:") p)
                    (newline p)
                    (display "  " p)
                    (display (string-join (filter string? prices) "\n  ") p)
                    (newline p)
                    (display (_ "Add remaining good quotes?") p)))
                 #t))
               (gnc:warn
                (call-with-output-string
                 (lambda (p)
                   (display (_ "Unable to create prices for these items:") p)
                   (newline p)
                   (display "  " p)
                   (display (string-join (filter string? prices) "\n  ") p)
                   (newline p)
                   (display (_ "Adding remaining good quotes.") p)
                   (newline p))))))

       (if keep-going?
           (book-add-prices! book (filter
                                   (lambda (x) (not (string? x)))
                                   prices)))))))

(define (gnc:add-quotes-to-book-at-url url)
  (let* ((session (gnc:url->loaded-session url #f #f))
         (quote-ok? (and session (gnc:session-add-quotes session))))

    (if (not quote-ok?) (gnc:msg "book-add-quotes failed"))
    (and session (gnc:session-save session))
    (if (not (eq? 'no-err
                  (gw:enum-<gnc:BackendError>-val->sym
                   (gnc:session-get-error session) #f)))
        (set! quote-ok? #f))
    (if (not quote-ok?)
        (gnc:msg "session-save failed " (gnc:session-get-error session)))
    (and session (gnc:session-destroy session))
    quote-ok?))

; (define (get-1-quote exchange . items)
;   (let ((cmd (apply list 'fetch exchange items))
; 	(quoter (run-sub-process #f
; 				 gnc:*finance-quote-helper*
; 				 gnc:*finance-quote-helper*)))
;     (and quoter
; 	 (write cmd (caddr quoter))
; 	 (newline (caddr quoter))
; 	 (force-output (caddr quoter))
; 	 (let ((result (read (cadr quoter))))
; 	   (close-input-port (cadr quoter))
; 	   (close-output-port (caddr quoter))
; 	   result))))
