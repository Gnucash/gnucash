;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(gnc:support "price-quotes.scm")

(gnc:depend "process.scm")
(gnc:depend "gnc-numeric.scm")
(gnc:depend "utilities.scm")
(gnc:depend "engine-utilities.scm")

(define gnc:*finance-quote-helper*
  (string-append (gnc:config-var-value-get gnc:*share-dir*)
                 "/finance-quote-helper"))

(define (gnc:fq-get-quotes requests)
  ;; requests should be a list where each item is of the form
  ;;
  ;; (<fq-method> sym sym ...)
  ;;
  ;; i.e. ("YAHOO" "RHAT" "LNUX" "IBM")
  ;;
  ;; This function will return #f on catastrophic failure, or a list
  ;; where, for each element in requests, the output list will contain
  ;; a quote-result element.  This element will be #f if the
  ;; corresponding method call fails, or a list otherwise.  A
  ;; quote-result list will contain the symbol representing the item
  ;; being quoted, followed by an alist detailing the quote data from
  ;; finance-quote-helper.
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

(define (gnc:book-add-quotes book)

  (define (find-quotables group)
    ;; Return a list of accounts for whose commodities we should get
    ;; quotes.
    (define (quotable-account? a)
      (let ((type (gw:enum-<gnc:AccountType>-val->sym (gnc:account-get-type a)
                                                      #f))
	    (src (gnc:account-get-price-src a)))

        (if (not type) (set! type '()))
        (if (symbol? type) (set! type (list type)))
        (if (and src
		 (or (memq 'stock type)
		     (memq 'mutual-func type)
		     (memq 'currency type)))
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
    ;; (("yahoo" (commodity-1 tz-1) (commodity-2 tz-2) ...)
    ;;  ("fidelity_direct" (commodity-3 tz-3) (commodity-4 tz-4) ...)
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
     (else #f)))

  (define (account->fq-cmd account)
    ;; Returns (cons fq-method-sym (list commod-sym-str assumed-timezone-str))
    (let* ((commodity (gnc:account-get-commodity account))
	   (src (and account (gnc:account-get-price-src account)))
           (tz (gnc:account-get-quote-tz account))
	   (fq-method-sym (and src (src->fq-method-sym src)))
           (mnemonic (and commodity (gnc:commodity-get-mnemonic commodity))))
      (and
       commodity
       fq-method-sym
       mnemonic
       (cons fq-method-sym (list commodity tz)))))
  
  (let ((cmd-list (map account->fq-cmd account-list))
	(cmd-hash (make-hash-table 31)))
    
    ;; Now collect symbols going to the same backend.
    (item-list->hash! cmd-list cmd-hash car cdr hashq-ref hashq-set! #t)
    ;; Now translate to just what finance-quote-helper expects.
    (hash-fold
     (lambda (key value prior-result)
       (cons (cons (fq-method-sym->str key) value)
	     prior-result))
     '()
     cmd-hash)))

  (define (fq-call-data->fq-call fq-call-data)
    ;; take an output element from accounts->fq-call-data and return a
    ;; list where the gnc_commodities have been converted to their
    ;; fq-suitable symbol strings.  i.e. turn the former into the
    ;; latter:
    ;;
    ;; ("yahoo" (commodity-1 tz-1) (commodity-2 tz-2) ...)
    ;; 
    ;; ("yahoo" "IBM" "AMD" ...)
    ;;

    (cons (car fq-call-data)
          (map
           (lambda (quote-item-info)
             (string-upcase
              (gnc:commodity-get-mnemonic (car quote-item-info))))
           (cdr fq-call-data))))

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
        ;; data -> (commod-1 tz-1)
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
                              (cadr call-data)
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
           (last-price (assq-ref quote-data 'last))
           (currency-str (assq-ref quote-data 'currency))
           (commodity-table (gnc:book-get-commodity-table book))
           (currency
            (and commodity-table
                 (string? currency-str)
                 (gnc:commodity-table-lookup commodity-table
                                             "ISO4217"
                                             (string-upcase currency-str)))))

      ;; FIXME: SIGFIGS is not what we want here...
      (if last-price
          (set! last-price
                (gnc:double-to-gnc-numeric last-price
                                           GNC-DENOM-AUTO
                                           (logior (GNC-DENOM-SIGFIGS 9)
                                                   GNC-RND-ROUND))))

      (if gnc-time
          (set! gnc-time (timestr->time-pair gnc-time time-zone)))

      (if (not (and commodity currency gnc-time last-price))
          (string-append
           currency-str ":" (gnc:commodity-get-mnemonic commodity))
          (let ((price (gnc:price-create)))
            (if (not price)
                (string-append
                 currency-str ":" (gnc:commodity-get-mnemonic commodity))
                (begin
                  (gnc:price-set-commodity price commodity)
                  (gnc:price-set-currency price currency)
                  (gnc:price-set-time price gnc-time)
                  (gnc:price-set-source price "Finance::Quote")
                  (gnc:price-set-type price "last")
                  (gnc:price-set-value price last-price)
                  ;;(gnc:price-print-stdout price 2)
                  price))))))

  (define (book-add-prices! book prices)
    (let ((pricedb (gnc:book-get-pricedb book)))
      (for-each
       (lambda (price)
         (gnc:pricedb-add-price pricedb price)
         (gnc:price-unref price))
       prices)))

  ;; FIXME: uses of gnc:warn in here need to be cleaned up.  Right
  ;; now, they'll result in funny formatting.

  (let* ((group (gnc:book-get-group book))
         (quotables (and group (find-quotables group)))
         (fq-call-data (and quotables (accounts->fq-call-data quotables)))
         (fq-calls (and fq-call-data
                        (map fq-call-data->fq-call fq-call-data)))
         (fq-results (and fq-calls (gnc:fq-get-quotes fq-calls)))
         (commod-tz-quote-triples
          (and fq-results
               (fq-results->commod-tz-quote-triples fq-call-data fq-results)))
         ;; At this point commod-quote-pairs will either be #f or a
         ;; list of items.  Each item will either be (commodity
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
         (keep-going? #t))

    (cond
     ((and (not commod-tz-quote-triples) (gnc:ui-is-running?))
      (gnc:error-dialog
       (string-append
        (_ "Unable to get quotes or diagnose the problem.\n")
        (_ "Sorry."))
       (set! keep-going? #f)))
     ((not commod-tz-quote-triples)
      (gnc:warn (_ "Unable to get quotes or diagnose the problem.\n")
                (_ "Sorry."))
      (set! keep-going? #f))
     ((not (null? problem-syms))
      (if (gnc:ui-is-running?)
          (set!
           keep-going?
           (gnc:verify-dialog
            (call-with-output-string
             (lambda (p)
               (display "Unable to retrieve quotes for these items:" p)
               (newline p)
               (display "  " p)
               (display (string-join problem-syms "\n  ") p)
               (newline p)
               (display "Continue using only the good quotes?" p)))
            #t))
          (gnc:warn
           (call-with-output-string
            (lambda (p)
              (display "Unable to retrieve quotes for these items:" p)
              (newline p)
              (display "  " p)
              (display (string-join problem-syms "\n  ") p)
              (newline p)
              (display "Continuing with good quotes.\n" p)))))))
    
    (if
     keep-going?
     (let ((prices (map (lambda (triple)
                          (commodity-tz-quote-triple->price book triple))
                        ;; strip out the "bad" ones from above.
                        (filter car commod-tz-quote-triples))))
       (if (any string? prices)
           (if (gnc:ui-is-running?)
               (set!
                keep-going?
                (gnc:verify-dialog
                 (call-with-output-string
                  (lambda (p)
                    (display "Unable to create prices for these items:" p)
                    (newline p)
                    (display "  " p)
                    (display (string-join (filter string? prices) "\n  ") p)
                    (newline p)
                    (display "Add remaining good quotes?" p)))
                 #t))
               (gnc:warn
                (call-with-output-string
                 (lambda (p)
                   (display "Unable to create prices for these items:" p)
                   (newline p)
                   (display "  " p)
                   (display (string-join (filter string? prices) "\n  ") p)
                   (newline p)
                   (display "Adding remaining good quotes.\n" p))))))

       (if keep-going?
           (book-add-prices! book (filter
                                   (lambda (x) (not (string? x)))
                                   prices)))))))

(define (gnc:add-quotes-to-book-at-url url)
  (let* ((book (gnc:url->loaded-book url #f #f))
         (quote-ok? (and book (gnc:book-add-quotes book))))

    (if (not quote-ok?) (gnc:msg "book-add-quotes failed"))
    (and book (gnc:book-save book))
    (if (not (eq? 'no-err
                  (gw:enum-<gnc:BackendError>-val->sym
                   (gnc:book-get-error book) #f)))
        (set! quote-ok? #f))
    (if (not quote-ok?)
        (gnc:msg "book-save failed " (gnc:book-get-error book)))
    (and book (gnc:book-destroy book))
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
