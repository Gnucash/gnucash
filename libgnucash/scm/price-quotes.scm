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
;;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash price-quotes))

(export gnc:book-add-quotes) ;; called from gnome/dialog-price-edit-db.c
(export gnc:price-quotes-install-sources)

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash core-utils))
(use-modules (srfi srfi-1))

(gnc:module-load "gnucash/gnome-utils" 0) ;; for gnucash-ui-is-running
(gnc:module-load "gnucash/app-utils" 0)

(define (item-list->hash! lst hash
			  getkey getval
			  hashref hashset
			  list-duplicates?)
  ;; Takes a list of the form (item item item item) and returns a hash
  ;; formed by traversing the list, and getting the key and val from
  ;; each item using the supplied get-key and get-val functions, and
  ;; building a hash table from the result using the given hashref and
  ;; hashset functions.  list-duplicates? determines whether or not in
  ;; the resulting hash, the value for a given key is a list of all
  ;; the values associated with that key in the input or just the
  ;; first one encountered.

  (define (handle-item item)
    (let* ((key (getkey item))
	   (val (getval item))
	   (existing-val (hashref hash key)))

      (if (not list-duplicates?)
	  ;; ignore if not first value.
	  (if (not existing-val) (hashset hash key val))
	  ;; else result is list.
	  (if existing-val
	      (hashset hash key (cons val existing-val))
	      (hashset hash key (list val))))))

  (for-each handle-item lst)
  hash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:*finance-quote-check*
  (string-append (gnc-path-get-bindir) "/gnc-fq-check"))

(define (gnc:fq-check-sources)
  (let ((program '())
        (from-child #f))

    (define (start-program)
      (if (not (string-null? gnc:*finance-quote-check*))
          (set! program (gnc-spawn-process-async
                         (list "perl" "-w" gnc:*finance-quote-check*) #t))))

    (define (get-sources)
      (if (not (null? program))
          (let ((results #f))
            (set! from-child (fdes->inport (gnc-process-get-fd program 1)))
            (catch
             #t
             (lambda ()
               (set! results (read from-child))
               (gnc:debug "results: " results)
               results)
             (lambda (key . args)
               key)))))

    (define (kill-program)
      (if (not (null? program))
          (gnc-detach-process program #t)))

    (dynamic-wind
        start-program
        get-sources
        kill-program)))

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
  (string-append (gnc-path-get-bindir) "/gnc-fq-helper"))

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
  ;; detailing the quote data from gnc-fq-helper.
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
  ;; was unparsable.  See the gnc-fq-helper for more details
  ;; about it's output.

  (let ((quoter '())
        (to-child #f)
        (from-child #f))

    (define (start-quoter)
      (if (not (string-null? gnc:*finance-quote-helper*))
          (set! quoter (gnc-spawn-process-async
                        (list "perl" "-w" gnc:*finance-quote-helper*) #t))))

    (define (get-quotes)
      (if (not (null? quoter))
          (let ((results #f))
            (set! to-child (fdes->outport (gnc-process-get-fd quoter 0)))
            (set! from-child (fdes->inport (gnc-process-get-fd quoter 1)))
            (map
             (lambda (request)
               (catch
                #t
                (lambda ()
                  (gnc:debug "handling-request: " request)
                  ;; we need to display the first element (the method, so it
                  ;; won't be quoted) and then write the rest
                  (display #\( to-child)
                  (display (car request) to-child)
                  (display " " to-child)
                  (for-each (lambda (x) (write x to-child)) (cdr request))
                  (display #\) to-child)
                  (newline to-child)
                  (force-output to-child)
                  (set! results (read from-child))
                  (gnc:debug "results: " results)
                  results)
                (lambda (key . args)
                  key)))
             requests))))

    (define (kill-quoter)
      (if (not (null? quoter))
          (gnc-detach-process quoter #t)))

    (dynamic-wind
        start-quoter
        get-quotes
        kill-quoter)))

(define (gnc:book-add-quotes window book)

  (define (book->commodity->fq-call-data book)
    ;; Call helper that walks all of the defined commodities to see if
    ;; any are marked for quote retrieval.  This function returns a
    ;; list of info needed for the relevant Finance::Quote calls, and
    ;; a list of the corresponding commodities.  Also perform a bit of
    ;; optimization, merging calls for symbols to the same
    ;; Finance::Quote method.
    ;;
    ;; Returns a list of the info needed for a set of calls to
    ;; gnc-fq-helper.  Each item will of the list will be of the
    ;; form:
    ;;
    ;; (("yahoo" (commodity-1 currency-1 tz-1)
    ;;           (commodity-2 currency-2 tz-2) ...)
    ;;  ("fidelity_direct" (commodity-3 currency-3 tz-3)
    ;;                     (commodity-4 currency-4 tz-4) ...)
    ;;  ...)

    (let* ((ct (gnc-commodity-table-get-table book))
	   (big-list
	    (gnc-commodity-table-get-quotable-commodities
	     ct))
	   (commodity-list #f)
	   (currency-list (filter
			   (lambda (a)
                             (and
                              (not (gnc-commodity-equiv (cadr a) (caddr a)))
                              (not (string=? "XXX" (gnc-commodity-get-mnemonic (cadr a))))
                              ))
			   (call-with-values
                               (lambda () (partition!
                                           (lambda (cmd)
                                             (not (string=? (car cmd) "currency")))
                                           big-list))
                             (lambda (a b) (set! commodity-list a) b))))
	   (quote-hash (make-hash-table 31)))

      (if (and (null? commodity-list) (null? currency-list))
	  #f
	  (begin

	    ;; Now collect symbols going to the same backend.
	    (item-list->hash! commodity-list quote-hash car cdr hash-ref hash-set! #t)

	    ;; Now translate to just what gnc-fq-helper expects.
	    (append
	     (hash-fold
	      (lambda (key value prior-result)
		(cons (cons key value)
		      prior-result))
	      '()
	      quote-hash)
	     (map (lambda (cmd) (cons (car cmd) (list (cdr cmd))))
		  currency-list))))))

  (define (fq-call-data->fq-calls fq-call-data)
    ;; take an output element from book->commodity->fq-call-data and
    ;; return a list where the gnc_commodities have been converted to
    ;; their fq-suitable symbol strings.  i.e. turn the former into
    ;; the latter:
    ;;
    ;; ("yahoo" (commodity-1 currency-1 tz-1)
    ;;          (commodity-2 currency-2 tz-2) ...)
    ;;
    ;; ("yahoo" "IBM" "AMD" ...)
    ;;

    (if (equal? (car fq-call-data) "currency")
        (map (lambda (quote-item-info)
               (list (car fq-call-data)
                     (gnc-commodity-get-mnemonic (car quote-item-info))
                     (gnc-commodity-get-mnemonic (cadr quote-item-info))))
             (cdr fq-call-data))
        (list
         (cons (car fq-call-data)
               (map
                (lambda (quote-item-info)
                  (gnc-commodity-get-mnemonic (car quote-item-info)))
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
            ;; OK, data is good (as far as we can tell).
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
	    (for-each
	     (lambda (call-data-item call-result-item)
	       (if (and (list? call-result-item) (list? (car call-result-item)))
		   (for-each
		    (lambda (result-subitem)
		      (gnc:debug "call-data-item: " call-data-item)
		      (gnc:debug "result-subitem: " result-subitem)
		      (process-a-quote call-data-item result-subitem))
		    call-result-item)
		   (process-a-quote call-data-item call-result-item)))
	     (cdr call-data) call-result)

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
    ;; time-zone is ignored currently
    (cons (gnc-parse-time-to-time64 timestr "%Y-%m-%d %H:%M:%S")
          0))

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
           (commodity-table (gnc-commodity-table-get-table book))
           (currency
            (and commodity-table
                 (string? currency-str)
                 (gnc-commodity-table-lookup commodity-table
                                             "ISO4217"
                                             (string-upcase currency-str))))
           (pricedb (gnc-pricedb-get-db book))
           (saved-price #f)
           (commodity-str (gnc-commodity-get-printname commodity))
           )
      (if (equal? (gnc-commodity-get-printname currency) commodity-str)
          (let* ((symbol (assq-ref quote-data 'symbol))
                 (other-curr
                  (and commodity-table
                       (string? symbol)
                       (gnc-commodity-table-lookup commodity-table "ISO4217"
                                                   (string-upcase symbol)))))
            (set! commodity other-curr))
        )
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
                (double-to-gnc-numeric price
                                           GNC-DENOM-AUTO
                                           (logior (GNC-DENOM-SIGFIGS 9)
                                                   GNC-RND-ROUND))))

      (if gnc-time
          (set! gnc-time (timestr->time-pair gnc-time time-zone))
          (set! gnc-time (gnc:get-today)))

      (if (not (and commodity currency gnc-time price price-type))
          (string-append
           currency-str ":" (gnc-commodity-get-mnemonic commodity))
          (begin
            (set! saved-price (gnc-pricedb-lookup-day pricedb
                                                      commodity currency
                                                      gnc-time))
            (if (not (null? saved-price))
                (begin
                  (if (gnc-commodity-equiv (gnc-price-get-currency saved-price)
                                           commodity)
                      (set! price (gnc-numeric-invert price)))
                  (if (>= (gnc-price-get-source saved-price) PRICE-SOURCE-FQ)
                      (begin
                        (gnc-price-begin-edit saved-price)
                        (gnc-price-set-time saved-price gnc-time)
                        (gnc-price-set-source saved-price PRICE-SOURCE-FQ)
                        (gnc-price-set-typestr saved-price price-type)
                        (gnc-price-set-value saved-price price)
                        (gnc-price-commit-edit saved-price)
                        #f)
                      #f))
                (let ((gnc-price (gnc-price-create book)))
                  (if (not gnc-price)
                      (string-append
                       currency-str ":" (gnc-commodity-get-mnemonic commodity))
                      (begin
                        (gnc-price-begin-edit gnc-price)
                        (gnc-price-set-commodity gnc-price commodity)
                        (gnc-price-set-currency gnc-price currency)
                        (gnc-price-set-time gnc-price gnc-time)
                        (gnc-price-set-source gnc-price PRICE-SOURCE-FQ)
                        (gnc-price-set-typestr gnc-price price-type)
                        (gnc-price-set-value gnc-price price)
                        (gnc-price-commit-edit gnc-price)
                        gnc-price))))
            ))
      ))

  (define (book-add-prices! book prices)
    (let ((pricedb (gnc-pricedb-get-db book)))
      (for-each
       (lambda (price)
         (if price
             (begin
               (gnc-pricedb-add-price pricedb price)
               (gnc-price-unref price)
               #f)))
       prices)))

  ;; FIXME: uses of gnc:warn in here need to be cleaned up.  Right
  ;; now, they'll result in funny formatting.

  (let* ((fq-call-data (book->commodity->fq-call-data book))
         (fq-calls (and fq-call-data
                        (apply append
                               (map fq-call-data->fq-calls fq-call-data))))
         (fq-results (and fq-calls (gnc:fq-get-quotes fq-calls)))
         (commod-tz-quote-triples
          (and fq-results (list? (car fq-results))
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
                                  (gnc-commodity-get-namespace (cdr cq-pair))
                                  ":"
                                  (gnc-commodity-get-mnemonic (cdr cq-pair)))))
                           commod-tz-quote-triples)))
         ;; strip out the "bad" ones from above.
         (ok-syms
          (and commod-tz-quote-triples
               (filter car commod-tz-quote-triples)))
         (keep-going? #t))

    (cond
     ((eq? fq-call-data #f)
      (set! keep-going? #f)
      (if (gnucash-ui-is-running)
          (gnc-error-dialog window (_ "No commodities marked for quote retrieval."))
	  (gnc:warn (_ "No commodities marked for quote retrieval."))))
     ((eq? fq-results #f)
      (set! keep-going? #f)
      (if (gnucash-ui-is-running)
          (gnc-error-dialog window (_ "Unable to get quotes or diagnose the problem."))
	  (gnc:warn (_ "Unable to get quotes or diagnose the problem."))))
     ((member 'missing-lib fq-results)
      (set! keep-going? #f)
      (if (gnucash-ui-is-running)
          (gnc-error-dialog window
           (_ "You are missing some needed Perl libraries.
Run 'gnc-fq-update' as root to install them."))
          (gnc:warn (_ "You are missing some needed Perl libraries.
Run 'gnc-fq-update' as root to install them.") "\n")))
     ((member 'system-error fq-results)
      (set! keep-going? #f)
      (if (gnucash-ui-is-running)
          (gnc-error-dialog window
           (_ "There was a system error while retrieving the price quotes."))
          (gnc:warn (_ "There was a system error while retrieving the price quotes.") "\n")))
     ((not (list? (car fq-results)))
      (set! keep-going? #f)
      (if (gnucash-ui-is-running)
          (gnc-error-dialog window
           (_ "There was an unknown error while retrieving the price quotes."))
          (gnc:warn (_ "There was an unknown error while retrieving the price quotes.") "\n")))
     ((and (not commod-tz-quote-triples) (gnucash-ui-is-running))
      (gnc-error-dialog window
       (_ "Unable to get quotes or diagnose the problem."))
       (set! keep-going? #f))
     ((not commod-tz-quote-triples)
      (gnc:warn (_ "Unable to get quotes or diagnose the problem."))
      (set! keep-going? #f))
     ((not (null? problem-syms))
      (if (gnucash-ui-is-running)
          (if (and ok-syms (not (null? ok-syms)))
              (set!
               keep-going?
               (gnc-verify-dialog window #t
                (call-with-output-string
                 (lambda (p)
                   (display (_ "Unable to retrieve quotes for these items:") p)
                   (newline p)
                   (display "  " p)
                   (display (string-join problem-syms "\n  ") p)
                   (newline p)
                   (display (_ "Continue using only the good quotes?") p)))))
              (begin
                (gnc-error-dialog window
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
           (if (gnucash-ui-is-running)
               (set!
                keep-going?
                (gnc-verify-dialog window #t
                 (call-with-output-string
                  (lambda (p)
                    (display (_ "Unable to create prices for these items:") p)
                    (newline p)
                    (display "  " p)
                    (display (string-join (filter string? prices) "\n  ") p)
                    (newline p)
                    (display (_ "Add remaining good quotes?") p)))))
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

(define (gnc:price-quotes-install-sources)
  (let ((sources (gnc:fq-check-sources)))
    (if (list? sources)
	(begin
      (format #t "Found Finance::Quote version ~A" (car sources))
      (newline)
	  (gnc:msg "Found Finance::Quote version " (car sources))
	  (gnc-quote-source-set-fq-installed (cdr sources))))))
