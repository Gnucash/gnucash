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

(use-modules (gnucash gettext))
(use-modules (gnucash utilities)) 
(use-modules (gnucash gnc-module))
(use-modules (gnucash core-utils))
(use-modules (srfi srfi-11)
             (srfi srfi-1))

(gnc:module-load "gnucash/gnome-utils" 0) ;; for gnucash-ui-is-running
(gnc:module-load "gnucash/app-utils" 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:*finance-quote-check*
  (string-append (gnc-path-get-bindir) "/gnc-fq-check"))

(define (gnc:fq-check-sources)
  (let ((program #f))

    (define (start-program)
      (set! program
        (gnc-spawn-process-async
         (list "perl" "-w" gnc:*finance-quote-check*) #t)))

    (define (get-sources)
      (when program
        (catch #t
          (lambda ()
            (let ((results (read (fdes->inport (gnc-process-get-fd program 1)))))
              (gnc:debug "gnc:fq-check-sources results: " results)
              results))
          (lambda (key . args) key))))

    (define (kill-program)
      (when program
        (gnc-detach-process program #t)
        (set! program #f)))

    (dynamic-wind start-program get-sources kill-program)))

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
  ;; i.e. (alphavantage "RHAT" "LNUX" "IBM")
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

  (let ((quoter #f))

    (define (start-quoter)
      (set! quoter
        (gnc-spawn-process-async (list "perl" "-w" gnc:*finance-quote-helper*) #t)))

    (define (get-quotes)
      (when quoter
        (map
         (lambda (request)
           (catch #t
             (lambda ()
               (gnc:debug "handling-request: " request)
               ;; we need to display the first element (the method,
               ;; so it won't be quoted) and then write the rest
               (with-output-to-port (fdes->outport (gnc-process-get-fd quoter 0))
                 (lambda ()
                   (display #\()
                   (display (car request))
                   (display " ")
                   (for-each write (cdr request))
                   (display #\))
                   (newline)
                   (force-output)))

               (let ((results (read (fdes->inport (gnc-process-get-fd quoter 1)))))
                 (gnc:debug "results: " results)
                 results))
             (lambda (key . args) key)))
         requests)))

    (define (kill-quoter)
      (when quoter
        (gnc-detach-process quoter #t)
        (set! quoter #f)))

    (dynamic-wind start-quoter get-quotes kill-quoter)))

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
    ;; (("alphavantage" (commodity-1 currency-1 tz-1)
    ;;                  (commodity-2 currency-2 tz-2) ...)
    ;;  ("fidelity_direct" (commodity-3 currency-3 tz-3)
    ;;                     (commodity-4 currency-4 tz-4) ...)
    ;;  ("currency" curr-1 curr-2 tz)
    ;;  ("currency" curr-3 curr-4 tz) ...)

    (let-values (((currency-list commodity-list)
                  (partition (lambda (a) (string=? (car a) "currency"))
                             (gnc-commodity-table-get-quotable-commodities
                              (gnc-commodity-table-get-table book)))))

      (let ((commodity-hash (make-hash-table))
            (currency-list-filtered
             (filter
              (lambda (a)
                (and (not (gnc-commodity-equiv (cadr a) (caddr a)))
                     (not (string=? (gnc-commodity-get-mnemonic (cadr a)) "XXX"))))
              currency-list)))

        ;; Now collect symbols going to the same backend.
        (for-each
         (lambda (item)
           (let ((key (car item))
                 (val (cdr item)))
             (hash-set! commodity-hash key
                        (cons val (hash-ref commodity-hash key '())))))
         commodity-list)

        ;; Now translate to just what gnc-fq-helper expects.
        (and (or (pair? currency-list-filtered) (pair? commodity-list))
             (append
              (hash-map->list cons commodity-hash)
              (map (lambda (cmd) (cons (car cmd) (list (cdr cmd))))
                   currency-list-filtered))))))

  (define (fq-call-data->fq-calls fq-call-data)
    ;; take an output element from book->commodity->fq-call-data and
    ;; return a list where the gnc_commodities have been converted to
    ;; their fq-suitable symbol strings.  i.e. turn the former into
    ;; the latter:
    ;;
    ;; ("alphavantage" (commodity-1 currency-1 tz-1)
    ;;                 (commodity-2 currency-2 tz-2) ...)
    ;;
    ;; ("alphavantage" "IBM" "AMD" ...)
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

  (define (timestr->time64 timestr time-zone)
    ;; time-zone is ignored currently
    (gnc-parse-time-to-time64 timestr "%Y-%m-%d %H:%M:%S"))

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
            (set! commodity other-curr)))

      (let lp ((price-syms '(last nav price))
               (price-types '("last" "nav" "unknown")))
        (unless (null? price-syms)
          (cond
           ((assq-ref quote-data (car price-syms)) =>
            (lambda (p)
              (set! price (gnc-scm-to-numeric p))
              (set! price-type (car price-types))))
           (else (lp (cdr price-syms) (cdr price-types))))))

      (if gnc-time
          (set! gnc-time (timestr->time64 gnc-time time-zone))
          (set! gnc-time (gnc:get-today)))

      (if (not (and commodity currency gnc-time price price-type))
          (string-append
           currency-str ":" (gnc-commodity-get-mnemonic commodity))
          (begin
            (set! saved-price (gnc-pricedb-lookup-day-t64 pricedb
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
                        (gnc-price-set-time64 saved-price gnc-time)
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
                        (gnc-price-set-time64 gnc-price gnc-time)
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
         (when price
           (gnc-pricedb-add-price pricedb price)
           (gnc-price-unref price)))
       prices)))

  (define (show-error msg)
    (gnc:gui-error msg (_ msg)))

  ;; Add the alphavantage api key to the environment. This value is taken from
  ;; the Online Quotes preference tab
  (let ((alphavantage-api-key
         (gnc-prefs-get-string "general.finance-quote" "alphavantage-api-key")))
    (gnc:debug "ALPHAVANTAGE_API_KEY=" alphavantage-api-key)
    (unless (string-null? alphavantage-api-key)
      (setenv "ALPHAVANTAGE_API_KEY" alphavantage-api-key)))

  (let* ((fq-call-data (book->commodity->fq-call-data book))
         (fq-calls (and fq-call-data
                        (append-map fq-call-data->fq-calls fq-call-data)))
         (fq-results (and fq-calls (gnc:fq-get-quotes fq-calls)))
         (commod-tz-quote-triples (and fq-results (list? (car fq-results))
                                       (fq-results->commod-tz-quote-triples
                                        fq-call-data fq-results)))
         ;; At this point commod-tz-quote-triples will either be #f or a
         ;; list of items. Each item will either be (commodity
         ;; timezone quote-data) or (#f . problem-commodity)
         (problem-syms (and commod-tz-quote-triples
                            (filter-map
                             (lambda (cq-pair)
                               (and (not (car cq-pair))
                                    (string-append
                                     (gnc-commodity-get-namespace (cdr cq-pair))
                                     ":"
                                     (gnc-commodity-get-mnemonic (cdr cq-pair)))))
                             commod-tz-quote-triples)))
         ;; strip out the "bad" ones from above.
         (ok-syms (and commod-tz-quote-triples (filter car commod-tz-quote-triples)))
         (keep-going? #t))

    (cond
     ((not fq-call-data)
      (set! keep-going? #f)
      (show-error (N_ "No commodities marked for quote retrieval.")))

     ((not fq-results)
      (set! keep-going? #f)
      (show-error (N_ "Unable to get quotes or diagnose the problem.")))

     ((memq 'missing-lib fq-results)
      (set! keep-going? #f)
      (show-error (N_ "You are missing some needed Perl libraries.
Run 'gnc-fq-update' as root to install them.")))

     ((memq 'system-error fq-results)
      (set! keep-going? #f)
      (show-error (N_ "There was a system error while retrieving the price quotes.")))

     ((not (list? (car fq-results)))
      (set! keep-going? #f)
      (show-error (N_ "There was an unknown error while retrieving the price quotes.")))

     ((not commod-tz-quote-triples)
      (set! keep-going? #f)
      (show-error (N_ "Unable to get quotes or diagnose the problem.")))

     ((pair? problem-syms)
      (cond
       ((not (gnucash-ui-is-running))
        (gnc:warn
         (with-output-to-string
           (lambda ()
             (display "Unable to retrieve quotes for these items:\n")
             (display (string-join problem-syms "\n  "))
             (newline)
             (display "Continuing with good quotes.")
             (newline)))))

       ((and ok-syms (not (null? ok-syms)))
        (set! keep-going?
          (gnc-verify-dialog
           window #t (with-output-to-string
                       (lambda ()
                         (display (_ "Unable to retrieve quotes for these items:"))
                         (display "\n  ")
                         (display (string-join problem-syms "\n  "))
                         (newline)
                         (display (_ "Continue using only the good quotes?")))))))

       (else
        (set! keep-going? #f)
        (gnc-error-dialog
         window (with-output-to-string
                  (lambda ()
                    (display (_ "Unable to retrieve quotes for these items:"))
                    (display "\n  ")
                    (display (string-join problem-syms "\n  ")))))))))

    (when keep-going?
      (let ((prices (map (lambda (triple)
                           (commodity-tz-quote-triple->price book triple))
                         ok-syms)))
        (when (any string? prices)
          (if (gnucash-ui-is-running)
              (set! keep-going?
                (gnc-verify-dialog
                 window #t
                 (with-output-to-string
                   (lambda ()
                     (display (_ "Unable to create prices for these items:"))
                     (display "\n  ")
                     (display (string-join (filter string? prices) "\n  "))
                     (newline)
                     (display (_ "Add remaining good quotes?"))))))
              (gnc:warn
               (with-output-to-string
                 (lambda ()
                   (display "Unable to create prices for these items:\n  ")
                   (display (string-join (filter string? prices) "\n  "))
                   (newline)
                   (display "Adding remaining good quotes.")
                   (newline))))))

        (when keep-going?
          (book-add-prices! book (filter (negate string?) prices)))))))

(define (gnc:price-quotes-install-sources)
  (let ((sources (gnc:fq-check-sources)))
    (cond
     ((list? sources)
      ;; Translators: ~A is the version string
      (format #t (_ "Found Finance::Quote version ~A.") (car sources))
      (newline)
      (gnc:msg "Found Finance::Quote version " (car sources))
      (gnc-quote-source-set-fq-installed (car sources) (cdr sources))))))
