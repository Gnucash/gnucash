;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commodity-utilities.scm: Functions for handling different commodities.
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to get splits with interesting data from accounts.

;; helper function. queries book for all splits in accounts before
;; end-date (end-date can be #f)
(define (get-all-splits accounts end-date)
  (let ((query (qof-query-create-for-splits)))
    (qof-query-set-book query (gnc-get-current-book))
    (gnc:query-set-match-non-voids-only! query (gnc-get-current-book))
    (xaccQueryAddAccountMatch query accounts QOF-GUID-MATCH-ANY QOF-QUERY-AND)
    (xaccQueryAddDateMatchTT query #f 0 (and end-date #t) (or end-date 0) QOF-QUERY-AND)
    (let ((splits (qof-query-run query)))
      (qof-query-destroy query)
      splits)))

;; Returns a list of all splits in the 'currency-accounts' up to
;; 'end-date' which have two different commodities involved, one of
;; which is equivalent to 'commodity' (the latter constraint only if
;; 'commodity' != #f ).
(define (gnc:get-match-commodity-splits
         currency-accounts end-date commodity)
  (filter
   (lambda (s)
     (let ((txn-comm (xaccTransGetCurrency (xaccSplitGetParent s)))
           (acc-comm (xaccAccountGetCommodity (xaccSplitGetAccount s))))
       (and (not (gnc-commodity-equiv txn-comm acc-comm))
            (or (not commodity)
                (gnc-commodity-equiv commodity txn-comm)
                (gnc-commodity-equiv commodity acc-comm)))))
   (get-all-splits currency-accounts end-date)))

;; Returns a sorted list of all splits in the 'currency-accounts' up
;; to 'end-date' which have the 'commodity' and one other commodity
;; involved. The splits are sorted by date.
(define (gnc:get-match-commodity-splits-sorted currency-accounts
                                               end-date
                                               commodity)
  (sort (gnc:get-match-commodity-splits currency-accounts
                                        end-date commodity)
        (lambda (a b)
          (<
           (xaccTransGetDate (xaccSplitGetParent a))
           (xaccTransGetDate (xaccSplitGetParent b))))))


;; Returns a list of all splits in the currency-accounts up to
;; end-date which have two *different* commodities involved.
(define (gnc:get-all-commodity-splits currency-accounts end-date)
  (gnc:get-match-commodity-splits currency-accounts end-date #f))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to create some list of prices from data in transactions.


;; Helper for warnings below.
(define (gnc-commodity-numeric->string commodity numeric)
  (issue-deprecation-warning "gnc-commodity-numeric->string deprecated. \
construct with gnc:make-gnc-monetary and gnc:monetary->string instead.")
  (gnc:monetary->string
   (gnc:make-gnc-monetary commodity numeric)))

;; Helper for exchange below
(define (gnc:exchange-by-euro-numeric
         foreign-commodity foreign-numeric domestic date)
  (gnc:exchange-by-euro
   (gnc:make-gnc-monetary foreign-commodity foreign-numeric)
   domestic date))

;; Create a list of all prices of 'price-commodity' measured in the currency
;; 'report-currency'. The prices are taken from all splits in
;; 'currency-accounts' up until the date 'end-date'. Returns a list of
;; lists. Each listelement looks like the list (time price), where 'time' is the
;; time64 when the <gnc:numeric*> 'price' was valid.  The results are distorted
;; by the existence of capital gains splits because the amount of the gain is
;; added to the total value with no adjustment to the total amount. For example
;; if on day1 one buys 200 shares of XYZ at 20 and on day2 sells 100 at 25
;; booking a capital gain of 500, the resulting foreignlist will be:
;; ((day1 4000 200)
;; (day2 500 0)
;; (day2 2500 100))
;; returning the following price list:
;; ((day1 20)
;;  (day2 22.50)
;;  (day2 23.33))
;; The intended second price is 6500/300, or 21.67.

(define (gnc:get-commodity-totalavg-prices
         currency-accounts end-date price-commodity report-currency)
  (gnc:get-commodity-totalavg-prices-internal
   currency-accounts end-date price-commodity report-currency
   (gnc:get-match-commodity-splits-sorted
    currency-accounts end-date price-commodity)))

(define (gnc:get-commodity-totalavg-prices-internal
         currency-accounts end-date price-commodity report-currency commodity-splits)
  (let loop ((tot-foreign 0)
             (tot-domestic 0)
             (commodity-splits commodity-splits)
             (result '()))
    (if (null? commodity-splits)
        (reverse! result)
        (let* ((a (car commodity-splits))
               (share-amt (abs (xaccSplitGetAmount a)))
               (value-amt (abs (xaccSplitGetValue a)))
               (txn-comm (xaccTransGetCurrency (xaccSplitGetParent a)))
               (acc-comm (xaccAccountGetCommodity (xaccSplitGetAccount a)))
               (txn-date (xaccTransGetDate (xaccSplitGetParent a))))
          (cond
           ((or (zero? share-amt) (zero? value-amt))
            (loop tot-foreign
                  tot-domestic
                  (cdr commodity-splits)
                  result))

           ((gnc-commodity-equiv acc-comm report-currency)
            (let ((new-foreign (+ tot-foreign value-amt))
                  (new-domestic (+ tot-domestic share-amt)))
              (loop new-foreign
                    new-domestic
                    (cdr commodity-splits)
                    (cons (list txn-date (/ new-domestic new-foreign)) result))))

           ((gnc-commodity-equiv txn-comm report-currency)
            (let ((new-foreign (+ tot-foreign share-amt))
                  (new-domestic (+ tot-domestic value-amt)))
              (loop new-foreign
                    new-domestic
                    (cdr commodity-splits)
                    (cons (list txn-date (/ new-domestic new-foreign)) result))))

           ((gnc:exchange-by-euro-numeric txn-comm value-amt report-currency txn-date)
            => (lambda (amt)
                 (let ((new-foreign (+ tot-foreign share-amt))
                       (new-domestic (+ tot-domestic (gnc:gnc-monetary-amount amt))))
                   (loop new-foreign
                         new-domestic
                         (cdr commodity-splits)
                         (cons (list txn-date (/ new-domestic new-foreign)) result)))))

           (else
            (warn "gnc:get-commodity-totalavg-prices: "
                  "Sorry, currency exchange not yet implemented:"
                  (gnc:monetary->string
                   (gnc:make-gnc-monetary txn-comm value-amt))
                  " (buying "
                  (gnc:monetary->string
                   (gnc:make-gnc-monetary price-commodity share-amt))
                  ") =? "
                  (gnc:monetary->string
                   (gnc:make-gnc-monetary report-currency 0)))
            (loop tot-foreign
                  tot-domestic
                  (cdr commodity-splits)
                  result)))))))


;; Create a list of prices for all commodities in 'commodity-list',
;; i.e. the same thing as in get-commodity-totalavg-prices but
;; extended to a commodity-list. Returns an alist. Each pair consists
;; of the foreign-currency and the appropriate list from
;; gnc:get-commodity-totalavg-prices, see there.
(define (gnc:get-commoditylist-totalavg-prices
         commodity-list report-currency end-date
         start-percent delta-percent)
  (define (interesting-split? s)
    (not (gnc-commodity-equiv
          (xaccTransGetCurrency (xaccSplitGetParent s))
          (xaccAccountGetCommodity (xaccSplitGetAccount s)))))
  (define (date<? a b)
    (< (xaccTransGetDate (xaccSplitGetParent a))
       (xaccTransGetDate (xaccSplitGetParent b))))
  (let* ((currency-accounts
          (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
         (all-splits (get-all-splits currency-accounts end-date))
         (interesting-splits (sort (filter interesting-split? all-splits) date<?))
         (work-to-do (length commodity-list)))
    (map
     (lambda (c work-done)
       (define (split-has-commodity? s)
         (or (gnc-commodity-equiv c (xaccTransGetCurrency (xaccSplitGetParent s)))
             (gnc-commodity-equiv c (xaccAccountGetCommodity (xaccSplitGetAccount s)))))
       (if start-percent
           (gnc:report-percent-done
            (+ start-percent (* delta-percent (/ work-done work-to-do)))))
       (cons c
             (gnc:get-commodity-totalavg-prices-internal
              currency-accounts end-date c report-currency
              (filter split-has-commodity? interesting-splits))))
     commodity-list
     (iota work-to-do))))

;; Get the instantaneous prices for the 'price-commodity', measured in
;; amounts of the 'report-currency'. The prices are taken from all
;; splits in 'currency-accounts' up until the date
;; 'end-date'. Returns a list of lists. Each listelement looks like
;; the list (time price), where 'time' is the time64 when the
;; <gnc:numeric*> 'price' was valid.
(define (gnc:get-commodity-inst-prices
         currency-accounts end-date price-commodity report-currency)
  ;; go through all splits; convert all splits into a price.
  (let loop ((result '())
             (commodity-splits (gnc:get-match-commodity-splits-sorted
                                currency-accounts end-date price-commodity)))
    (if (null? commodity-splits)
        (reverse! result)
        (let* ((a (car commodity-splits))
               (txn-comm (xaccTransGetCurrency (xaccSplitGetParent a)))
               (acc-comm (xaccAccountGetCommodity (xaccSplitGetAccount a)))
               (share-amt (abs (xaccSplitGetAmount a)))
               (value-amt (abs (xaccSplitGetValue a)))
               (txn-date (xaccTransGetDate (xaccSplitGetParent a))))
          (cond
           ((or (zero? share-amt) (zero? value-amt))
            (loop result
                  (cdr commodity-splits)))

           ((gnc-commodity-equiv acc-comm report-currency)
            (loop (cons (list txn-date (/ share-amt value-amt)) result)
                  (cdr commodity-splits)))

           ((gnc-commodity-equiv txn-comm report-currency)
            (loop (cons (list txn-date (/ value-amt share-amt)) result)
                  (cdr commodity-splits)))

           ((gnc:exchange-by-euro-numeric txn-comm value-amt report-currency txn-date)
            => (lambda (amt)
                 (loop (cons (list txn-date (/ (gnc:gnc-monetary-amount amt) share-amt))
                             result)
                       (cdr commodity-splits))))

           (else
            (warn "get-commodity-inst-prices: "
                  "Sorry, currency exchange not yet implemented:"
                  (gnc:monetary->string
                   (gnc:make-gnc-monetary txn-comm value-amt))
                  " (buying "
                  (gnc:monetary->string
                   (gnc:make-gnc-monetary price-commodity share-amt))
                  ") =? "
                  (gnc:monetary->string
                   (gnc:make-gnc-monetary report-currency 0)))
            (loop result
                  (cdr commodity-splits))))))))

;; Get the instantaneous prices for all commodities in
;; 'commodity-list', i.e. the same thing as get-commodity-inst-prices
;; but extended to a commodity-list. Returns an alist. Each pair
;; consists of the foreign-currency and the appropriate list from
;; gnc:get-commodity-inst-prices, see there.
(define (gnc:get-commoditylist-inst-prices
         commodity-list report-currency end-date
         start-percent delta-percent)
  (issue-deprecation-warning
   "gnc:get-commoditylist-inst-prices is deprecated.")
  (let ((currency-accounts
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
        (work-to-do (length commodity-list)))
    (map
     (lambda (c work-done)
       (if start-percent
           (gnc:report-percent-done
            (+ start-percent (* delta-percent (/ work-done work-to-do)))))
       (cons c
             (gnc:get-commodity-inst-prices
              currency-accounts end-date c report-currency)))
     commodity-list
     (iota work-to-do))))


;; Find the price in 'pricelist' that's nearest to 'date'. The
;; pricelist comes from
;; e.g. gnc:get-commodity-totalavg-prices. Returns a <gnc-numeric> or,
;; if pricelist was empty, #f.
(define (gnc:pricelist-price-find-nearest pricelist date)
  (let lp ((pricelist pricelist))
    (match pricelist
      (() #f)
      (((date price)) price)
      (((date1 price1) (date2 price2) . rest)
       (cond
        ((< date2 date) (lp (cdr pricelist)))
        ((< (- date date1) (- date2 date)) price1)
        (else price2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to get one price at a given time (i.e. not time-variant).

;; Several of the following functions use two types of alist, a return-list or a
;; sumlist.
;;
;; A return-list is an alist with a GncCommodity pointer as the keys and a pair
;; of gnc-value-collectors, one for amount and the other for value, as the
;; value.
;;
;; A sumlist is an alist having a GncCommodity pointer as the key and a
;; return-list as the values. The reason for the outer alist is that there might
;; be commodity transactions which do not involve the report-commodity, but
;; which can still be calculated after *all* transactions are processed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go through all toplevel non-'report-commodity' balances in 'sumlist' and add
;; them to 'report-commodity', if possible. This function takes a sumlist
;; (described in gnc:get-exchange-totals) and returns a report-list, This
;; resulting alist can immediately be plugged into gnc:make-exchange-alist.

(define (gnc:resolve-unknown-comm sumlist report-commodity)
  ;; reportlist contains all known transactions with the
  ;; report-commodity, and now the transactions with unknown
  ;; currencies should be added to that list (with an appropriate
  ;; exchange rate).

  ;; Helper function to calculate (a*b)/c and create the new pair of
  ;; numeric-collectors, where [abc] are numeric-collectors. See the
  ;; real variable names below.
  (define (make-newrate unknown-coll un->known-coll known-pair)
    (let ((a (gnc:make-value-collector))
          (b (gnc:make-value-collector))
          (c ((caadr known-pair) 'total #f)))
      (a 'add (unknown-coll 'total #f))
      (unless (zero? c)
        (b 'add (/ (* (un->known-coll 'total #f)
                      ((cdadr known-pair) 'total #f))
                   c)))
      (cons a b)))

  ;; Go through sumlist.
  (let loop ((sumlist sumlist)
             (reportlist (cadr (assoc report-commodity sumlist))))
    (cond
     ((null? sumlist) reportlist)

     ((gnc-commodity-equiv (caar sumlist) report-commodity)
      (loop (cdr sumlist) reportlist))

     (else
      (let innerloop ((pairs (cadr (car sumlist)))
                      (reportlist reportlist))
        (cond
         ((null? pairs)
          (loop (cdr sumlist) reportlist))

         ;; Check whether by any accident the report-commodity appears
         ;; here.  Huh, the report-currency showed up on the wrong
         ;; side -- we will just add it to the reportlist on the right
         ;; side.
         ((gnc-commodity-equiv (caar pairs) report-commodity)
          (innerloop (cdr pairs)
                     (cons (list (caar sumlist)
                                 (cons (cdadr (car pairs)) (caadr (car pairs))))
                           reportlist)))

         (else
          ;; pair-{a,b}: Try to find either the currency of (car
          ;; sumlist) or of pair in reportlist.
          (let* ((pair (car pairs))
                 (pair-a (or (assoc (car (car sumlist)) reportlist)
                             ;; Find the (car sumlist)'s currency in reportlist
                             ;; Or try whether that's an Euro currency.
                             (let ((euro-monetary
                                    (gnc:exchange-by-euro
                                     (gnc:make-gnc-monetary
                                      (car (car sumlist))
                                      ((cdadr pair) 'total #f))
                                     report-commodity #f)))
                               ;; If this is an Euro currency, create the
                               ;; pair of appropriately exchanged amounts.
                               (and euro-monetary
                                    (let ((a (gnc:make-value-collector)))
                                      (a 'add (gnc:gnc-monetary-amount euro-monetary))
                                      (list report-commodity
                                            (cons (cdadr pair) a)))))))
                 ;; Find the pair's currency in reportlist. FIXME:
                 ;; Also try the Euro here.
                 (pair-b (assoc (car pair) reportlist)))

            (cond
             ((and (not pair-a) (not pair-b))
              ;; If neither the currency of (car sumlist) nor of pair
              ;; was found in reportlist then we can't resolve the
              ;; exchange rate to this currency.
              (warn "gnc:resolve-unknown-comm:"
                    "can't calculate rate for "
                    (gnc:monetary->string
                     (gnc:make-gnc-monetary (car pair) ((caadr pair) 'total #f)))
                    " = "
                    (gnc:monetary->string
                     (gnc:make-gnc-monetary (caar sumlist) ((cdadr pair) 'total #f)))
                    " to "
                    (gnc:monetary->string (gnc:make-gnc-monetary report-commodity 0)))
              (innerloop (cdr pairs) reportlist))

             ((and pair-a pair-b)
              ;; If both currencies are found then something went
              ;; wrong inside gnc:get-exchange-totals. FIXME: Find a
              ;; better thing to do in this case.
              (warn "gnc:resolve-unknown-comm:"
                    "Oops - exchange rate ambiguity error: "
                    (gnc:monetary->string
                     (gnc:make-gnc-monetary (car pair) ((caadr pair) 'total #f)))
                    " = "
                    (gnc:monetary->string
                     (gnc:make-gnc-monetary (caar sumlist) ((cdadr pair) 'total #f))))
              (innerloop (cdr pairs) reportlist))

             ;; Usual case: one of pair-{a,b} was found in reportlist,
             ;; i.e. this transaction can be resolved to
             ;; report-commodity.
             (pair-a
              (innerloop (cdr pairs)
                         (cons (list (car pair)
                                     (make-newrate (caadr pair) (cdadr pair) pair-a))
                               reportlist)))
             (pair-b
              (innerloop (cdr pairs)
                         (cons (list (car (car sumlist))
                                     (make-newrate (cdadr pair) (caadr pair) pair-b))
                               reportlist))))))))))))

;; Some thoughts: In the (and (not pair-a) (not pair-b)) case above we
;; will have unresolvable transaction exchange rates. But there might
;; be cases where we will be able to resolve this, but only after one
;; or more runs of gnc:resolve-unknown-comm. Maybe we could transform
;; this functions to use some kind of recursiveness.


;; Sum the absolute value of the amounts and values in the report commodity of
;; all exchanges, ignoring gain/loss splits (i.e. those with a 0 amount). For
;; example, if one bought 1000 GBP for 1210 EUR and then 1190 EUR for 1000 GBP,
;; if the report currency is EUR the result will be an entry for GBP having an
;; amount total of 2000 GBP and a value of 2400 EUR. Returns a report-list.

(define (gnc:get-exchange-totals report-commodity end-date)
  (let ((curr-accounts (gnc-account-get-descendants-sorted
                        (gnc-get-current-root-account))))

    ;; Go through all splits and add up all value-amounts
    ;; and share-amounts
    (let loop ((comm-splits (gnc:get-all-commodity-splits curr-accounts end-date))
               (sumlist (list (list report-commodity '()))))
      (cond
       ((null? comm-splits)
        (gnc:resolve-unknown-comm sumlist report-commodity))

       (else
        (let* ((a (car comm-splits))
               (txn-comm (xaccTransGetCurrency (xaccSplitGetParent a)))
               (acc-comm (xaccAccountGetCommodity (xaccSplitGetAccount a)))
               ;; Always use the absolute value here.
               (share-amt (abs (xaccSplitGetAmount a)))
               (value-amt (abs (xaccSplitGetValue a))))
          (cond
           ;; Without shares this is not a buy or sell; ignore it.
           ((zero? share-amt)
            (loop (cdr comm-splits)
                  sumlist))

           ((assoc txn-comm sumlist)
            => (lambda (comm-list)
                 (cond
                  ((assoc acc-comm (cadr comm-list)) =>
                   ;; second commodity already exists in comm-list:
                   (lambda (pair)
                     ((caadr pair) 'add share-amt)
                     ((cdadr pair) 'add value-amt)
                     (loop (cdr comm-splits)
                           sumlist)))
                  (else
                   ;; if not, create a new entry in comm-list.
                   (let ((pair (list acc-comm (cons (gnc:make-value-collector)
                                                    (gnc:make-value-collector)))))
                     ((caadr pair) 'add share-amt)
                     ((cdadr pair) 'add value-amt)
                     (loop (cdr comm-splits)
                           (cons (list txn-comm (cons pair (cadr comm-list)))
                                 (alist-delete txn-comm sumlist))))))))

           ((assoc acc-comm sumlist)
            => (lambda (comm-list)
                 (cond
                  ((assoc txn-comm (cadr comm-list)) =>
                   ;; second commodity already exists in comm-list:
                   (lambda (pair)
                     ((caadr pair) 'add value-amt)
                     ((cdadr pair) 'add share-amt)
                     (loop (cdr comm-splits)
                           sumlist)))
                  (else
                   ;; if not, create a new entry in comm-list.
                   (let ((pair (list txn-comm (cons (gnc:make-value-collector)
                                                    (gnc:make-value-collector)))))
                     ((caadr pair) 'add value-amt)
                     ((cdadr pair) 'add share-amt)
                     (loop (cdr comm-splits)
                           (cons (list acc-comm (cons pair (cadr comm-list)))
                                 (alist-delete acc-comm sumlist))))))))

           ;; entry doesn't exist in sumlist. create sub-alist from scratch
           (else
            (let ((pair (list txn-comm
                              (cons (gnc:make-value-collector)
                                    (gnc:make-value-collector)))))
              ((caadr pair) 'add value-amt)
              ((cdadr pair) 'add share-amt)
              (loop (cdr comm-splits)
                    (cons (list acc-comm (list pair))
                          sumlist)))))))))))

;; Sum the net amounts and values in the report commodity, including booked
;; gains and losses, of each commodity across all accounts. Returns a
;; report-list.
(define (gnc:get-exchange-cost-totals report-commodity end-date)
  (let ((curr-accounts (gnc-account-get-descendants-sorted
                        (gnc-get-current-root-account))))

    (let loop ((comm-splits (gnc:get-all-commodity-splits curr-accounts end-date))
               (sumlist (list (list report-commodity '()))))
      (cond
       ((null? comm-splits)
        (gnc:resolve-unknown-comm sumlist report-commodity))

       ;; However skip splits in trading accounts as these counterbalance
       ;; the actual value and share amounts back to zero
       ((eqv? (xaccAccountGetType (xaccSplitGetAccount (car comm-splits)))
              ACCT-TYPE-TRADING)
        (loop (cdr comm-splits)
              sumlist))

       ;; Go through all splits and add up all value-amounts
       ;; and share-amounts
       (else
        (let* ((a (car comm-splits))
               (txn-comm (xaccTransGetCurrency (xaccSplitGetParent a)))
               (acc-comm (xaccAccountGetCommodity (xaccSplitGetAccount a)))
               (share-amt (xaccSplitGetAmount a))
               (value-amt (xaccSplitGetValue a)))

          (cond
           ((assoc txn-comm sumlist)
            => (lambda (comm-list)
                 (cond
                  ;; other commodity already exists in comm-list?
                  ((assoc acc-comm (cadr comm-list))
                   => (lambda (pair)
                        ((caadr pair) 'add share-amt)
                        ((cdadr pair) 'add value-amt)
                        (loop (cdr comm-splits)
                              sumlist)))
                  ;; if not, create a new entry in comm-list.
                  (else
                   (let ((pair (list acc-comm (cons (gnc:make-value-collector)
                                                    (gnc:make-value-collector)))))
                     ((caadr pair) 'add share-amt)
                     ((cdadr pair) 'add value-amt)
                     (loop (cdr comm-splits)
                           (cons (list (car comm-list) (cons pair (cadr comm-list)))
                                 (alist-delete
                                  (car comm-list) sumlist))))))))

           ((assoc acc-comm sumlist)
            => (lambda (comm-list)
                 (cond
                  ;; other commodity already exists in comm-list?
                  ((assoc txn-comm (cadr comm-list))
                   => (lambda (pair)
                        ((caadr pair) 'add (- value-amt))
                        ((cdadr pair) 'add (- share-amt))
                        (loop (cdr comm-splits)
                              sumlist)))
                  (else
                   (let ((pair (list txn-comm (cons (gnc:make-value-collector)
                                                    (gnc:make-value-collector)))))
                     ;; And add the balances to the comm-list entry.
                     ((caadr pair) 'add (- value-amt))
                     ((cdadr pair) 'add (- share-amt))
                     (loop (cdr comm-splits)
                           (cons (list (car comm-list) (cons pair (cadr comm-list)))
                                 (alist-delete
                                  (car comm-list) sumlist))))))))

           (else
            ;; no, create sub-alist from scratch
            (let ((pair (list txn-comm (cons (gnc:make-value-collector)
                                             (gnc:make-value-collector)))))
              ((caadr pair) 'add value-amt)
              ((cdadr pair) 'add share-amt)
              ;; and add the new sub-alist to sumlist.
              (loop (cdr comm-splits)
                    (cons (list acc-comm (list pair)) sumlist)))))))))))

(define (gnc:make-exchange-alist report-commodity end-date)
  ;; This returns the alist with the actual exchange rates, i.e. the
  ;; total balances from get-exchange-totals are divided by each
  ;; other.
  (map
   (match-lambda
     ((comm (domestic . foreign))
      (list comm (abs (/ (foreign 'total #f) (domestic 'total #f))))))
   (gnc:get-exchange-totals report-commodity end-date)))

(define (gnc:make-exchange-cost-alist report-commodity end-date)
  ;; This returns the alist with the actual exchange rates, i.e. the
  ;; total balances from get-exchange-totals are divided by each
  ;; other.
  (map
   (match-lambda
     ((comm (domestic . foreign))
      (let ((denom (domestic 'total #f)))
        (list comm (if (zero? denom) 0 (abs (/ (foreign 'total #f) denom)))))))
   (gnc:get-exchange-cost-totals report-commodity end-date)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual functions for exchanging amounts.


;; Exchange EURO currencies to each other, or returns #f if one of
;; them is not an EURO currency at the given time. The function takes
;; the <gnc-monetary> 'foreign' amount, the <gnc:commodity*>
;; 'domestic' commodity, and a <gnc:time-pair> 'date'. It exchanges
;; the amount into the domestic currency. If the 'date' is #f, it
;; doesn't check for it. Returns a <gnc-monetary>, or #f if at least
;; one of the currencies is not in the EURO.
(define (gnc:exchange-by-euro foreign domestic date)
  (and (gnc-is-euro-currency domestic)
       (gnc-is-euro-currency (gnc:gnc-monetary-commodity foreign))
       ;; FIXME: implement the date check.
       (gnc:make-gnc-monetary
        domestic
        (gnc-convert-from-euro
         domestic
         (gnc-convert-to-euro (gnc:gnc-monetary-commodity foreign)
                              (gnc:gnc-monetary-amount foreign))))))


;; A trivial exchange function - if the "foreign" monetary amount
;; and the domestic currency are the same, return the foreign
;; amount unchanged, otherwise return 0

;; WARNING: many uses of exchange functions assume that the function
;; will return a valid <gnc:monetary>.  However, this function returns
;; #f if the commodities don't match.  Therefore, if you use this
;; function in a mixed commodity context, stuff will probably crash.
(define (gnc:exchange-if-same foreign domestic)
  (and (gnc-commodity-equiv (gnc:gnc-monetary-commodity foreign) domestic)
       foreign))

;; This one returns the ready-to-use function for calculation of the
;; exchange rates.  The returned function takes a <gnc-monetary> and
;; the <gnc:commodity*> domestic-commodity, exchanges the amount into
;; the domestic currency and returns a <gnc-monetary>.
(define (gnc:make-exchange-function exchange-alist)
  (lambda (foreign domestic)
    (gnc:debug "foreign: " (gnc:monetary->string foreign))
    (gnc:debug "domestic: " (gnc-commodity-get-printname domestic))
    (and foreign
         (or (gnc:exchange-by-euro foreign domestic #f)
             (gnc:exchange-if-same foreign domestic)
             (let* ((foreign-comm (gnc:gnc-monetary-commodity foreign))
                    (pair (assoc foreign-comm exchange-alist)))
               (gnc:make-gnc-monetary
                domestic
                (if pair
                    (* (gnc:gnc-monetary-amount foreign) (cadr pair))
                    0)))))))

;; Helper for the gnc:exchange-by-pricalist* below. Exchange the
;; <gnc:monetary> 'foreign' into the <gnc:commodity*> 'domestic' by
;; the <gnc:numeric> 'price-value'. Returns a <gnc:monetary>.
(define (gnc:exchange-by-pricevalue-helper
         foreign domestic price-value)
  (issue-deprecation-warning
   "gnc:exchange-by-pricevalue-helper is deprecated. please inline function.")
  (and (gnc:gnc-monetary? foreign)
       (gnc:make-gnc-monetary
        domestic
        (if price-value
            (* (gnc:gnc-monetary-amount foreign)
               price-value)
            (begin
              (warn "gnc:exchange-by-pricevalue-helper: No price found for "
                    (gnc:monetary->string foreign) " into "
                    (gnc:monetary->string
                     (gnc:make-gnc-monetary domestic 0)))
              0)))))

;; Helper for gnc:exchange-by-pricedb-* below. 'price' gets tested for
;; #f here, and gets unref'd here too. Exchange the <gnc:monetary>
;; 'foreign' into the <gnc:commodity*> 'domestic' by the <gnc:Price>
;; 'price'. Returns a <gnc:monetary>.
(define (gnc:exchange-by-pricedb-helper
         foreign domestic price)
  (issue-deprecation-warning
   "gnc:exchange-by-pricedb-helper is deprecated.")
  (and (gnc:gnc-monetary? foreign)
       (gnc:make-gnc-monetary
        domestic
        (if price
            (let ((result
                   (* (gnc:gnc-monetary-amount foreign)
                      (gnc-price-get-value price))))
              (gnc-price-unref price)
              result)
            (begin
              (warn "gnc:exchange-by-pricedb-helper: No price found for "
                    (gnc:monetary->string foreign) " into "
                    (gnc:monetary->string
                     (gnc:make-gnc-monetary domestic 0)))
              0)))))

;; This is another ready-to-use function for calculation of exchange
;; rates. (Note that this is already the function itself. It doesn't
;; return a function as opposed to make-exchange-function.) It takes
;; the <gnc-monetary> 'foreign' amount and the <gnc:commodity*>
;; 'domestic' commodity. It exchanges the amount into the domestic
;; currency, using the latest price from the pricedb. The function
;; returns a <gnc-monetary>.
(define (gnc:exchange-by-pricedb-latest
         foreign domestic)
  (and (record? foreign)
       (gnc:gnc-monetary? foreign)
       (or (gnc:exchange-by-euro foreign domestic #f)
           (gnc:exchange-if-same foreign domestic)
           (gnc:make-gnc-monetary
            domestic
            (gnc-pricedb-convert-balance-latest-price
             (gnc-pricedb-get-db (gnc-get-current-book))
             (gnc:gnc-monetary-amount foreign)
             (gnc:gnc-monetary-commodity foreign)
             domestic)))))

;; Yet another ready-to-use function for calculation of exchange
;; rates. (Note that this is already the function itself. It doesn't
;; return a function as opposed to make-exchange-function.) It takes
;; the <gnc-monetary> 'foreign' amount, the <gnc:commodity*>
;; 'domestic' commodity *and* a <gnc:time-pair> 'date'. It exchanges
;; the amount into the domestic currency, using a price from the
;; pricedb according to the given date. The function returns a
;; <gnc-monetary>.
(define (gnc:exchange-by-pricedb-nearest
         foreign domestic date)
  (and (record? foreign)
       (gnc:gnc-monetary? foreign)
       date
       (or (gnc:exchange-by-euro foreign domestic date)
           (gnc:exchange-if-same foreign domestic)
           (gnc:make-gnc-monetary
            domestic
            (gnc-pricedb-convert-balance-nearest-price-t64
             (gnc-pricedb-get-db (gnc-get-current-book))
             (gnc:gnc-monetary-amount foreign)
             (gnc:gnc-monetary-commodity foreign)
             domestic (time64CanonicalDayTime date))))))

;; Exchange by the nearest price from pricelist. This function takes
;; the <gnc-monetary> 'foreign' amount, the <gnc:commodity*>
;; 'domestic' commodity, a <gnc:time64> 'date' and the
;; 'pricelist'. It exchanges the amount into the domestic currency,
;; using the price nearest to 'data' found in the pricelist. The
;; function returns a <gnc-monetary>.
(define (gnc:exchange-by-pricealist-nearest
         pricealist foreign domestic date)
  ;;Used in weighted-average gnc:case-exchange-time-fn only.
  (gnc:debug "foreign " (gnc:monetary->string foreign))
  (gnc:debug "domestic " (gnc-commodity-get-printname domestic))
  (and (record? foreign)
       (gnc:gnc-monetary? foreign)
       date
       (or (gnc:exchange-by-euro foreign domestic date)
           (gnc:exchange-if-same foreign domestic)
           (let* ((foreign-comm (gnc:gnc-monetary-commodity foreign))
                  (foreign-amt (gnc:gnc-monetary-amount foreign))
                  (plist (assoc-ref pricealist foreign-comm))
                  (price (and plist
                              (gnc:pricelist-price-find-nearest plist date))))
             (gnc:make-gnc-monetary domestic (* foreign-amt (or price 0)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Choosing exchange functions made easy -- get the right function by
;; the value of a multichoice option.


;; Return a ready-to-use function. Which one to use is determined by
;; the value of 'source-option', whose possible values are set in
;; gnc:options-add-price-source!.
(define (gnc:case-exchange-fn
         source-option report-currency to-date-tp)
  (case source-option
    ((average-cost) (gnc:make-exchange-function
                     (gnc:make-exchange-cost-alist
                      report-currency to-date-tp)))
    ((weighted-average) (gnc:make-exchange-function
                         (gnc:make-exchange-alist
                          report-currency to-date-tp)))
    ((pricedb-latest) gnc:exchange-by-pricedb-latest)
    ((pricedb-nearest) (lambda (foreign domestic)
                         (gnc:exchange-by-pricedb-nearest
                          foreign domestic to-date-tp)))
    (else
     (begin
       ;; FIX-ME
       ;; this is a hack to prevent report crashing if a report
       ;; implements source-options that aren't fully implemented. We
       ;; return a reasonably sane fallback function: nearest.
       ;;
       ;; known to be missing: pricedb-latest-before
       (gnc:warn "gnc:case-exchange-fn: bad price-source value: "
                 source-option " using pricedb-nearest.")
       (lambda (foreign domestic)
         (gnc:exchange-by-pricedb-nearest
          foreign domestic to-date-tp))))))

;; Return a ready-to-use function. Which one to use is determined by
;; the value of 'source-option', whose possible values are set in
;; gnc:options-add-price-source!.
;;
;; arguments:
;; source-option: symbol 'average-cost 'weighted-average
;;                'pricedb-nearest 'pricedb-latest
;; report-currency: the target currency
;; commodity-list: the list of commodities to generate an exchange-fn for
;; to-date-tp (time64): last date to analyse transactions
;; start-percent, delta-percent: Fill in the [start:start+delta]
;; section of the progress bar while running this function.
;;
;; returns: a function which takes 3 arguments, and returns a gnc-monetary
;;    foreign  - foreign commodity/currency
;;    domestic - a gnc-monetary pair
;;    date     - time64 price
(define (gnc:case-exchange-time-fn
         source-option report-currency commodity-list to-date-tp
         start-percent delta-percent)
  (case source-option
    ;; Make this the same as gnc:case-exchange-fn
    ((average-cost) (let* ((exchange-fn (gnc:make-exchange-function
                                         (gnc:make-exchange-cost-alist
                                          report-currency to-date-tp))))
                      (lambda (foreign domestic date)
                        (exchange-fn foreign domestic))))
    ((weighted-average) (let ((pricealist
                               (gnc:get-commoditylist-totalavg-prices
                                commodity-list report-currency to-date-tp
                                start-percent delta-percent)))
                          (gnc:debug "weighted-average pricealist " pricealist)
                          (lambda (foreign domestic date)
                            (gnc:exchange-by-pricealist-nearest
                             pricealist foreign domestic date))))
  ;; actual-transactions isn't used, at least not as a value passed to this
  ;; function. price-scatter.scm does use it but calls
  ;; gnc:get-commodity-inst-prices directly.
    ((actual-transactions) (let ((pricealist
                                  (gnc:get-commoditylist-inst-prices
                                   commodity-list report-currency to-date-tp
                                   start-percent delta-percent)))
                             (issue-deprecation-warning
                              "this path is never reached in code.")
                             (lambda (foreign domestic date)
                               (gnc:exchange-by-pricealist-nearest
                                pricealist foreign domestic date))))
    ((pricedb-latest) (lambda (foreign domestic date)
                        (gnc:exchange-by-pricedb-latest foreign domestic)))
    ((pricedb-nearest) gnc:exchange-by-pricedb-nearest)
    (else
     (begin
       (gnc:warn "gnc:case-exchange-time-fn: bad price-source value: "
                 source-option ". Using pricedb-nearest.")
       ;; FIX-ME another hack to prevent report crashing when an
       ;; unimplemented source-option comes through
       gnc:exchange-by-pricedb-nearest))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions using the exchange-fn's above to get from a
;; commodity-collector to one value.


;; Adds all different commodities in the commodity-collector <foreign>
;; by using the exchange rates of <exchange-fn> to calculate the
;; exchange rates to the commodity <domestic>.
;;
;; CAS: Previously, the exchange-fn was not optional -- we would crash
;; if it was invalid.  I've changed this so that when exchange-fn is
;; #f, #f is returned.  Since #f is already returned when foreign is
;; #f, and since any previous dependence on some behavior for the case
;; where exchange-fn was #f would've crashed, I think this change is
;; safe.
;;
;; Returns a <gnc-monetary> with the domestic commodity and its
;; corresponding balance. If the foreign balance is #f, it returns #f.
(define (gnc:sum-collector-commodity foreign domestic exchange-fn)
  (and foreign
       exchange-fn
       (gnc:make-gnc-monetary
        domestic
        (apply + (map
                  (lambda (mon)
                    (gnc-numeric-convert
                     (gnc:gnc-monetary-amount (exchange-fn mon domestic))
                     (gnc-commodity-get-fraction domestic)
                     GNC-RND-ROUND))
                  (foreign 'format gnc:make-gnc-monetary #f))))))

;; As above, but adds only the commodities of other stocks and
;; mutual-funds. Returns a commodity-collector, (not a <gnc:monetary>)
;; which (still) may have several different commodities in it -- if
;; there have been different *currencies*, not only stocks.
(define (gnc:sum-collector-stocks foreign domestic exchange-fn)
  (issue-deprecation-warning
   "gnc:sum-collector-stocks is never used in code.")
  (and foreign
       (let ((balance (gnc:make-commodity-collector)))
         (foreign
          'format
          (lambda (curr val)
            (if (gnc-commodity-equiv domestic curr)
                (balance 'add domestic val)
                (if (gnc-commodity-is-currency curr)
                    (balance 'add curr val)
                    (balance 'add domestic
                             (gnc:gnc-monetary-amount
                              (exchange-fn (gnc:make-gnc-monetary curr val)
                                           domestic))))))
          #f)
         balance)))

(define (gnc-commodity-collector-commodity-count collector)
  (issue-deprecation-warning
   "gnc-commodity-collector-commodity-count is deprecated. please inline.")
  (length (collector 'format (lambda (comm amt) comm) #f)))

(define (gnc-commodity-collector-contains-commodity? collector commodity)
  (issue-deprecation-warning
   "gnc-commodity-collector-contains-commodity? is deprecated. please inline.")
  (member commodity
          (collector 'format (lambda (comm amt) comm) #f)
          gnc-commodity-equiv))

(define (gnc:uniform-commodity? amt report-commodity)
  ;; function to see if the commodity-collector amt
  ;; contains any foreign commodities
  (match (amt 'format (lambda (comm amt) comm) #f)
    (() #t)
    ((comm) (gnc-commodity-equiv report-commodity comm))
    (_ #f)))
