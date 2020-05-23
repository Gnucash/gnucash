;; -*-scheme-*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; advanced-portfolio.scm
;; by Martijn van Oosterhout (kleptog@svana.org) Feb 2002
;; modified for GnuCash 1.8 by Herbert Thoma (herbie@hthoma.de) Oct 2002
;;
;; Heavily based on portfolio.scm
;; by Robert Merkel (rgmerk@mira.net)
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash reports standard advanced-portfolio))

(use-modules (gnucash engine))
(use-modules (gnucash utilities)) 
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))

(define reportname (N_ "Advanced Portfolio"))

(define optname-price-source (N_ "Price Source"))
(define optname-shares-digits (N_ "Share decimal places"))
(define optname-zero-shares (N_ "Include accounts with no shares"))
(define optname-show-symbol (N_ "Show ticker symbols"))
(define optname-show-listing (N_ "Show listings"))
(define optname-show-price (N_ "Show prices"))
(define optname-show-shares (N_ "Show number of shares"))
(define optname-basis-method (N_ "Basis calculation method"))
(define optname-prefer-pricelist (N_ "Set preference for price list data"))
(define optname-brokerage-fees (N_ "How to report brokerage fees"))

(define OVERFLOW-ERROR "<h3>Error</h3>There is an error processing the
transaction '~a'. This may to be caused by a sell transaction causing
a negative stock balance, and a subsequent buy transaction causing a
zero balance. This leads to a division-by-zero error. It can be fixed
by preventing negative stock balances.<br/>")

;; To avoid overflows in our calculations, define a denominator for prices and unit values
(define price-denom 100000000)
(define units-denom 100000000)

(define (options-generator)
  (let* ((options (gnc:new-options))
         ;; This is just a helper function for making options.
         ;; See libgnucash/scm/options.scm for details.
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    ;; General Tab
    ;; date at which to report balance
    (gnc:options-add-report-date!
     options gnc:pagename-general
     (N_ "Date") "a")

    (gnc:options-add-currency!
     options gnc:pagename-general (N_ "Report's currency") "c")

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-price-source
      "d" (N_ "The source of price information.") 'pricedb-nearest
      (list (vector 'pricedb-latest
		    (N_ "Most recent")
		    (N_ "The most recent recorded price."))
	    (vector 'pricedb-nearest
		    (N_ "Nearest in time")
		    (N_ "The price recorded nearest in time to the report date."))
	    )))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-basis-method
      "e" (N_ "Basis calculation method.") 'average-basis
      (list (vector 'average-basis
		    (N_ "Average")
		    (N_ "Use average cost of all shares for basis."))
	    (vector 'fifo-basis
		    (N_ "FIFO")
		    (N_ "Use first-in first-out method for basis."))
	    (vector 'filo-basis
		    (N_ "LIFO")
		    (N_ "Use last-in first-out method for basis."))
	    )))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-prefer-pricelist "f"
      (N_ "Prefer use of price editor pricing over transactions, where applicable.")
      #t))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-brokerage-fees
      "g" (N_ "How to report commissions and other brokerage fees.") 'include-in-basis
      (list (vector 'include-in-basis
                    (N_ "Include in basis")
                    (N_ "Include brokerage fees in the basis for the asset."))
            (vector 'include-in-gain
                    (N_ "Include in gain")
                    (N_  "Include brokerage fees in the gain and loss but not in the basis."))
            (vector 'ignore-brokerage
                    (N_ "Ignore")
                    (N_ "Ignore brokerage fees entirely."))
            )))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-symbol "a"
	(N_ "Display the ticker symbols.")
	#t))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-listing "b"
	(N_ "Display exchange listings.")
	#t))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-shares "c"
	(N_ "Display numbers of shares in accounts.")
	#t))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-shares-digits
      "d" (N_ "The number of decimal places to use for share numbers.") 2
      0 6 0 1))

    (gnc:register-option
      options
      (gnc:make-simple-boolean-option
	gnc:pagename-display optname-show-price "e"
	(N_ "Display share prices.")
	#t))

    ;; Account tab
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts (N_ "Accounts")
      "b"
      (N_ "Stock Accounts to report on.")
      (lambda () (filter gnc:account-is-stock?
                         (gnc-account-get-descendants-sorted
                          (gnc-get-current-root-account))))
      (lambda (accounts) (list  #t
                                (filter gnc:account-is-stock? accounts)))
      #t))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-zero-shares "e"
      (N_ "Include accounts that have a zero share balances.")
      #f))

    (gnc:options-set-default-section options gnc:pagename-general)
    options))


(define (make-text-cell . args)
  (apply gnc:make-html-table-header-cell/markup "text-cell" args))
(define (make-number-cell . args)
  (apply gnc:make-html-table-header-cell/markup "number-cell" args))
(define (make-total-number-cell . args)
  (apply gnc:make-html-table-cell/markup "total-number-cell" args))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.

(define (advanced-portfolio-renderer report-obj)

 (let ((work-done 0)
       (work-to-do 0)
       (warn-no-price #f)
       (warn-price-dirty #f))

  ;; These are some helper functions for looking up option values.
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))

  (define (get-option section name)
    (gnc:option-value (get-op section name)))

  (define (split-account-type? split type)
    (eq? type (xaccAccountGetType (xaccSplitGetAccount split))))

  ;; sum up the contents of the b-list built by basis-builder below
  (define (sum-basis b-list)
    (match b-list
      (() 0)
      (((units . value) . rest) (+ (* units value) (sum-basis rest)))))

  ;; sum up the total number of units in the b-list built by basis-builder below
  (define (units-basis b-list)
    (match b-list
      (() 0)
      (((units . _) . rest) (+ units (units-basis rest)))))

  ;; apply a ratio to an existing basis-list, useful for splits/mergers and spinoffs
  ;; I need to get a brain and use (map) for this.
  (define (apply-basis-ratio b-list units-ratio value-ratio)
    (match b-list
      (() '())
      (((units . value) . rest)
       (cons (cons (* units-ratio units) (* value-ratio value))
             (apply-basis-ratio rest units-ratio value-ratio)))))

  ;; this builds a list for basis calculation and handles average,
  ;; fifo and lifo methods the list is cons cells of (units-of-stock
  ;; . price-per-unit)... average method produces only one cell that
  ;; mutates to the new average. Need to add a date checker so that we
  ;; allow for prices coming in out of order, such as a transfer with
  ;; a price adjusted to carryover the basis.
  (define (basis-builder b-list b-units b-value b-method currency-frac)
    (gnc:debug "actually in basis-builder")
    (gnc:debug "b-list is " b-list " b-units is " b-units
               " b-value is " b-value " b-method is " b-method)

    ;; if there is no b-value, then this is a split/merger and needs
    ;; special handling
    (cond

     ;; we have value and positive units, add units to basis
     ((and (not (zero? b-value)) (positive? b-units))
      (case b-method
        ((average-basis)
         (cond
          ((null? b-list)
           (append b-list (list (cons b-units (/ b-value b-units)))))
          ((zero? (+ b-units (caar b-list)))
           (throw 'div/0 (format #f "buying ~0,4f share units" b-units)))
          (else
           (list (cons (+ b-units (caar b-list))
                       (/ (+ b-value (* (caar b-list) (cdar b-list)))
                          (+ b-units (caar b-list))))))))
        (else (append b-list (list (cons b-units (/ b-value b-units)))))))

     ;; we have value and negative units, remove units from basis
     ((and (not (zero? b-value)) (negative? b-units))
      (if (null? b-list)
          '()
          (case b-method
            ((fifo-basis)
             (case (gnc-numeric-compare (abs b-units) (caar b-list))
               ((-1)
                ;; Sold less than the first lot, create a new first
                ;; lot from the remainder
                (cons (cons (+ b-units (caar b-list)) (cdar b-list)) (cdr b-list)))
               ((0)
                ;; Sold all of the first lot
                (cdr b-list))
               ((1)
                ;; Sold more than the first lot, delete it and recurse
                (basis-builder (cdr b-list) (+ b-units (caar b-list))
                               b-value b-method currency-frac))))

            ((filo-basis)
             (let ((rev-b-list (reverse b-list)))
               (case (gnc-numeric-compare (abs b-units) (caar rev-b-list))
                 ((-1)
                  ;; Sold less than the last lot
                  (reverse (cons (cons (+ b-units (caar rev-b-list)) (cdar rev-b-list))
                                 (cdr rev-b-list))))
                 ((0)
                  ;; Sold all of the last lot
                  (reverse (cdr rev-b-list)))

                 ((1)
                  ;; Sold more than the last lot
                  (basis-builder (reverse (cdr rev-b-list))
                                 (+ b-units (caar rev-b-list))
                                 b-value b-method currency-frac)))))

            ((average-basis)
             (list (cons (+ (caar b-list) b-units) (cdar b-list)))))))

     ;; no value, just units, this is a split/merge...
     ((and (zero? b-value) (not (zero? b-units)))
      (let* ((current-units (units-basis b-list))
             ;; If current-units is zero then so should be everything else.
             (units-ratio (if (zero? current-units)
                              0
                              (/ (+ b-units current-units) current-units)))
             ;; If the units ratio is zero the stock is worthless and
             ;; the value should be zero too
             (value-ratio (if (zero? units-ratio)
                              0
                              (/ 1 units-ratio))))

        (gnc:debug "blist is " b-list " current units is " current-units
                   " value ratio is " value-ratio
                   " units ratio is " units-ratio)
        (apply-basis-ratio b-list units-ratio value-ratio)))

     ;; If there are no units, just a value, then its a spin-off,
     ;; calculate a ratio for the values, but leave the units alone
     ;; with a ratio of 1
     ((and (zero? b-units) (not (zero? b-value)))
      (let* ((current-value (sum-basis b-list))
             (value-ratio (if (zero? current-value)
                              (throw 'div/0 (format #f "spinoff of ~,2f currency units"
                                                    current-value))
                              (/ (+ b-value current-value)
                                 current-value))))

        (gnc:debug "this is a spinoff")
        (gnc:debug "blist is " b-list " value ratio is " value-ratio)
        (apply-basis-ratio b-list 1 value-ratio)))

     ;; when all else fails, just send the b-list back
     (else b-list)))

  ;; Given a price list and a currency find the price for that currency on the list.
  ;; If there is none for the requested currency, return the first one.
  ;; The price list is released but the price returned is ref counted.
  (define (find-price price-list currency)
    (if (eqv? price-list '()) #f
      (let ((price (car price-list)))
        (for-each
          (lambda (p)
            (if (gnc-commodity-equiv currency (gnc-price-get-currency p))
                (set! price p))
            (if (gnc-commodity-equiv currency (gnc-price-get-commodity p))
                (set! price (gnc-price-invert p))))
          price-list)
        (gnc-price-ref price)
        (gnc-price-list-destroy price-list)
        price)))

  ;; Return true if either account is the parent of the other or they are siblings
  (define (parent-or-sibling? a1 a2)
    (let ((a2parent (gnc-account-get-parent a2))
          (a1parent (gnc-account-get-parent a1)))
          (or (equal? a2parent a1)
              (equal? a1parent a2)
              (equal? a1parent a2parent))))

  ;; Test whether the given split is the source of a spin off transaction
  ;; This will be a no-units split with only one other split.
  ;; xaccSplitGetOtherSplit only returns on a two-split txn.  It's not a spinoff
  ;; is the other split is in an income or expense account.
  (define (spin-off? split current)
     (let ((other-split (xaccSplitGetOtherSplit split)))
          (and (gnc-numeric-zero-p (xaccSplitGetAmount split))
               (equal? current (xaccSplitGetAccount split))
               (not (null? other-split))
               (not (split-account-type? other-split ACCT-TYPE-EXPENSE))
               (not (split-account-type? other-split ACCT-TYPE-INCOME)))))

  (define share-print-info
    (gnc-share-print-info-places
     (inexact->exact (get-option gnc:pagename-display optname-shares-digits))))

  (define (table-add-stock-rows
           table accounts to-date currency
           price-fn exchange-fn price-source include-empty show-symbol
           show-listing show-shares show-price basis-method prefer-pricelist
           handle-brokerage-fees total-basis total-value total-moneyin
           total-moneyout total-income total-gain total-ugain total-brokerage)

    (set! work-to-do (gnc:accounts-count-splits accounts))

    (let lp ((accounts accounts) (odd-row? #t))
      (match accounts
        (() total-value)
        ((current . rest)
         (let* ((row-style (if odd-row? "normal-row" "alternate-row"))
                ;; commodity is the actual stock/thing we are looking at
                (commodity (xaccAccountGetCommodity current))
                (ticker-symbol (gnc-commodity-get-mnemonic commodity))
                (listing (gnc-commodity-get-namespace commodity))
                (unit-collector (gnc:account-get-comm-balance-at-date
                                 current to-date #f))
                (units (cadr (unit-collector 'getpair commodity #f)))

                ;; Counter to keep track of stuff
                (brokeragecoll (gnc:make-commodity-collector))
                (dividendcoll  (gnc:make-commodity-collector))
                (moneyincoll   (gnc:make-commodity-collector))
                (moneyoutcoll  (gnc:make-commodity-collector))
                (gaincoll      (gnc:make-commodity-collector))

                ;; the price of the commodity at the time of the report
                (price (price-fn commodity currency to-date))
                ;; the value of the commodity, expressed in terms of
                ;; the report's currency.
                (value (gnc:make-gnc-monetary currency 0))  ;; Set later
                (currency-frac (gnc-commodity-get-fraction currency))

                (pricing-txn #f)
                (use-txn #f)
                (basis-list '())
                ;; setup an alist for the splits we've already seen.
                (seen_trans '())
                ;; Account used to hold remainders from income reinvestments and
                ;; running total of amount moved there
                (drp-holding-account #f)
                (drp-holding-amount 0))

           (define (my-exchange-fn fromunits tocurrency)
             (if (and (gnc-commodity-equiv currency tocurrency)
                      (gnc-commodity-equiv (gnc:gnc-monetary-commodity fromunits) commodity))
                 ;; Have a price for this commodity, but not necessarily in the report's
                 ;; currency.  Get the value in the commodity's currency and convert it to
                 ;; report currency.
                 (exchange-fn
                  ;; This currency will usually be the same as tocurrency so the
                  ;; call to exchange-fn below will do nothing
                  (gnc:make-gnc-monetary
                   (if use-txn
                       (gnc:gnc-monetary-commodity price)
                       (gnc-price-get-currency price))
                   (* (gnc:gnc-monetary-amount fromunits)
                      (if use-txn
                          (gnc:gnc-monetary-amount price)
                          (gnc-price-get-value price))))
                  tocurrency)
                 (exchange-fn fromunits tocurrency)))

           (define (zero-split? s)
             (or (zero? (xaccSplitGetAmount s))
                 (zero? (xaccSplitGetValue s))))

           (gnc:debug "Starting account " (xaccAccountGetName current) ", initial price: "
                      (and price
                           (gnc:monetary->string
                            (gnc:make-gnc-monetary
                             (gnc-price-get-currency price) (gnc-price-get-value price)))))

           ;; If we have a price that can't be converted to the report currency
           ;; don't use it
           (if (and price (zero? (gnc:gnc-monetary-amount
                                  (exchange-fn
                                   (gnc:make-gnc-monetary
                                    (gnc-price-get-currency price)
                                    100)
                                   currency))))
               (set! price #f))

           ;; If we are told to use a pricing transaction, or if we don't have a price
           ;; from the price DB, find a good transaction to use.
           (if (and (not use-txn) (or (not price) (not prefer-pricelist)))
               (let lp ((split-list (reverse (gnc:get-match-commodity-splits-sorted
                                              (list current)
                                              (case price-source
                                                ((pricedb-latest) (gnc:get-today))
                                                ((pricedb-nearest) to-date)
                                                (else (gnc:get-today)))  ;; error, but don't crash
                                              #f))))
                 (match split-list
                   (() #f)
                   (((? zero-split?) . rest) (lp rest))
                   ((split . rest)
                    (let* ((trans (xaccSplitGetParent split))
                           (trans-currency (xaccTransGetCurrency trans))
                           (trans-price (exchange-fn (gnc:make-gnc-monetary
                                                      trans-currency
                                                      (xaccSplitGetSharePrice split))
                                                     currency)))
                      (cond
                       ((zero? (gnc:gnc-monetary-amount trans-price)) (lp rest))
                       ;; We can exchange the price from this transaction into the report currency
                       (else
                        (if price (gnc-price-unref price))
                        (set! pricing-txn trans)
                        (set! price trans-price)
                        (gnc:debug "Transaction price is " (gnc:monetary->string price))
                        (set! use-txn #t))))))))

           ;; If we still don't have a price, use a price of 1 and complain later
           (unless price
             (set! price (gnc:make-gnc-monetary currency 1))
             ;; If use-txn is set, but pricing-txn isn't set, it's a bogus price
             (set! use-txn #t)
             (set! pricing-txn #f))

           ;; Now that we have a pricing transaction if needed, set the value of the asset
           (set! value (my-exchange-fn (gnc:make-gnc-monetary commodity units) currency))
           (gnc:debug "Value " (gnc:monetary->string value)
                      " from " (gnc:monetary->string
                                (gnc:make-gnc-monetary commodity units)))

           (for-each
            ;; we're looking at each split we find in the account. these splits
            ;; could refer to the same transaction, so we have to examine each
            ;; split, determine what kind of split it is and then act accordingly.
            (lambda (split)
              (set! work-done (+ 1 work-done))
              (gnc:report-percent-done (* 100 (/ work-done work-to-do)))

              (let* ((parent (xaccSplitGetParent split))
                     (txn-date (xaccTransGetDate parent))
                     (commod-currency (xaccTransGetCurrency parent))
                     (commod-currency-frac (gnc-commodity-get-fraction commod-currency)))

                (if (and (<= txn-date to-date)
                         (not (assoc-ref seen_trans (gncTransGetGUID parent))))
                    (let ((trans-income 0)
                          (trans-brokerage 0)
                          (trans-shares 0)
                          (shares-bought 0)
                          (trans-sold 0)
                          (trans-bought 0)
                          (trans-spinoff 0)
                          (trans-drp-residual 0)
                          (trans-drp-account #f))

                      (gnc:debug "Transaction " (xaccTransGetDescription parent))
                      ;; Add this transaction to the list of processed transactions so we don't
                      ;; do it again if there is another split in it for this account
                      (set! seen_trans (acons (gncTransGetGUID parent) #t seen_trans))

                      ;; Go through all the splits in the transaction to get an overall idea of
                      ;; what it does in terms of income, money in or out, shares bought or sold, etc.
                      (for-each
                       (lambda (s)
                         (let ((split-units (xaccSplitGetAmount s))
                               (split-value (xaccSplitGetValue s)))

                           (gnc:debug "Pass 1: split units " (gnc-numeric-to-string split-units) " split-value "
                                      (gnc-numeric-to-string split-value) " commod-currency "
                                      (gnc-commodity-get-printname commod-currency))

                           (cond
                            ((split-account-type? s ACCT-TYPE-EXPENSE)
                             ;; Brokerage expense unless a two split transaction with other split
                             ;; in the stock account in which case it's a stock donation to charity.
                             (if (not (equal? current (xaccSplitGetAccount (xaccSplitGetOtherSplit s))))
                                 (set! trans-brokerage
                                   (+ trans-brokerage split-value))))

                            ((split-account-type? s ACCT-TYPE-INCOME)
                             (set! trans-income (- trans-income split-value)))

                            ((equal? current (xaccSplitGetAccount s))
                             (set! trans-shares (+ trans-shares (abs split-units)))
                             (if (zero? split-units)
                                 (if (spin-off? s current)
                                     ;; Count money used in a spin off as money out
                                     (if (negative? split-value)
                                         (set! trans-spinoff (- trans-spinoff split-value)))
                                     (if (not (zero? split-value))
                                         ;; Gain/loss split (amount zero, value non-zero, and not spinoff).  There will be
                                         ;; a corresponding income split that will incorrectly be added to trans-income
                                         ;; Fix that by subtracting it here
                                         (set! trans-income (- trans-income split-value))))
                                 ;; Non-zero amount, add the value to the sale or purchase total.
                                 (if (positive? split-value)
                                     (begin
                                       (set! trans-bought (+ trans-bought split-value))
                                       (set! shares-bought (+ shares-bought split-units)))
                                     (set! trans-sold (- trans-sold split-value)))))

                            ((split-account-type? s ACCT-TYPE-ASSET)
                             ;; If all the asset accounts mentioned in the transaction are siblings of each other
                             ;; keep track of the money transferred to them if it is in the correct currency
                             (if trans-drp-account
                                 (if (not (eq? trans-drp-account 'none))
                                     (if (parent-or-sibling? trans-drp-account (xaccSplitGetAccount s))
                                         (set! trans-drp-residual (+ trans-drp-residual split-value))
                                         (set! trans-drp-account 'none)))
                                 (begin
                                   (set! trans-drp-account (xaccSplitGetAccount s))
                                   (if (gnc-commodity-equiv commod-currency (xaccAccountGetCommodity trans-drp-account))
                                       (set! trans-drp-residual split-value)
                                       (set! trans-drp-account 'none))))))))
                       (xaccTransGetSplitList parent))

                      (gnc:debug "Income: " (gnc-numeric-to-string trans-income)
                                 " Brokerage: " (gnc-numeric-to-string trans-brokerage)
                                 " Shares traded: " (gnc-numeric-to-string trans-shares)
                                 " Shares bought: " (gnc-numeric-to-string shares-bought))
                      (gnc:debug " Value sold: " (gnc-numeric-to-string trans-sold)
                                 " Value purchased: " (gnc-numeric-to-string trans-bought)
                                 " Spinoff value " (gnc-numeric-to-string trans-spinoff)
                                 " Trans DRP residual: " (gnc-numeric-to-string trans-drp-residual))

                      ;; We need to calculate several things for this transaction:
                      ;; 1. Total income: this is already in trans-income
                      ;; 2. Change in basis: calculated by loop below that looks at every
                      ;;    that acquires or disposes of shares
                      ;; 3. Realized gain: also calculated below while calculating basis
                      ;; 4. Money in to the account: this is the value of shares bought
                      ;;    except those purchased with reinvested income
                      ;; 5. Money out: the money received by disposing of shares.   This
                      ;;    is in trans-sold plus trans-spinoff
                      ;; 6. Brokerage fees: this is in trans-brokerage

                      ;; Income
                      (dividendcoll 'add commod-currency trans-income)

                      ;; Brokerage fees.  May be either ignored or part of basis, but that
                      ;; will be dealt with elsewhere.
                      (brokeragecoll 'add commod-currency trans-brokerage)

                      ;; Add brokerage fees to trans-bought if not ignoring them and there are any
                      (if (and (not (eq? handle-brokerage-fees 'ignore-brokerage))
                               (positive? trans-brokerage)
                               (positive? trans-shares))
                          (let* ((fee-frac (/ shares-bought trans-shares))
                                 (fees (* trans-brokerage fee-frac)))
                            (set! trans-bought (+ trans-bought fees))))

                      ;; Update the running total of the money in the DRP residual account.  This is relevant
                      ;; if this is a reinvestment transaction (both income and purchase) and there seems to
                      ;; asset accounts used to hold excess income.
                      (if (and trans-drp-account
                               (not (eq? trans-drp-account 'none))
                               (positive? trans-income)
                               (positive? trans-bought))
                          (cond
                           ((not drp-holding-account)
                            (set! drp-holding-account trans-drp-account)
                            (set! drp-holding-amount trans-drp-residual))
                           ((and (not (eq? drp-holding-account 'none))
                                 (parent-or-sibling? trans-drp-account drp-holding-account))
                            (set! drp-holding-amount (+ drp-holding-amount trans-drp-residual)))
                           (else
                            ;; Wrong account (or no account), assume there isn't a DRP holding account
                            (set! drp-holding-account 'none)
                            (set trans-drp-residual 0)
                            (set! drp-holding-amount 0))))

                      ;; Set trans-bought to the amount of money moved in to the account which was used to
                      ;; purchase more shares.  If this is not a DRP transaction then all money used to purchase
                      ;; shares is money in.
                      (when (and (positive? trans-income)
                                 (positive? trans-bought))
                        (set! trans-bought (- trans-bought trans-income))
                        (set! trans-bought (+ trans-bought trans-drp-residual))
                        (set! trans-bought (- trans-bought drp-holding-amount))
                        ;; If the DRP holding account balance is negative, adjust it by the amount
                        ;; used in this transaction
                        (if (and (negative? drp-holding-amount)
                                 (positive? trans-bought))
                            (set! drp-holding-amount
                              (+ drp-holding-amount trans-bought)))
                        ;; Money in is never more than amount spent to purchase shares
                        (if (negative? trans-bought)
                            (set! trans-bought 0)))

                      (gnc:debug "Adjusted trans-bought " (gnc-numeric-to-string trans-bought)
                                 " DRP holding account " (gnc-numeric-to-string drp-holding-amount))

                      (moneyincoll 'add commod-currency trans-bought)
                      (moneyoutcoll 'add commod-currency trans-sold)
                      (moneyoutcoll 'add commod-currency trans-spinoff)

                      ;; Look at splits again to handle changes in basis and realized gains
                      (for-each
                       (lambda (s)
                         (let ((split-units (xaccSplitGetAmount s))
                               (split-value (xaccSplitGetValue s)))

                           (gnc:debug "Pass 2: split units " (gnc-numeric-to-string split-units) " split-value "
                                      (gnc-numeric-to-string split-value) " commod-currency "
                                      (gnc-commodity-get-printname commod-currency))

                           (cond
                            ((and (not (zero? split-units))
                                  (equal? current (xaccSplitGetAccount s)))
                             ;; Split into subject account with non-zero amount.  This is a purchase
                             ;; or a sale, adjust the basis
                             (let* ((split-value-currency (gnc:gnc-monetary-amount
                                                           (my-exchange-fn (gnc:make-gnc-monetary
                                                                            commod-currency split-value) currency)))
                                    (orig-basis (sum-basis basis-list))
                                    ;; proportion of the fees attributable to this split
                                    (fee-ratio (/ (abs split-units) trans-shares))
                                    ;; Fees for this split in report currency
                                    (fees-currency (gnc:gnc-monetary-amount (my-exchange-fn
                                                                             (gnc:make-gnc-monetary
                                                                              commod-currency
                                                                              (* fee-ratio trans-brokerage))
                                                                             currency)))
                                    (split-value-with-fees (if (eq? handle-brokerage-fees 'include-in-basis)
                                                               ;; Include brokerage fees in basis
                                                               (+ split-value-currency fees-currency)
                                                               split-value-currency)))
                               (gnc:debug "going in to basis list " basis-list " " (gnc-numeric-to-string split-units) " "
                                          (gnc-numeric-to-string split-value-with-fees))

                               ;; adjust the basis
                               (set! basis-list (basis-builder basis-list split-units split-value-with-fees
                                                               basis-method currency-frac))
                               (gnc:debug  "coming out of basis list " basis-list)

                               ;; If it's a sale or the stock is worthless, calculate the gain
                               (if (not (positive? split-value))
                                   ;; Split value is zero or negative.  If it's zero it's either a stock split/merge
                                   ;; or the stock has become worthless (which looks like a merge where the number
                                   ;; of shares goes to zero).  If the value is negative then it's a disposal of some sort.
                                   (let ((new-basis (sum-basis basis-list)))
                                     (if (or (zero? new-basis)
                                             (negative? split-value))
                                         ;; Split value is negative or new basis is zero (stock is worthless),
                                         ;; Capital gain is money out minus change in basis
                                         (let ((gain (- (abs split-value-with-fees)
                                                        (- orig-basis new-basis))))
                                           (gnc:debug "Old basis=" (gnc-numeric-to-string orig-basis)
                                                      " New basis=" (gnc-numeric-to-string new-basis)
                                                      " Gain=" (gnc-numeric-to-string gain))
                                           (gaincoll 'add currency gain)))))))

                            ;; here is where we handle a spin-off txn. This will be a no-units
                            ;; split with only one other split. xaccSplitGetOtherSplit only
                            ;; returns on a two-split txn.  It's not a spinoff is the other split is
                            ;; in an income or expense account.
                            ((spin-off? s current)
                             (gnc:debug "before spin-off basis list " basis-list)
                             (set! basis-list
                               (basis-builder
                                basis-list split-units
                                (gnc:gnc-monetary-amount
                                 (my-exchange-fn (gnc:make-gnc-monetary
                                                  commod-currency split-value)
                                                 currency))
                                basis-method currency-frac))
                             (gnc:debug "after spin-off basis list "  basis-list)))))
                       (xaccTransGetSplitList parent))))))
            (xaccAccountGetSplitList current))

           ;; Look for income and expense transactions that don't have a split in the
           ;; the account we're processing.  We do this as follow
           ;; 1. Make sure the parent account is a currency-valued asset or bank account
           ;; 2. If so go through all the splits in that account
           ;; 3. If a split is part of a two split transaction where the other split is
           ;;    to an income or expense account and the leaf name of that account is the
           ;;    same as the leaf name of the account we're processing, add it to the
           ;;    income or expense accumulator
           ;;
           ;; In other words with an account structure like
           ;;
           ;;   Assets (type ASSET)
           ;;     Broker (type ASSET)
           ;;       Widget Stock (type STOCK)
           ;;   Income (type INCOME)
           ;;     Dividends (type INCOME)
           ;;       Widget Stock (type INCOME)
           ;;
           ;; If you are producing a report on "Assets:Broker:Widget Stock" a
           ;; transaction that debits the Assets:Broker account and credits the
           ;; "Income:Dividends:Widget Stock" account will count as income in
           ;; the report even though it doesn't have a split in the account
           ;; being reported on.

           (let ((parent-account (gnc-account-get-parent current))
                 (account-name (xaccAccountGetName current)))
             (if (and (not (null? parent-account))
                      (member (xaccAccountGetType parent-account) (list ACCT-TYPE-ASSET ACCT-TYPE-BANK))
                      (gnc-commodity-is-currency (xaccAccountGetCommodity parent-account)))
                 (for-each
                  (lambda (split)
                    (let* ((other-split (xaccSplitGetOtherSplit split))
                           ;; This is safe because xaccSplitGetAccount returns null for a null split
                           (other-acct (xaccSplitGetAccount other-split))
                           (parent (xaccSplitGetParent split))
                           (txn-date (xaccTransGetDate parent)))
                      (if (and (not (null? other-acct))
                               (<= txn-date to-date)
                               (string=? (xaccAccountGetName other-acct) account-name)
                               (gnc-commodity-is-currency (xaccAccountGetCommodity other-acct)))
                          ;; This is a two split transaction where the other split is to an
                          ;; account with the same name as the current account.  If it's an
                          ;; income or expense account accumulate the value of the transaction
                          (let ((val (xaccSplitGetValue split))
                                (curr (xaccAccountGetCommodity other-acct)))
                            (cond ((split-account-type? other-split ACCT-TYPE-INCOME)
                                   (gnc:debug "More income " (gnc-numeric-to-string val))
                                   (dividendcoll 'add curr val))
                                  ((split-account-type? other-split ACCT-TYPE-EXPENSE)
                                   (gnc:debug "More expense " (gnc-numeric-to-string (- val)))
                                   (brokeragecoll 'add curr (- val))))))))
                  (xaccAccountGetSplitList parent-account))))

           (gnc:debug "pricing txn is " pricing-txn)
           (gnc:debug "use txn is " use-txn)
           (gnc:debug "prefer-pricelist is " prefer-pricelist)
           (gnc:debug "price is " price)

           (gnc:debug "basis we're using to build rows is " (gnc-numeric-to-string (sum-basis basis-list)))
           (gnc:debug "but the actual basis list is " basis-list)

           (if (eq? handle-brokerage-fees 'include-in-gain)
               (gaincoll 'minusmerge brokeragecoll #f))

           (if (or include-empty (not (zero? units)))
               (let* ((moneyin (gnc:sum-collector-commodity moneyincoll currency my-exchange-fn))
                      (moneyout (gnc:sum-collector-commodity moneyoutcoll currency my-exchange-fn))
                      (brokerage (gnc:sum-collector-commodity brokeragecoll currency my-exchange-fn))
                      (income (gnc:sum-collector-commodity dividendcoll currency my-exchange-fn))
                      ;; just so you know, gain == realized gain, ugain == un-realized gain, bothgain, well..
                      (gain (gnc:sum-collector-commodity gaincoll currency my-exchange-fn))
                      (ugain (gnc:make-gnc-monetary currency
                                                    (- (gnc:gnc-monetary-amount (my-exchange-fn value currency))
                                                       (sum-basis basis-list))))
                      (bothgain (gnc:make-gnc-monetary currency  (+ (gnc:gnc-monetary-amount gain)
                                                                    (gnc:gnc-monetary-amount ugain))))
                      (totalreturn (gnc:make-gnc-monetary currency (+ (gnc:gnc-monetary-amount bothgain)
                                                                      (gnc:gnc-monetary-amount income))))

                      (activecols (list (gnc:html-account-anchor current))))

                 ;; If we're using the txn, warn the user
                 (if use-txn
                     (if pricing-txn
                         (set! warn-price-dirty #t)
                         (set! warn-no-price #t)))

                 (total-value 'add (gnc:gnc-monetary-commodity value) (gnc:gnc-monetary-amount value))
                 (total-moneyin 'merge moneyincoll #f)
                 (total-moneyout 'merge moneyoutcoll #f)
                 (total-brokerage 'merge brokeragecoll #f)
                 (total-income 'merge dividendcoll #f)
                 (total-gain 'merge gaincoll #f)
                 (total-ugain 'add (gnc:gnc-monetary-commodity ugain) (gnc:gnc-monetary-amount ugain))
                 (total-basis 'add currency (sum-basis basis-list))

                 ;; build a list for the row  based on user selections
                 (if show-symbol (append! activecols (list (make-text-cell ticker-symbol))))
                 (if show-listing (append! activecols (list (make-text-cell listing))))
                 (if show-shares (append! activecols (list (make-number-cell (xaccPrintAmount units share-print-info)))))
                 (if show-price (append! activecols (list (make-number-cell
                                                           (if use-txn
                                                               (if pricing-txn
                                                                   (gnc:html-transaction-anchor pricing-txn price)
                                                                   price)
                                                               (gnc:html-price-anchor
                                                                price (gnc:default-price-renderer
                                                                       (gnc-price-get-currency price)
                                                                       (gnc-price-get-value price))))))))
                 (append! activecols (list (if use-txn (if pricing-txn "*" "**") " ")
                                           (make-number-cell (gnc:make-gnc-monetary currency (sum-basis basis-list)))
                                           (make-number-cell value)
                                           (make-number-cell moneyin)
                                           (make-number-cell moneyout)
                                           (make-number-cell gain)
                                           (make-number-cell ugain)
                                           (make-number-cell bothgain)
                                           (make-number-cell
                                            (let* ((moneyinvalue (gnc:gnc-monetary-amount moneyin))
                                                   (bothgainvalue (gnc:gnc-monetary-amount bothgain)))
                                              (if (zero? moneyinvalue)
                                                  ""
                                                  (format #f "~,2f%" (* 100 (/ bothgainvalue moneyinvalue))))))
                                           (make-number-cell income)))
                 (if (not (eq? handle-brokerage-fees 'ignore-brokerage))
                     (append! activecols (list (make-number-cell brokerage))))

                 (append! activecols
                          (list (make-number-cell totalreturn)
                                (make-number-cell
                                 (let* ((moneyinvalue (gnc:gnc-monetary-amount moneyin))
                                        (totalreturnvalue (gnc:gnc-monetary-amount totalreturn)))
                                   (if (zero? moneyinvalue)
                                       ""
                                       (format #f "~,2f%" (* 100 (/ totalreturnvalue moneyinvalue))))))))

                 (gnc:html-table-append-row/markup! table row-style activecols)

                 (if (and (not use-txn) price) (gnc-price-unref price))
                 (lp rest (not odd-row?)))

               (begin
                 (if (and (not use-txn) price) (gnc-price-unref price))
                 (lp rest odd-row?))))))))

  ;; Tell the user that we're starting.
  (gnc:report-starting reportname)

  ;; The first thing we do is make local variables for all the specific
  ;; options in the set of options given to the function. This set will
  ;; be generated by the options generator above.
  (let ((to-date     (gnc:time64-end-day-time
                      (gnc:date-option-absolute-time
                       (get-option gnc:pagename-general "Date"))))
        (accounts    (get-option gnc:pagename-accounts "Accounts"))
        (currency    (get-option gnc:pagename-general "Report's currency"))
        (price-source (get-option gnc:pagename-general optname-price-source))
        (report-title (get-option gnc:pagename-general gnc:optname-reportname))
        (include-empty (get-option gnc:pagename-accounts optname-zero-shares))
        (show-symbol (get-option gnc:pagename-display optname-show-symbol))
        (show-listing (get-option gnc:pagename-display optname-show-listing))
        (show-shares (get-option gnc:pagename-display optname-show-shares))
        (show-price (get-option gnc:pagename-display optname-show-price))
        (basis-method (get-option gnc:pagename-general optname-basis-method))
        (prefer-pricelist (get-option gnc:pagename-general optname-prefer-pricelist))
        (handle-brokerage-fees (get-option gnc:pagename-general optname-brokerage-fees))

        (total-basis (gnc:make-commodity-collector))
        (total-value    (gnc:make-commodity-collector))
        (total-moneyin  (gnc:make-commodity-collector))
        (total-moneyout (gnc:make-commodity-collector))
        (total-income   (gnc:make-commodity-collector))
        (total-gain     (gnc:make-commodity-collector)) ;; realized gain
        (total-ugain (gnc:make-commodity-collector))    ;; unrealized gain
        (total-brokerage (gnc:make-commodity-collector))
        ;;document will be the HTML document that we return.
        (table (gnc:make-html-table))
        (document (gnc:make-html-document)))

    (gnc:html-document-set-title!
     document (string-append
               report-title
               (format #f " ~a" (qof-print-date to-date))))

    (cond
     ((null? accounts)
      (gnc:html-document-add-object!
       document
       (gnc:html-make-no-account-warning
        report-title (gnc:report-id report-obj))))

     (else
      (let* ((exchange-fn (gnc:case-exchange-fn price-source currency to-date))
             (pricedb (gnc-pricedb-get-db (gnc-get-current-book)))
             (price-fn
              (case price-source
                ((pricedb-latest)
                 (lambda (foreign domestic date)
                   (find-price (gnc-pricedb-lookup-latest-any-currency pricedb foreign)
                               domestic)))
                ((pricedb-nearest)
                 (lambda (foreign domestic date)
                   (find-price (gnc-pricedb-lookup-nearest-in-time-any-currency-t64
                                pricedb foreign (time64CanonicalDayTime date)) domestic)))))
             (headercols (list (_ "Account")))
             (totalscols (list (gnc:make-html-table-cell/markup "total-label-cell" (_ "Total"))))
             (sum-total-moneyin 0)
             (sum-total-income 0)
             (sum-total-both-gains 0)
             (sum-total-gain 0)
             (sum-total-ugain 0)
             (sum-total-brokerage 0)
             (sum-total-totalreturn 0)) ;;end of let

        ;;begin building lists for which columns to display
        (when show-symbol
          (append! headercols (list (_ "Symbol")))
          (append! totalscols (list " ")))

        (when show-listing
          (append! headercols (list (_ "Listing")))
          (append! totalscols (list " ")))

        (when show-shares
          (append! headercols (list (_ "Shares")))
          (append! totalscols (list " ")))

        (when show-price
          (append! headercols (list (_ "Price")))
          (append! totalscols (list " ")))

        (append! headercols (list " "
                                  (_ "Basis")
                                  (_ "Value")
                                  (_ "Money In")
                                  (_ "Money Out")
                                  (_ "Realized Gain")
                                  (_ "Unrealized Gain")
                                  (_ "Total Gain")
                                  (_ "Rate of Gain")
                                  (_ "Income")))

        (if (not (eq? handle-brokerage-fees 'ignore-brokerage))
            (append! headercols (list (_ "Brokerage Fees"))))

        (append! headercols (list (_ "Total Return")
                                  (_ "Rate of Return")))

        (append! totalscols (list " "))

        (gnc:html-table-set-col-headers! table headercols)

        (catch 'div/0
          (lambda ()
            (table-add-stock-rows
             table accounts to-date currency price-fn exchange-fn price-source
             include-empty show-symbol show-listing show-shares show-price basis-method
             prefer-pricelist handle-brokerage-fees
             total-basis total-value total-moneyin total-moneyout
             total-income total-gain total-ugain total-brokerage))
          (lambda (k reason)
            (gnc:html-document-add-object!
             document (format #f OVERFLOW-ERROR reason))))


        (set! sum-total-moneyin (gnc:sum-collector-commodity total-moneyin currency exchange-fn))
        (set! sum-total-income (gnc:sum-collector-commodity total-income currency exchange-fn))
        (set! sum-total-gain (gnc:sum-collector-commodity total-gain currency exchange-fn))
        (set! sum-total-ugain (gnc:sum-collector-commodity total-ugain currency exchange-fn))
        (set! sum-total-both-gains (gnc:make-gnc-monetary currency (+ (gnc:gnc-monetary-amount sum-total-gain)
                                                                      (gnc:gnc-monetary-amount sum-total-ugain))))
        (set! sum-total-brokerage (gnc:sum-collector-commodity total-brokerage currency exchange-fn))
        (set! sum-total-totalreturn (gnc:make-gnc-monetary currency (+ (gnc:gnc-monetary-amount sum-total-both-gains)
                                                                       (gnc:gnc-monetary-amount sum-total-income))))

        (gnc:html-table-append-row/markup!
         table "grand-total"
         (list (gnc:make-html-table-cell/size
                1 17 (gnc:make-html-text (gnc:html-markup-hr)))))

        ;; finish building the totals columns, now that totals are complete
        (append! totalscols (list
                             (make-total-number-cell (gnc:sum-collector-commodity total-basis currency exchange-fn))
                             (make-total-number-cell (gnc:sum-collector-commodity total-value currency exchange-fn))
                             (make-total-number-cell sum-total-moneyin)
                             (make-total-number-cell (gnc:sum-collector-commodity total-moneyout currency exchange-fn))
                             (make-total-number-cell sum-total-gain)
                             (make-total-number-cell sum-total-ugain)
                             (make-total-number-cell sum-total-both-gains)
                             (make-total-number-cell
                              (let* ((totalinvalue (gnc:gnc-monetary-amount sum-total-moneyin))
                                     (totalgainvalue (gnc:gnc-monetary-amount sum-total-both-gains)))
                                (if (zero? totalinvalue)
                                    ""
                                    (format #f "~,2f%" (* 100 (/ totalgainvalue totalinvalue))))))
                             (make-total-number-cell sum-total-income)))

        (unless (eq? handle-brokerage-fees 'ignore-brokerage)
          (append! totalscols (list (make-total-number-cell sum-total-brokerage))))

        (append! totalscols (list
                             (make-total-number-cell sum-total-totalreturn)
                             (make-total-number-cell
                              (let* ((totalinvalue (gnc:gnc-monetary-amount sum-total-moneyin))
                                     (totalreturnvalue (gnc:gnc-monetary-amount sum-total-totalreturn)))
                                (if (zero? totalinvalue)
                                    ""
                                    (format #f "~,2f%" (* 100 (/ totalreturnvalue totalinvalue))))))))

        (gnc:html-table-append-row/markup! table "grand-total" totalscols )

        (gnc:html-document-add-object! document table)
        (when warn-price-dirty
          (gnc:html-document-append-objects!
           document
           (list
            (gnc:make-html-text
             (_ "* this commodity data was built using transaction pricing instead of the price list.")
             (gnc:html-markup-br)
             (_ "If you are in a multi-currency situation, the exchanges may not be correct.")))))

        (when warn-no-price
          (gnc:html-document-append-objects!
           document
           (list
            (gnc:make-html-text
             (if warn-price-dirty (gnc:html-markup-br) "")
             (_ "** this commodity has no price and a price of 1 has been used."))))))))

    (gnc:report-finished)
    document)))

(gnc:define-report
 'version 1
 'report-guid "21d7cfc59fc74f22887596ebde7e462d"
 'name reportname
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator options-generator
 'renderer advanced-portfolio-renderer)
