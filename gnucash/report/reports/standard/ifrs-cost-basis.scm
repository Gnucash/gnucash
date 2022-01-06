;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define-module (gnucash reports standard ifrs-cost-basis))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (ice-9 match))
(use-modules (gnucash utilities))
(use-modules (gnucash report))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))

(define disclaimer
  (gnc:make-html-text
   (gnc:html-markup-p "This report is designed for cost basis
accumulation and capital gain/loss reporting using the weighted
average cost basis method, which is most consistent with typical
accounting frameworks (US GAAP, IFRS, etc.).  This report allows for
for the choice to capitalize (most consistent with typical accounting
frameworks) vs expense (used by some taxing jurisdictions) commissions
paid on purchase.")
   (gnc:html-markup-p "This report is not appropriate for FIFO, LIFO, or
specific-identification methods for cost basis accumulation and
capital gain/loss reporting.  This report may not be appropriate for
tax purposes, if the taxing jurisdiction requires a method other than
the weighted average cost basis method.")
   (gnc:html-markup-p "This report is not designed with options
reporting in mind.  If your activity involves options and/or futures
that are purchased, written, and/or exercised, there is no guarantee
that this report will accurately portray this options activity.")))

(define reportname "IFRS weighted-average cost basis report")

(define optname-startdate (N_ "Start Date"))
(define optname-enddate (N_ "End Date"))

(define optname-stock-acct "Stock Account")
(define optname-proceeds-acct "Proceeds Account")
(define optname-dividend-acct "Dividend Account")
(define optname-capgains-acct "Cap Gains Account")
(define optname-fees-acct "Fees Account")
(define optname-report-currency "Report's currency")

(define optname-format-cells "Format monetary cells")
(define opthelp-format-cells "Check this option to show cells with currency")

(define optname-format-short "Alternative row-style for shorts")
(define opthelp-format-short "Check this option to use alternate style \
for shorts. Disable to use alternate style every other row")

(define optname-cap-purch-costs "Capitalise purchase commissions")
(define opthelp-cap-purch-costs "Check this option to capitalise purchase \
commissions in cumulative average cost and gain/loss after commission")

(define optname-cap-fee-action "Action field filter for fees")
(define opthelp-cap-fee-action "This string will be used to compare with \
the split action field to detect capitalized fees on stock activity")

(define (options-generator)
  (let ((options (gnc:new-options)))

    (define (add-option new-option)
      (gnc:register-option options new-option))

    (gnc:options-add-date-interval!
     options gnc:pagename-general optname-startdate optname-enddate " ")

    (gnc:options-add-currency!
     options gnc:pagename-general optname-report-currency "a")

    (add-option
     (gnc:make-account-sel-limited-option
      gnc:pagename-general optname-stock-acct "b" "Stock Account"
      #f #f (list ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)))

    (add-option
     (gnc:make-account-sel-limited-option
      gnc:pagename-general optname-proceeds-acct "c" "Proceeds Account"
      #f #f (list ACCT-TYPE-ASSET ACCT-TYPE-BANK)))

    (add-option
     (gnc:make-account-sel-limited-option
      gnc:pagename-general optname-dividend-acct "c" "Dividend Account"
      #f #f (list ACCT-TYPE-INCOME)))

    (add-option
     (gnc:make-account-sel-limited-option
      gnc:pagename-general optname-capgains-acct "d" "Cap Gains Account"
      #f #f (list ACCT-TYPE-INCOME)))

    (add-option
     (gnc:make-account-sel-limited-option
      gnc:pagename-general optname-fees-acct "c5" "Fees Account"
      #f #f (list ACCT-TYPE-EXPENSE)))

    (add-option
     (gnc:make-string-option
      gnc:pagename-general optname-cap-fee-action "d5" opthelp-cap-fee-action "Fee"))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-format-cells "e" opthelp-format-cells #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-format-short "f" opthelp-format-short #t))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-cap-purch-costs "g" opthelp-cap-purch-costs #t))

    options))

(define M+
  (case-lambda
    (() (error "M+ needs at least 1 arg"))
    ((a b) (if a (if b (+ a b) a) b))
    ((head . tail) (fold M+ head tail))))

(define M-abs
  (case-lambda
    (() (error "M-abs needs 1 arg"))
    ((a) (and a (abs a)))))

(define M*
  (case-lambda
    (() (error "M* needs at least 1 arg"))
    ((a b) (and a b (* a b)))
    ((head . tail) (fold M* head tail))))

(define M-
  (case-lambda
    (() (error "M- needs at least 1 arg"))
    ((n) (and n (- n)))
    ((minuend head . tail) (M+ minuend (M- (fold M+ head tail))))))

(define M/
  (case-lambda
    (() (error "M/ needs at least 1 arg"))
    ((n) (and n (not (zero? n)) (/ n)))
    ((divisor head . tail) (M* divisor (M/ (fold M* head tail))))))

(define-record-type :txn-info
  (make-txn-info stock-amt stock-val proceeds-val
                 fees-cap-val fees-exp-val dividend-val capgains-val)
  txn-info?
  (stock-amt get-stock-amt set-stock-amt!)
  (stock-val get-stock-val set-stock-val!)
  (proceeds-val get-proceeds-val set-proceeds-val!)
  (fees-cap-val get-fees-cap-val set-fees-cap-val!)
  (fees-exp-val get-fees-exp-val set-fees-exp-val!)
  (dividend-val get-dividend-val set-dividend-val!)
  (capgains-val get-capgains-val set-capgains-val!))

;; "bitfield" Nabc a=neg b=zero c=pos
(define (N001 x) (if (number? x) (>  x 0) #f))
(define (N100 x) (if (number? x) (<  x 0) #f))
(define (N010 x) (if (number? x) (=  x 0) #t))
(define (N011 x) (if (number? x) (>= x 0) #t))
(define (N110 x) (if (number? x) (<= x 0) #t))
(define (N111 x) #t)
;; N000 should be (not x) however we can accept a zero-amount split too
(define (N000 x) (if (number? x) (=  x 0) #t))

;;       --stock-- cash cap  exp  divi capg
;;       amt  val       fees fees

(define open-types
  (list
   (list N001 N001 N100 N011 N000 N000 N000 "Open Long")
   (list N100 N100 N001 N011 N000 N000 N000 "Open Short")))

(define long-types
  (list
   (list N001 N001 N100 N011 N000 N000 N000 "Buy")
   (list N100 N100 N011 N000 N011 N000 N111 "Sell")
   (list N000 N000 N001 N000 N011 N100 N000 "Dividend")
   (list N001 N001 N001 N011 N000 N100 N000 "Dividend reinvestment (w/ remainder)")
   (list N001 N001 N000 N011 N000 N100 N000 "Dividend reinvestment (w/o remainder)")
   (list N000 N100 N001 N011 N000 N000 N000 "Return of Capital")
   (list N000 N001 N000 N000 N011 N100 N000 "Notional distribution")
   (list N001 N000 N000 N011 N000 N000 N000 "Stock split")
   (list N100 N000 N000 N011 N000 N000 N000 "Reverse split")
   (list N100 N100 N001 N000 N011 N000 N111 "Reverse split w/ cash in lieu for fractionals")))

(define short-types
  (list
   (list N100 N100 N001 N011 N000 N000 N000 "Short Sell")
   (list N001 N001 N110 N000 N011 N000 N111 "Cover Buy")
   (list N000 N000 N100 N000 N011 N001 N000 "Compensatory dividend")
   (list N000 N000 N000 N011 N000 N000 N000 "Dividend reinvestment (w remainder)")
   (list N000 N000 N000 N011 N000 N000 N000 "Dividend reinvestment (w/o remainder)")
   (list N000 N001 N100 N011 N000 N000 N000 "Compensatory return of capital")
   (list N000 N100 N000 N000 N011 N001 N000 "Compensatory notional distribution")
   (list N100 N000 N000 N011 N000 N000 N000 "Stock split")
   (list N001 N000 N000 N011 N000 N000 N000 "Reverse split")
   (list N001 N001 N100 N000 N011 N000 N111 "Reverse split w/ cash in lieu for fractionals")))

(define (cmp amt neg zero pos)
  (cond ((< amt 0) neg)
        ((= amt 0) zero)
        (else pos)))

(define shown-headers? #f)
(define (txn-identify trans txn-info cumul-units)
  (let lp ((types (cmp cumul-units short-types open-types long-types)))
    (match types
      (()
       ;; (gnc:pk (qof-print-date (xaccTransGetDate trans)) txn-info)
       "Unknown")
      (((amt-fn val-fn proc-fn fee-cap-fn fee-exp-fn div-fn capg-fn res) . tail)
       (if (and (amt-fn (get-stock-amt txn-info))
                (val-fn (get-stock-val txn-info))
                (proc-fn (get-proceeds-val txn-info))
                (fee-cap-fn (get-fees-cap-val txn-info))
                (fee-exp-fn (get-fees-exp-val txn-info))
                (div-fn (get-dividend-val txn-info))
                (capg-fn (get-capgains-val txn-info)))
           res
           (lp tail))))))

(define (txn->info txn stock-acct cap-fee-action
                   proceeds-acct capgains-acct expenses-acct dividend-acct)
  (define (from-acct? acct)
    (lambda (split)
      (equal? (xaccSplitGetAccount split) acct)))
  (define (cap-expenses? split)
    (and ((from-acct? stock-acct) split)
         (equal? (gnc-get-action-num txn split) cap-fee-action)))
  (let lp ((splits (xaccTransGetSplitList txn))
           (stock-amt #f)
           (stock-val #f)
           (proceeds-val #f)
           (fees-cap-val #f)
           (fees-exp-val #f)
           (dividend-val #f)
           (capgains-val #f))
    (match splits
      (() (make-txn-info stock-amt stock-val proceeds-val fees-cap-val
                         fees-exp-val dividend-val capgains-val))

      (((? (from-acct? proceeds-acct) split) . rest)
       (lp rest stock-amt stock-val
           (M+ proceeds-val (xaccSplitGetAmount split))
           fees-cap-val fees-exp-val dividend-val capgains-val))

      (((? (from-acct? capgains-acct) split) . rest)
       (lp rest stock-amt stock-val proceeds-val fees-cap-val fees-exp-val dividend-val
           (M+ capgains-val (xaccSplitGetAmount split))))

      (((? (from-acct? expenses-acct) split) . rest)
       (lp rest stock-amt stock-val proceeds-val fees-cap-val
           (M+ fees-exp-val (xaccSplitGetAmount split))
           dividend-val capgains-val))

      (((? (from-acct? dividend-acct) split) . rest)
       (lp rest stock-amt stock-val proceeds-val fees-cap-val fees-exp-val
           (M+ dividend-val (xaccSplitGetAmount split))
           capgains-val))

      ;; testing capitalized fees must take place *before* processing
      ;; stock amt/val because it belongs to the stock account.
      (((? cap-expenses? split) . rest)
       (lp rest stock-amt stock-val proceeds-val
           (M+ fees-cap-val (xaccSplitGetValue split))
           fees-exp-val dividend-val capgains-val))

      (((? (from-acct? stock-acct) split) . rest)
       (lp rest
           (M+ stock-amt (xaccSplitGetAmount split))
           (M+ stock-val (xaccSplitGetValue split))
           proceeds-val fees-cap-val fees-exp-val dividend-val capgains-val))

      ((_ . rest)
       (lp rest stock-amt stock-val proceeds-val fees-cap-val fees-exp-val
           dividend-val capgains-val)))))

(define (ifrs-cost-basis-renderer report-obj)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (define opt-startdate (opt-val gnc:pagename-general optname-startdate))
  (define opt-enddate   (opt-val gnc:pagename-general optname-enddate))
  (define startdate
    (gnc:time64-start-day-time
     (gnc:date-option-absolute-time opt-startdate)))
  (define enddate
    (gnc:time64-start-day-time
     (gnc:date-option-absolute-time opt-enddate)))
  (define stock-acct   (opt-val gnc:pagename-general optname-stock-acct))
  (define proceeds-acct (opt-val gnc:pagename-general optname-proceeds-acct))
  (define dividend-acct (opt-val gnc:pagename-general optname-dividend-acct))
  (define capgains-acct (opt-val gnc:pagename-general optname-capgains-acct))
  (define fees-acct (opt-val gnc:pagename-general optname-fees-acct))
  (define report-currency (opt-val gnc:pagename-general optname-report-currency))
  (define format-cells (opt-val gnc:pagename-general optname-format-cells))
  (define short-alternate-format? (opt-val gnc:pagename-general optname-format-short))
  (define cap-purch-costs? (opt-val gnc:pagename-general optname-cap-purch-costs))
  (define cap-fee-action (opt-val gnc:pagename-general optname-cap-fee-action))
  (define document (gnc:make-html-document))

  (define large 10000000)
  (define (get-fx db from to time)
    (/ (gnc-pricedb-convert-balance-nearest-price-t64 db large from to time)
       large))

  (define (to-cell elt)
    (gnc:make-html-table-cell/markup "number-cell" elt))

  (gnc:html-document-set-title! document "IFRS weighted average cost basis Report")

  (cond
   ((null? stock-acct)
    (gnc:html-document-add-object!
     document (gnc:html-make-generic-options-warning
               reportname (gnc:report-id report-obj))))

   (else
    (let ((commodity (xaccAccountGetCommodity stock-acct))
          (currency (gnc-account-get-currency-or-parent stock-acct))
          (pricedb (gnc-pricedb-get-db (gnc-get-current-book)))
          (splits
           (let ((query (qof-query-create-for-splits)))
             (qof-query-set-book query (gnc-get-current-book))
             (xaccQueryAddSingleAccountMatch query stock-acct QOF-QUERY-AND)
             (let ((result (xaccQueryGetSplitsUniqueTrans query)))
               (qof-query-destroy query)
               result))))

      (define (to-commodity amt)
        (if format-cells
            (and amt (gnc:make-gnc-monetary commodity amt))
            amt))

      (define (to-orig-currency amt)
        (if format-cells
            (and amt (gnc:make-gnc-monetary currency amt))
            amt))

      (define (to-report-currency amt)
        (if format-cells
            (and amt (gnc:make-gnc-monetary report-currency amt))
            amt))

      (define table (gnc:make-html-table))

      (gnc:html-document-set-title!
       document
       (gnc:format "Average-Cost (Basis) Report: From ${startdate} to ${enddate}. Report-currency ${currency}"
                   'startdate (qof-print-date startdate)
                   'enddate (qof-print-date enddate)
                   'currency (gnc-commodity-get-mnemonic report-currency)))

      (gnc:html-table-set-col-headers!
       table (list "date" "description" "trans-units" "cumul-units" "note"
                   "curr" "fx" "purchase-val" "purchase-cost" "cash-dividends"
                   "proceeds-val" "proceeds-cost" "conv-purchase-val"
                   "conv-purchase-cost" "conv-dividends"
                   "conv-proceeds-val" "conv-proceeds-cost"
                   "cumulative-average-cost-basis"
                   "average-cost-basis/unit-for-sale" "average-cost-basis-of-sale"
                   "net-proceeds" "gain-post-commission" "gain-pre-commission"
                   "cumul-gross-profit" "cumul-net-profit" "cumul-tot-return"))

      (let lp ((splits splits)
               (odd-row? #t)
               (cumul-units 0)
               (cumul-average-cost-basis 0)
               (cumul-gross-profit 0)
               (cumul-net-profit 0)
               (cumul-tot-return 0))

        (match splits
          (() (gnc:html-document-add-object! document table))

          ((split . rest-splits)
           (let* ((trans (xaccSplitGetParent split))
                  (txn-info (txn->info trans stock-acct cap-fee-action proceeds-acct
                                       capgains-acct fees-acct dividend-acct))
                  (trans-units (get-stock-amt txn-info))
                  (cash-value (get-proceeds-val txn-info))
                  (dividends-val (get-dividend-val txn-info))
                  (capgains-val (get-capgains-val txn-info))
                  (fees-expense (get-fees-exp-val txn-info))
                  (fees-value (M+ (get-fees-cap-val txn-info) fees-expense))
                  (trans-value (M+ (get-stock-val txn-info)
                                   (get-fees-cap-val txn-info)))
                  (new-units (M+ cumul-units trans-units))

                  (sale?
                   (cond
                    ((< trans-units 0) (<= 0 new-units))
                    ((> trans-units 0) (<= new-units 0))
                    (else #f)))

                  (purchase?
                   (cond
                    ((= trans-value 0) dividends-val)        ;dividends
                    ((= trans-units 0) cash-value)           ;return of capital
                    ((> trans-units 0) (< 0 new-units))      ;regular buy
                    ((< trans-units 0) (< new-units 0))))    ;buy during short

                  (shorting? (or (< new-units 0)
                                 (and (= new-units 0) (< 0 trans-units))))

                  (purchase-cost (and purchase? fees-value))
                  (purchase-val (and purchase? (M- trans-value purchase-cost)))
                  (cash-dividends (M- dividends-val))
                  (proceeds-cost (and sale? fees-value))
                  (proceeds-value (and sale? (M+ cash-value proceeds-cost)))

                  ;; now convert to report-currency
                  (fx (get-fx pricedb currency report-currency
                              (time64CanonicalDayTime (xaccTransGetDate trans))))
                  (conv-purchase-val (M* fx purchase-val))
                  (conv-purchase-cost (M* fx purchase-cost))
                  (conv-dividends (M* fx cash-dividends))
                  (conv-proceeds-value (M* fx proceeds-value))
                  (conv-proceeds-cost (M* fx proceeds-cost))

                  ;; now perform AVERAGE-COST-BASIS calculations
                  (average-cost-basis/unit-for-sale
                   (M-abs (M/ cumul-average-cost-basis cumul-units)))
                  (average-cost-basis-of-sale
                   (and proceeds-value (M* average-cost-basis/unit-for-sale
                                         trans-units)))
                  (cumul-average-cost-basis
                   (M+ cumul-average-cost-basis
                       conv-purchase-val
                       (and cap-purch-costs? conv-purchase-cost)
                       average-cost-basis-of-sale))

                  (net-proceeds (M- conv-proceeds-value conv-proceeds-cost))
                  (gain-post-commission (M+ net-proceeds average-cost-basis-of-sale
                                            (and (not cap-purch-costs?)
                                                 conv-purchase-cost)))
                  (gain-pre-commission (M+ conv-proceeds-value
                                           average-cost-basis-of-sale))

                  (new-gross-profit (M+ cumul-gross-profit gain-pre-commission))
                  (new-net-profit (M+ cumul-net-profit gain-post-commission))
                  (new-tot-return (M+ cumul-tot-return gain-post-commission
                                      conv-dividends)))

             ;; (gnc:pk trans 'trans-units trans-units 'trans-value trans-value
             ;;         'cumul-units cumul-units 'proceeds-value proceeds-value
             ;;         'sale? sale? 'purchase? purchase?)
             (cond
              ((not (< startdate (xaccTransGetDate (xaccSplitGetParent (car splits)))
                       enddate))
               (lp rest-splits
                   odd-row?
                   new-units
                   cumul-average-cost-basis
                   new-gross-profit
                   new-net-profit
                   new-tot-return))

              (else
               (gnc:html-table-append-row/markup!
                table (if short-alternate-format?
                          (if shorting? "alternate-row" "normal-row")
                          (if odd-row? "normal-row" "alternate-row"))
                (list (qof-print-date (xaccTransGetDate trans))
                      (gnc:html-string-sanitize (xaccTransGetDescription trans))
                      (to-cell (gnc:html-split-anchor split (to-commodity trans-units)))
                      (to-cell (to-commodity new-units))
                      (cond
                       ((< new-units 0 cumul-units) "ERROR: long→short")
                       ((< cumul-units 0 new-units) "ERROR: short→long")
                       (else (txn-identify trans txn-info cumul-units)))
                      (gnc-commodity-get-mnemonic currency)
                      (to-cell (gnc:default-price-renderer report-currency fx))
                      (to-cell (to-orig-currency purchase-val))
                      (to-cell (to-orig-currency purchase-cost))
                      (to-cell (to-orig-currency cash-dividends))
                      (to-cell (to-orig-currency proceeds-value))
                      (to-cell (to-orig-currency proceeds-cost))
                      (to-cell (to-report-currency conv-purchase-val))
                      (to-cell (to-report-currency conv-purchase-cost))
                      (to-cell (to-report-currency conv-dividends))
                      (to-cell (to-report-currency conv-proceeds-value))
                      (to-cell (to-report-currency conv-proceeds-cost))
                      (to-cell (to-report-currency cumul-average-cost-basis))
                      (to-cell (to-report-currency average-cost-basis/unit-for-sale))
                      (to-cell (to-report-currency (M- average-cost-basis-of-sale)))
                      (to-cell (to-report-currency net-proceeds))
                      (to-cell (to-report-currency gain-post-commission))
                      (to-cell (to-report-currency gain-pre-commission))
                      (to-cell (to-report-currency new-gross-profit))
                      (to-cell (to-report-currency new-net-profit))
                      (to-cell (to-report-currency new-tot-return))))

               (lp rest-splits
                   (not odd-row?)
                   new-units
                   cumul-average-cost-basis
                   new-gross-profit
                   new-net-profit
                   new-tot-return))))))))))

  ;; (gnc:dump-all-transactions)
  (gnc:html-document-add-object! document disclaimer)
  document)


;; Here we define the actual report
(gnc:define-report
 'version 1
 'name reportname
 'report-guid "15d5b744176c4625a703720338725291"
 'menu-path (list gnc:menuname-experimental)
 'options-generator options-generator
 'renderer ifrs-cost-basis-renderer)
