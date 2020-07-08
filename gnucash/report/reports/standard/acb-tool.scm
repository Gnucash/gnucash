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

(define-module (gnucash reports standard acb-tool))

(use-modules (srfi srfi-1))
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

(define reportname (N_ "ACB Tool"))
(define optname-stock-acct "Stock Account")
(define optname-proceeds-acct "Proceeds Account")
(define optname-dividend-acct "Dividend Account")
(define optname-capgains-acct "Cap Gains Account")
;; (define optname-fees-acct "Fees Account")
(define optname-report-currency "Report's currency")

(define optname-format-cells "Format monetary cells")
(define opthelp-format-cells "Check this option to show cells with currency")

(define optname-format-short "Alternative row-style for shorts")
(define opthelp-format-short "Check this option to use alternate style \
for shorts. Disable to use alternate style every other row")

(define optname-cap-purch-costs "Capitalise purchase commissions")
(define opthelp-cap-purch-costs "Check this option to capitalise purchase \
commissions in cumulative ACB and gain/loss after commission")

(define (options-generator)
  (let ((options (gnc:new-options)))

    (define (add-option new-option)
      (gnc:register-option options new-option))

    (gnc:options-add-currency!
     options gnc:pagename-general optname-report-currency "a")

    (add-option
     (gnc:make-account-sel-limited-option
      gnc:pagename-general optname-stock-acct "b" (N_ "Stock Account")
      #f #f (list ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)))

    (add-option
     (gnc:make-account-sel-limited-option
      gnc:pagename-general optname-proceeds-acct "c" (N_ "Proceeds Account")
      #f #f (list ACCT-TYPE-ASSET ACCT-TYPE-BANK)))

    (add-option
     (gnc:make-account-sel-limited-option
      gnc:pagename-general optname-dividend-acct "c" (N_ "Dividend Account")
      #f #f (list ACCT-TYPE-INCOME)))

    (add-option
     (gnc:make-account-sel-limited-option
      gnc:pagename-general optname-capgains-acct "d" (N_ "Cap Gains Account")
      #f #f (list ACCT-TYPE-INCOME)))

    ;; (add-option
    ;;  (gnc:make-account-sel-limited-option
    ;;   gnc:pagename-general optname-fees-acct "c" (N_ "Fees Account")
    ;;   #f #f (list ACCT-TYPE-EXPENSE)))

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

(define (trans-extract trans account numfilter split->amount)
  (define (not-account? s)
    (and account (not (equal? (xaccSplitGetAccount s) account))))
  (define (not-num-filter? s)
    (and numfilter
         (not (equal? (gnc-get-action-num (xaccSplitGetParent s) s) numfilter))))
  (let lp ((splits (xaccTransGetSplitList trans)) (result #f))
    (match splits
      (() result)
      (((? not-account?) . rest) (lp rest result))
      (((? not-num-filter?) . rest) (lp rest result))
      ((split . rest) (lp rest (M+ (split->amount split) result))))))

(define (trans-extract-value trans account numfilter)
  (trans-extract trans account numfilter xaccSplitGetValue))

(define (trans-extract-amount trans account numfilter)
  (trans-extract trans account numfilter xaccSplitGetAmount))

(define (acb-renderer report-obj)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (define stock-acct   (opt-val gnc:pagename-general optname-stock-acct))
  (define proceeds-acct (opt-val gnc:pagename-general optname-proceeds-acct))
  (define dividend-acct (opt-val gnc:pagename-general optname-dividend-acct))
  (define capgains-acct (opt-val gnc:pagename-general optname-capgains-acct))
  ;; (define fees-acct (opt-val gnc:pagename-general optname-fees-acct))
  (define report-currency (opt-val gnc:pagename-general optname-report-currency))
  (define format-cells (opt-val gnc:pagename-general optname-format-cells))
  (define short-alternate-format? (opt-val gnc:pagename-general optname-format-short))
  (define cap-purch-costs? (opt-val gnc:pagename-general optname-cap-purch-costs))
  (define document (gnc:make-html-document))

  (define (elt->cell split)
    (gnc:html-markup-anchor
     (gnc:split-anchor-text split)
     (amount->monetary (xaccSplitGetAmount split))))

  (define large 10000000)
  (define (get-fx db from to time)
    (/ (gnc-pricedb-convert-balance-nearest-price-t64 db large from to time)
       large))

  (define (stock-split prev delta)
    (let ((exact (/ (+ delta prev) prev)))
      (format #f "~a:~a Split" (numerator exact) (denominator exact))))

  (define (to-cell elt)
    (gnc:make-html-table-cell/markup "number-cell" elt))

  (define (cmp amt neg zero pos)
    (cond ((< amt 0) neg)
          ((= amt 0) zero)
          (else pos)))

  (gnc:html-document-set-title! document "ACB Tool")

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
             (xaccQueryGetSplitsUniqueTrans query))))

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
       (format #f "ACB Tool: report-currency ~a"
               (gnc-commodity-get-mnemonic report-currency)))

      (gnc:html-table-set-col-headers!
       table (list "date" "description" "trans-units" "cumul-units" "note"
                   "curr" "fx" "purchase-val" "purchase-cost" "cash-dividends"
                   "proceeds-val" "proceeds-cost" "conv-purchase-val"
                   "conv-purchase-cost" "conv-dividends"
                   "conv-proceeds-val" "conv-proceeds-cost"
                   "acb/unit-for-sale" "acb-of-sale" "cumulative-acb"
                   "gain-post-commission" "gain-pre-commission" "net-proceeds"
                   "cumul-gross-profit" "cumul-net-profit" "cumul-tot-return"))

      (let lp ((splits splits)
               (odd-row? #t)
               (cumul-units 0)
               (cumul-acb 0)
               (cumul-gross-profit 0)
               (cumul-net-profit 0)
               (cumul-tot-return 0))

        (match splits
          (() (gnc:html-document-add-object! document table))

          ((split . rest-splits)
           (let* ((trans (xaccSplitGetParent split))
                  (trans-units (trans-extract-amount trans stock-acct #f))
                  (trans-value (trans-extract-value trans stock-acct #f))
                  (proceeds-val (trans-extract-value trans proceeds-acct #f))
                  (dividends-val (trans-extract-value trans dividend-acct #f))
                  (capgains-val (trans-extract-value trans capgains-acct #f))
                  (fees-value (trans-extract-value trans #f "Fee"))
                  (new-units (M+ cumul-units trans-units))

                  (sale?
                   (cond
                    ((< trans-units 0) (<= 0 new-units))
                    ((> trans-units 0) (<= new-units 0))
                    (else #f)))

                  (purchase?
                   (cond
                    ((= trans-value 0) dividends-val)        ;dividends
                    ((= trans-units 0) proceeds-val)         ;return of capital
                    ((> trans-units 0) (< 0 new-units))      ;regular buy
                    ((< trans-units 0) (< new-units 0))))    ;buy during short

                  (shorting? (or (< new-units 0)
                                 (and (= new-units 0) (< 0 trans-units))))

                  (purchase-cost (and purchase? fees-value))
                  (purchase-val (and purchase? (M- trans-value purchase-cost)))
                  (cash-dividends (M- dividends-val))
                  (proceeds-cost (and sale? fees-value))
                  (proceeds-val (and sale? (M+ proceeds-val proceeds-cost)))

                  ;; now convert to report-currency
                  (fx (get-fx pricedb currency report-currency
                              (time64CanonicalDayTime (xaccTransGetDate trans))))
                  (conv-purchase-val (M* fx purchase-val))
                  (conv-purchase-cost (M* fx purchase-cost))
                  (conv-dividends (M* fx cash-dividends))
                  (conv-proceeds-val (M* fx proceeds-val))
                  (conv-proceeds-cost (M* fx proceeds-cost))

                  ;; now perform ACB calculations
                  (acb/unit-for-sale (M-abs (M/ cumul-acb cumul-units)))
                  (acb-of-sale (and proceeds-val (M* acb/unit-for-sale trans-units)))
                  (cumul-acb (M+ cumul-acb
                                 conv-purchase-val
                                 (and cap-purch-costs? conv-purchase-cost)
                                 acb-of-sale))

                  (net-proceeds (M- conv-proceeds-val conv-proceeds-cost))
                  (gain-post-commission (M+ net-proceeds acb-of-sale
                                            (and (not cap-purch-costs?)
                                                 conv-purchase-cost)))
                  (gain-pre-commission (M+ conv-proceeds-val acb-of-sale))

                  (new-gross-profit (M+ cumul-gross-profit gain-pre-commission))
                  (new-net-profit (M+ cumul-net-profit gain-post-commission))
                  (new-tot-return (M+ cumul-tot-return gain-post-commission
                                      conv-dividends)))

             ;; (gnc:pk trans 'trans-units trans-units 'trans-value trans-value
             ;;         'cumul-units cumul-units 'proceeds-val proceeds-val
             ;;         'sale? sale? 'purchase? purchase?)

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
                     ((= 0 cumul-units) (cmp new-units "Open Short" "1" "Open Long"))
                     ((= 0 new-units) (cmp trans-units "Close Long" "2" "Close Short"))
                     ((= 0 trans-units trans-value)
                      (cmp cumul-units "Compensatory Dividend" "7" "Dividend"))
                     ((= 0 trans-units)
                      (cond (cash-dividends
                             (cmp cumul-units
                                  "Compensatory Notional Distribution"
                                  "7"
                                  "Notional Distribution"))
                            (purchase-val
                             (cmp cumul-units
                                  "Compensatory Return Capital"
                                  "8"
                                  "Return Capital"))
                            (else "3")))
                     ((= 0 trans-value) (stock-split cumul-units trans-units))
                     (purchase-val (cmp purchase-val "Short Sell" "5" "Buy"))
                     (proceeds-val (cmp proceeds-val "Short Buy" "6" "Sell"))
                     (else "4"))
                    (gnc-commodity-get-mnemonic currency)
                    (to-cell (gnc:default-price-renderer report-currency fx))
                    (to-cell (to-orig-currency purchase-val))
                    (to-cell (to-orig-currency purchase-cost))
                    (to-cell (to-orig-currency cash-dividends))
                    (to-cell (to-orig-currency proceeds-val))
                    (to-cell (to-orig-currency proceeds-cost))
                    (to-cell (to-report-currency conv-purchase-val))
                    (to-cell (to-report-currency conv-purchase-cost))
                    (to-cell (to-report-currency conv-dividends))
                    (to-cell (to-report-currency conv-proceeds-val))
                    (to-cell (to-report-currency conv-proceeds-cost))
                    (to-cell (to-report-currency acb/unit-for-sale))
                    (to-cell (to-report-currency (M- acb-of-sale)))
                    (to-cell (to-report-currency cumul-acb))
                    (to-cell (to-report-currency gain-post-commission))
                    (to-cell (to-report-currency gain-pre-commission))
                    (to-cell (to-report-currency net-proceeds))
                    (to-cell (to-report-currency new-gross-profit))
                    (to-cell (to-report-currency new-net-profit))
                    (to-cell (to-report-currency new-tot-return))))

             (lp rest-splits
                 (not odd-row?)
                 new-units
                 cumul-acb
                 new-gross-profit
                 new-net-profit
                 new-tot-return))))))))

  ;; (gnc:dump-all-transactions)
  (gnc:html-document-add-object! document disclaimer)
  document)


;; Here we define the actual report
(gnc:define-report
 'version 1
 'name reportname
 'report-guid "acb-tool"
 'menu-path (list gnc:menuname-experimental)
 'options-generator options-generator
 'renderer acb-renderer)
