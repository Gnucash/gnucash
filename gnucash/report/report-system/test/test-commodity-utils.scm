;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-commodity-utilities.scm: Test the commodity functions
;; Copyright 2018 John Ralls <jralls@ceridwen.us>
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


(use-modules (srfi srfi-64))
(use-modules (ice-9 pretty-print))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))
(use-modules (sw_engine))
(use-modules (sw_app_utils))
(use-modules (gnucash report report-system))
(use-modules (system vm coverage))

(setlocale LC_ALL "C")

(define (run-test-proper)
  (test-runner-factory gnc:test-runner)
  (test-begin "commodity-utils")
  ;; Tests go here
  (test-setup)
  (test-resolve-unknown-comm)
  (test-get-exchange-totals)
  (test-get-exchange-cost-totals)
  (test-get-exchange-cost-totals-trading)
  (test-exchange-by-pricedb-latest)
  (test-exchange-by-pricedb-nearest)
  (test-get-commodity-totalavg-prices)
  (test-get-commodity-inst-prices)
  (test-weighted-average)
  (test-end "commodity-utils"))

(define (coverage-test)
  (let* ((currfile (dirname (current-filename)))
         (path (string-take currfile (string-rindex currfile #\/))))
    (add-to-load-path path))
  (call-with-values
      (lambda()
        (with-code-coverage run-test-proper))
    (lambda (data result)
      (let ((port (open-output-file "/tmp/lcov.info")))
        (coverage-data->lcov data port)
        (close port)))))

(define (run-test)
  (if #f                                ;switch to #t to run coverage
      (coverage-test)
      (run-test-proper)))

(define test-accounts
  (list "Root" (list (cons 'type ACCT-TYPE-ROOT))
        (list "Assets"(list (cons 'type ACCT-TYPE-ASSET))
              (list "Current"
                    (list "Savings" (list (cons 'type ACCT-TYPE-BANK)))
                    (list "Checking-DEM" (list (cons 'type ACCT-TYPE-BANK)))
                    (list "Checking" (list (cons 'type ACCT-TYPE-BANK))))
              (list "Investment"
                    (list "Broker A"
                          (list "Cash-A" (list (cons 'type ACCT-TYPE-BANK)))
                          (list "Stocks" (list (cons 'type ACCT-TYPE-STOCK))
                                (list "AAPL-A")
                                (list "IBM-A")
                                (list "MSFT-A")
                                (list "DMLR-A")
                                (list "TSLA-A")))
                    (list "Broker B"
                          (list "Cash-B" (list (cons 'type ACCT-TYPE-BANK)))
                          (list "Stocks" (list (cons 'type ACCT-TYPE-STOCK))
                                (list "AAPL-B")
                                (list "IBM-B")
                                (list "MSFT-B")
                                (list "TSLA-B")))
                    (list "Broker-GBP"
                          (list "Cash-GBP" (list (cons 'type ACCT-TYPE-BANK)))
                          (list "Stocks" (list (cons 'type ACCT-TYPE-STOCK))
                                (list "RDSA")))))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME))
              (list "Capital Gains-DEM")
              (list "Capital Gains"))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))
        (list "Liability" (list (cons 'type ACCT-TYPE-LIABILITY)))
        (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY))
              (list "Opening Balances"))))

(define (setup trading)
  (let* ((env (create-test-env))
         (book (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (IBM (gnc-commodity-new book "International Business Machines"
                                 "NYSE" "IBM" "" 1))
         (MSFT (gnc-commodity-new book "Microsoft" "NASDAQ" "MSFT" "" 1))
         (TSLA (gnc-commodity-new book "Tesla Motors" "NASDAQ" "TSLA" "" 1))
         (RDSA (gnc-commodity-new book "Royal Dutch Shell A" "LSE" "RDSA" "" 1))
         ;; Yeah, this is fake, it's for testing DEM->EUR conversions.
         (DMLR (gnc-commodity-new book "Daimler Motors" "FSE" "DMLR" "" 1))
         (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
         (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
         (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (account-alist (env-create-account-structure-alist env test-accounts))
         (checking (cdr (assoc "Checking" account-alist)))
         (checking-dem (cdr (assoc "Checking-DEM" account-alist)))
         (saving (cdr (assoc "Savings" account-alist)))
         (cash-a (cdr (assoc "Cash-A" account-alist)))
         (aapl-a (cdr (assoc "AAPL-A" account-alist)))
         (ibm-a (cdr (assoc "IBM-A" account-alist)))
         (msft-a (cdr (assoc "MSFT-A" account-alist)))
         (tsla-a (cdr (assoc "TSLA-A" account-alist)))
         (dmlr-a (cdr (assoc "DMLR-A" account-alist)))
         (cash-b (cdr (assoc "Cash-B" account-alist)))
         (aapl-b (cdr (assoc "AAPL-B" account-alist)))
         (ibm-b (cdr (assoc "IBM-B" account-alist)))
         (msft-b (cdr (assoc "MSFT-B" account-alist)))
         (tsla-b (cdr (assoc "TSLA-B" account-alist)))
         (capgain (cdr (assoc "Capital Gains" account-alist)))
         (capgain-dem (cdr (assoc "Capital Gains-DEM" account-alist)))
         (openbal (cdr (assoc "Opening Balances" account-alist))))
    ;; Set account commodities
    (gnc-commodity-table-insert comm-table AAPL)
    (gnc-commodity-table-insert comm-table MSFT)
    (gnc-commodity-table-insert comm-table IBM)
    (gnc-commodity-table-insert comm-table RDSA)
    (gnc-commodity-table-insert comm-table TSLA)
    (gnc-commodity-table-insert comm-table DMLR)
    (xaccAccountSetCommodity checking-dem DEM)
    (xaccAccountSetCommodity capgain-dem DEM)
    (xaccAccountSetCommodity aapl-a AAPL)
    (xaccAccountSetCommodity ibm-a IBM)
    (xaccAccountSetCommodity msft-a MSFT)
    (xaccAccountSetCommodity tsla-a TSLA)
    (xaccAccountSetCommodity dmlr-a DMLR)
    (xaccAccountSetCommodity aapl-b AAPL)
    (xaccAccountSetCommodity ibm-b IBM)
    (xaccAccountSetCommodity msft-b MSFT)
    (xaccAccountSetCommodity tsla-b TSLA)
    ;; Create transactions in the accounts
    (env-transfer env 15 11 2011 openbal saving 1553746/100
                  #:description "Fund Savings")
    (env-transfer env 15 11 2011 openbal checking 329726/100
                  #:description "Fund Checking")
    (env-transfer env 15 11 2011 openbal cash-a 11543627/100
                  #:description "Fund Broker A")
    (env-transfer-foreign env 15 01 2012 cash-a ibm-a 3583200/100 200
                          #:description "Buy IBM 200") ;;200 @ $179.16
    (env-transfer-foreign env 15 01 2012 cash-a msft-a 4216500/100 1500
                          #:description "Buy MSFT 1500") ;;1500 @ $28.11
    (env-transfer-foreign env 20 01 2012 checking-dem dmlr-a 1500 80
                          #:description "Buy DMLR 80") ;;80 @ DM1500.00
    (env-transfer-foreign env 20 02 2012 checking-dem dmlr-a -1610 80
                          #:description "Sell DMLR 80") ;;80 @ DM1610.00
    (env-transfer-foreign env 20 02 2012 capgain-dem dmlr-a 110 0
                          #:description "DMLR 80 G/L") ;;80 @ DM1610.00
    (env-transfer-foreign env 9 8 2013 cash-a aapl-a 3684000/100 600
                          #:description "Buy AAPL 600") ;;600 @ $61.40
    (env-transfer-foreign env 5 12 2014  cash-a msft-a -2421000/100 -500
                          #:description "Sell MSFT 500");;-500 @ $48.42
    (env-transfer-foreign env 5 12 2014 capgain msft-a 1015500/100 0
                          #:description "MSFT 500 G/L")
    (env-transfer-foreign env 8 8 2014 cash-a ibm-a -3732600/100 -200
                          #:description "Sell IBM 200") ;;-200 @ $186.63
    (env-transfer-foreign env 8 8 2014 capgain ibm-a 149400/100 0
                          #:description "IBM 200 G/L")
    (env-transfer env 15 6 2014 cash-a cash-b 4000000/100
                  #:description "Fund Broker B")
    (env-transfer-foreign env 11 7 2014 cash-b aapl-b 3808800/100 400
                          #:description "Buy AAPL 400") ;;400 @ $95.22
    (env-transfer-foreign env 2 4 2015 cash-a msft-a 3223200/100 800
                          #:description "Buy MSFT 800") ;;800 @ $40.29
    (env-transfer-foreign env 23 10 2015 cash-a aapl-a -3572400/100 -300
                          #:description "Sell AAPL 300") ;;-300 @ $119.08
    (env-transfer-foreign env 23 10 2015 capgain aapl-a 1730400/100 0
                          #:description"AAPL 300 G/L")
    (env-transfer-foreign env 11 3 2016 cash-a msft-a -4776300/100 -900
                          #:description "Sell MSFT 900") ;;-900 @ $53.07
    (env-transfer-foreign env 11 3 2016 capgain msft-a 1758200/100 0
                          #:description"MSFT 900 G/L")
    (gnc-pricedb-create USD MSFT (gnc-dmy2time64 1 1 2013) 2674/100)
    (gnc-pricedb-create USD IBM (gnc-dmy2time64 1 1 2013) 19399/100)
    (gnc-pricedb-create USD AAPL (gnc-dmy2time64 1 1 2014) 7728/100)
    (gnc-pricedb-create USD MSFT (gnc-dmy2time64 1 1 2014) 3691/100)
    (gnc-pricedb-create USD IBM (gnc-dmy2time64 1 1 2014) 18669/100)
    (gnc-pricedb-create USD AAPL (gnc-dmy2time64 1 1 2015) 10933/100)
    (gnc-pricedb-create USD MSFT (gnc-dmy2time64 1 1 2015) 4676/100)
    (gnc-pricedb-create USD IBM (gnc-dmy2time64 1 1 2015) 16206/100)
    (gnc-pricedb-create USD AAPL (gnc-dmy2time64 1 1 2016) 10526/100)
    (gnc-pricedb-create USD MSFT (gnc-dmy2time64 1 1 2016) 5548/100)
    (gnc-pricedb-create USD IBM (gnc-dmy2time64 1 1 2016) 13163/100)
    (gnc-pricedb-create USD AAPL (gnc-dmy2time64 1 1 2017) 11582/100)
    (gnc-pricedb-create USD MSFT (gnc-dmy2time64 1 1 2017) 6214/100)
    (gnc-pricedb-create USD IBM (gnc-dmy2time64 1 1 2017) 16599/100)
    account-alist))


(define (teardown)
  (gnc-clear-current-session))

(define (collect collector shares value)
  ((car collector) 'add shares)
  ((cdr collector) 'add value))

(define (test-setup)
  ;; This test ensures that our setup function creates a suitable book
  (let* ((account-alist (setup #f))
         (cash-a (cdr (assoc "Cash-A" account-alist)))
         (book (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (pricedb (gnc-pricedb-get-db book))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM")))
    (test-begin "Test Setup")
    (test-equal "Broker A Cash account balance 73390.27"
                7339027/100 (xaccAccountGetBalance cash-a))
    (test-assert "Have IBM Prices" (gnc-pricedb-has-prices pricedb IBM USD))
    (let ((ibm-price (gnc-pricedb-lookup-latest pricedb IBM USD)))
      (test-equal "IBM Latest Price" 16599/100 (gnc-price-get-value ibm-price))
      (gnc-price-unref ibm-price))
    (test-end "Test Setup")
    (teardown)))

(define (test-resolve-unknown-comm)
  (test-group-with-cleanup "gnc:resolve-unknown-comm"
  (let* ((account-alist (setup #f))
         (book  (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
         (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
         (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
         (MSFT (gnc-commodity-table-lookup comm-table "NASDAQ" "MSFT"))
         (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM"))
         (AAPL (gnc-commodity-table-lookup comm-table "NASDAQ" "AAPL"))
         (RDSA (gnc-commodity-table-lookup comm-table "LSE" "RDSA"))
         (DMLR (gnc-commodity-table-lookup comm-table "FSE" "DMLR"))
         (aapl-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (msft-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (ibm-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (rdsa-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (rdsa-gbp-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (dmlr-dem-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (gbp-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (eur-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (gbp-usd-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (gbp-eur-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (gbp-dem-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (dem-gbp-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (eur-gbp-col (cons (gnc:make-value-collector) (gnc:make-value-collector)))
         (eur-usd-col (cons (gnc:make-value-collector) (gnc:make-value-collector))))
    (test-begin "basic")
    ;; Entries in the report currency just fall through and are emitted in the
    ;; result alist.
    (collect aapl-col 600  3684000/100)
    (collect aapl-col  -300 -3572400/100)
    (collect aapl-col 0 1730400/100) ;; cap gain
    (let* ((sumlist (list (list USD  (list (list AAPL  aapl-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist USD)))
      (test-equal "AAPL 700 shares" 300 ((caadr (assoc AAPL return-alist)) 'total #f))
      (test-equal "AAPL $18420.00" 1842000/100 ((cdadr (assoc AAPL return-alist)) 'total #f)))
    (test-end "basic")
    (test-begin "foreign-no-coll")
    ;; Now we begin to exercise the function. First up is that it fails to
    ;; register the security at all if there's no pair of prices that can
    ;; resolve the transaction commodity to the report currency.
    (collect rdsa-gbp-col  500 3223400/100)
    ;; We need a report-currency alist with something in it or
    ;; resolve-unknown-comm crashes.
    (let* ((sumlist (list (list USD  (list (list AAPL  aapl-col)))
                          (list GBP (list (list RDSA rdsa-gbp-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist USD)))
      (test-equal "RDSA #f" #f ((caadr (assoc RDSA return-alist)) 'total #f))
      (test-equal "RDSA #f" #f ((cdadr (assoc RDSA return-alist)) 'total #f)))
    (test-end "foreign-no-coll")
    (test-begin "foreign-no-amount")
    ;; There's a collector but it doesn't have a price in it so the returned
    ;; price is 0.
    (let* ((sumlist (list (list USD (list (list GBP gbp-col)))
                           (list GBP (list (list RDSA rdsa-gbp-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist USD)))
      (test-equal "RDSA 500 shares" 500 ((caadr (assoc RDSA return-alist)) 'total #f))
      (test-equal "RDSA $0" 0 ((cdadr (assoc RDSA return-alist)) 'total #f)))
    (test-end "foreign-no-amount")
    (test-begin "foreign-coll-and-amount")
    (collect gbp-col  10000/100 15300/100)
    (let* ((sumlist (list (list USD (list (list GBP gbp-col)))
                           (list GBP (list (list RDSA rdsa-gbp-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist USD)))
      (test-equal "RDSA 500 shares" 500 ((caadr (assoc RDSA return-alist)) 'total #f))
      (test-equal "RDSA $49,318.02" 4931802/100 ((cdadr (assoc RDSA return-alist)) 'total #f)))
    (test-end "foreign-coll-and-amount")
    (test-begin "foreign-inv-coll")
    ;; Now try with a conversion in the foreign currency instead of the native
    ;; one.
    (collect gbp-usd-col  15300/100 10000/100)
    (let* ((sumlist (list (list USD  (list (list AAPL  aapl-col)))
                          (list GBP (list (list USD gbp-usd-col)
                                          (list RDSA rdsa-gbp-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist USD)))
      (test-equal "RDSA 500 shares" 500 ((caadr (assoc RDSA return-alist)) 'total #f))
      (test-equal "RDSA $49318.02" 4931802/100 ((cdadr (assoc RDSA return-alist)) 'total #f)))
    (test-end "foreign-inv-coll")
    (test-begin "foreign-3way")
    ;; Three-way conversion, gbp->eur->usd
    (collect eur-gbp-col  10000/100 121045/1000)
    (collect eur-col 10000/100 126399/1000)
    (let* ((sumlist (list (list USD  (list (list EUR  eur-col)))
                          (list EUR  (list (list GBP eur-gbp-col)))
                          (list GBP (list (list RDSA rdsa-gbp-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist USD)))
      (test-equal "RDSA 500 shares" 500 ((caadr (assoc RDSA return-alist)) 'total #f))
      (test-equal "RDSA $49317.91" 4931791/100 (gnc-numeric-convert ((cdadr (assoc RDSA return-alist)) 'total #f) 100 GNC-HOW-RND-ROUND)))
    (test-end "foreign-3way")
    (test-begin "foreign-3way-ambig")
    ;; Three-way conversion, gbp->eur->usd The equalities are false because
    ;; there is both a USD price and a GBP price for
    ;; RDSA. gnc:get-exchange-totals is supposed to resolve this when writing
    ;; the sumlist, we're testing that gnc:resolve-unknown-comm writes its
    ;; warning.
    (collect eur-gbp-col  10000/100 121045/1000)
    (collect eur-col 10000/100 126399/1000)
    (collect eur-usd-col 126399/1000 10000/100)
    (collect rdsa-col 10000/100 1219300/100)
    (let* ((sumlist (list (list USD  (list (list RDSA rdsa-col)
                                           (list EUR  eur-col)))
                          (list EUR  (list (list GBP eur-gbp-col)
                                           (list USD eur-usd-col)))
                          (list GBP (list (list RDSA rdsa-gbp-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist USD)))
      (test-assert "RDSA 600 shares" (not (equal? 600 ((caadr (assoc RDSA return-alist)) 'total #f))))
      (test-assert "RDSA $61510.91" (not (equal? 6151091/100 (gnc-numeric-convert ((cdadr (assoc RDSA return-alist)) 'total #f) 100 GNC-HOW-RND-ROUND)))))
    (test-end "foreign-3way-ambig")
    (test-begin "foreign-DEM>EUR")
    ;; Old currency->Euro conversion.
    (collect dmlr-dem-col  500 2668000/100)
    (let* ((sumlist (list (list EUR  (list (list USD eur-usd-col)))
                          (list DEM (list (list DMLR  dmlr-dem-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist EUR)))
      (test-equal "DMLR 500 shares" 500 ((caadr (assoc DMLR return-alist)) 'total #f))
      (test-equal "DMLR EUR13631.27" 1364127/100 (gnc-numeric-convert ((cdadr (assoc DMLR return-alist)) 'total #f) 100 GNC-HOW-RND-ROUND)))
    (test-end "foreign-DEM>EUR")
    (test-begin "foreign-3way-DEM>EUR")
    ;; Three-way conversion, gbp->dem->eur->usd
    ;; Too many levels for resolve-unknown-comm to resolve.
    (collect gbp-dem-col  10000/100 23665543/100000)
    (let* ((sumlist (list (list USD  (list (list EUR  eur-col)))
                          (list GBP (list (list DEM gbp-dem-col)
                                          (list RDSA rdsa-gbp-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist USD)))
      (test-equal "Shares fails" #f ((caadr (assoc RDSA return-alist)) 'total #f))
      (test-equal "Value fails" #f ((cdadr (assoc RDSA return-alist)) 'total #f)))
    (test-end "foreign-3way-GBP>DEM")
    (test-begin "foreign-3way-DEM>GBP")
    ;; Three-way conversion, gbp->dem->eur->usd
    ;; Too many levels for resolve-unknown-comm to resolve.
    (collect gbp-dem-col  23665543/100000 10000/100)
    (let* ((sumlist (list (list USD  (list (list EUR  eur-col)))
                          (list DEM (list (list GBP dem-gbp-col)))
                          (list GBP (list (list RDSA rdsa-gbp-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist USD)))
      (test-equal "Shares fails" #f ((caadr (assoc RDSA return-alist)) 'total #f))
      (test-equal "Value fails" #f ((cdadr (assoc RDSA return-alist)) 'total #f)))
    (test-end "foreign-3way-DEM>GBP")
    (test-begin "foreign-DEM>EUR")
    ;; Three-way conversion, gbp->dem->eur
    ;; Too many levels for resolve-unknown-comm to resolve.
    (let* ((sumlist (list (list EUR  (list (list USD  eur-usd-col)))
                          (list GBP (list (list DEM gbp-dem-col)
                                          (list RDSA rdsa-gbp-col)))))
           (return-alist  (gnc:resolve-unknown-comm sumlist EUR)))
      (test-equal "Shares fails" #f ((caadr (assoc RDSA return-alist)) 'total #f))
      (test-equal "Value fails" #f ((cdadr (assoc RDSA return-alist)) 'total #f)))
    (test-end "foreign-DEM>EUR"))

  (teardown)))

(define (test-get-exchange-totals)
  (test-group-with-cleanup "gnc:get-exchange-totals"
  (let* ((account-alist (setup #f))
         (book  (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
         (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
         (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
         (MSFT (gnc-commodity-table-lookup comm-table "NASDAQ" "MSFT"))
         (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM"))
         (AAPL (gnc-commodity-table-lookup comm-table "NASDAQ" "AAPL"))
         (RDSA (gnc-commodity-table-lookup comm-table "LSE" "RDSA"))
         (DMLR (gnc-commodity-table-lookup comm-table "FSE" "DMLR")))
    (test-begin "multiple")
    (let ((return-alist (gnc:get-exchange-totals USD (gnc-dmy2time64-neutral 1 12 2016))))
      (test-equal "AAPL 1300 shares" 1300 ((caadr (assoc AAPL return-alist)) 'total #f))
      (test-equal "AAPL $110652.00" 11065200/100 ((cdadr (assoc AAPL return-alist)) 'total #f))
      (test-equal "MSFT 3700 shares" 3700 ((caadr (assoc MSFT return-alist)) 'total #f))
      (test-equal "MSFT $146370.00" 14637000/100 ((cdadr (assoc MSFT return-alist)) 'total #f))
      (test-equal "IBM 400 shares" 400 ((caadr (assoc IBM  return-alist)) 'total #f))
      (test-equal "IBM $73158" 7315800/100 ((cdadr (assoc IBM return-alist)) 'total #f)))
    (test-end "multiple"))
  (teardown)))

(define (test-get-exchange-cost-totals)
  (test-group-with-cleanup "gnc:get-exchange-cost-totals"
  (let* ((account-alist (setup #f))
         (book  (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
         (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
         (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
         (MSFT (gnc-commodity-table-lookup comm-table "NASDAQ" "MSFT"))
         (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM"))
         (AAPL (gnc-commodity-table-lookup comm-table "NASDAQ" "AAPL"))
         (RDSA (gnc-commodity-table-lookup comm-table "LSE" "RDSA"))
         (DMLR (gnc-commodity-table-lookup comm-table "FSE" "DMLR")))
    (test-begin "multiple")
    (let ((return-alist (gnc:get-exchange-cost-totals USD (gnc-dmy2time64-neutral 1 12 2016))))
      (test-equal "AAPL 700 shares" 700 ((caadr (assoc AAPL return-alist)) 'total #f))
      (test-equal "AAPL $56512.00" 5650800/100 ((cdadr (assoc AAPL return-alist)) 'total #f))
      (test-equal "MSFT 900 shares" 900 ((caadr (assoc MSFT return-alist)) 'total #f))
      (test-equal "MSFT $30161.00" 3016100/100 ((cdadr (assoc MSFT return-alist)) 'total #f))
      (test-equal "IBM 0 shares" 0 ((caadr (assoc IBM  return-alist)) 'total #f))
      (test-equal "IBM $0" 0 ((cdadr (assoc IBM return-alist)) 'total #f)))
    (test-end "multiple"))
  (teardown)))

(define (test-get-exchange-cost-totals-trading)
  (test-group-with-cleanup
   "gnc:get-exchange-totals-trading"
   (let* ((account-alist (setup #t))
         (book  (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
         (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
         (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
         (MSFT (gnc-commodity-table-lookup comm-table "NASDAQ" "MSFT"))
         (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM"))
         (AAPL (gnc-commodity-table-lookup comm-table "NASDAQ" "AAPL"))
         (RDSA (gnc-commodity-table-lookup comm-table "LSE" "RDSA"))
         (DMLR (gnc-commodity-table-lookup comm-table "FSE" "DMLR")))
     (test-begin "multiple")
     (let ((return-alist (gnc:get-exchange-cost-totals
                          USD (gnc-dmy2time64-neutral 1 12 2016))))
       (test-equal "AAPL 700 shares"
                   700 ((caadr (assoc AAPL return-alist)) 'total #f))
       (test-equal "AAPL $56512.00"
                   5650800/100 ((cdadr (assoc AAPL return-alist)) 'total #f))
       (test-equal "MSFT 900 shares"
                   900 ((caadr (assoc MSFT return-alist)) 'total #f))
       (test-equal "MSFT $30161.00"
                   3016100/100 ((cdadr (assoc MSFT return-alist)) 'total #f))
       (test-equal "IBM 0 shares"
                   0 ((caadr (assoc IBM  return-alist)) 'total #f))
       (test-equal "IBM $0"
                   0 ((cdadr (assoc IBM return-alist)) 'total #f)))
     (test-end "multiple"))
   (teardown)))

(define (test-exchange-by-pricedb-latest)
  (test-group-with-cleanup
   "gnc:exchange-by-pricedb-latest"
   (let* ((account-alist (setup #f))
         (book  (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (pricedb (gnc-pricedb-get-db book))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
         (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
         (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
         (MSFT (gnc-commodity-table-lookup comm-table "NASDAQ" "MSFT"))
         (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM"))
         (AAPL (gnc-commodity-table-lookup comm-table "NASDAQ" "AAPL"))
         (RDSA (gnc-commodity-table-lookup comm-table "LSE" "RDSA"))
         (DMLR (gnc-commodity-table-lookup comm-table "FSE" "DMLR")))
     (test-begin "multiple")
     (test-equal "AAPL latest" 11582/100 (gnc:gnc-monetary-amount
                                          (gnc:exchange-by-pricedb-latest
                                           (gnc:make-gnc-monetary AAPL 1) USD)))
     (test-equal "MSFT latest" 6214/100 (gnc:gnc-monetary-amount
                                         (gnc:exchange-by-pricedb-latest
                                          (gnc:make-gnc-monetary MSFT 1) USD)))
     (test-equal "IBM latest" 16599/100 (gnc:gnc-monetary-amount
                                         (gnc:exchange-by-pricedb-latest
                                          (gnc:make-gnc-monetary IBM 1) USD)))
     (test-end "multiple"))
   (teardown)))

(define (test-exchange-by-pricedb-nearest)
    (test-group-with-cleanup
   "gnc:exchange-by-pricedb-nearest"
   (let* ((account-alist (setup #f))
         (book  (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
         (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
         (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
         (MSFT (gnc-commodity-table-lookup comm-table "NASDAQ" "MSFT"))
         (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM"))
         (AAPL (gnc-commodity-table-lookup comm-table "NASDAQ" "AAPL"))
         (RDSA (gnc-commodity-table-lookup comm-table "LSE" "RDSA"))
         (DMLR (gnc-commodity-table-lookup comm-table "FSE" "DMLR")))
     (test-begin "multiple")
     (test-equal "AAPL nearest 23 March 2015"
                 10933/100 (gnc:gnc-monetary-amount
                            (gnc:exchange-by-pricedb-nearest
                             (gnc:make-gnc-monetary AAPL 1) USD
                             (gnc-dmy2time64 23 3 2015))))
     (test-equal "MSFT nearest 11 September 2016"
                 6214/100 (gnc:gnc-monetary-amount
                           (gnc:exchange-by-pricedb-nearest
                            (gnc:make-gnc-monetary MSFT 1) USD
                            (gnc-dmy2time64 11 9 2016))))
     (test-equal "IBM nearest 1 July 2014"
                 18663/100 (gnc:gnc-monetary-amount
                            (gnc:exchange-by-pricedb-nearest
                             (gnc:make-gnc-monetary IBM 1) USD
                             (gnc-dmy2time64 1 7 2014))))
     (test-end "multiple"))
   (teardown)))

(define (test-get-commodity-totalavg-prices)
    (test-group-with-cleanup
   "gnc:get-commodity-totalavg-prices"
   (let* ((account-alist (setup #f))
         (book  (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
         (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
         (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
         (MSFT (gnc-commodity-table-lookup comm-table "NASDAQ" "MSFT"))
         (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM"))
         (AAPL (gnc-commodity-table-lookup comm-table "NASDAQ" "AAPL"))
         (RDSA (gnc-commodity-table-lookup comm-table "LSE" "RDSA"))
         (DMLR (gnc-commodity-table-lookup comm-table "FSE" "DMLR")))
     (test-begin "Microsoft-USD")
     (let* ((curraccts (gnc-account-get-descendants-sorted
                        (gnc-get-current-root-account)))
            (report-list
             (gnc:get-commodity-totalavg-prices curraccts
                                                (gnc-dmy2time64 4 7 2016)
                                                MSFT USD)))
       (test-equal "MSFT totalavg 2012-01-15" (/ 4216500/100 1500)
                   (cadr (assoc (gnc-dmy2time64-neutral 15 01 2012)
                                report-list)))
;; Astute observers will notice that the totals include the
;; capital gain split but not the actual sell split on the day because the
;; capital gain price is first in the list so that's the one (assoc) finds. See
;; the comment at the gnc:get-commodity-totalavg-prices definition for more
;; about the prices from this function.
       (test-equal "MSFT totalavg 2014-12-05"
         (/ 6637500/100 2000)
         (cadr (assoc (gnc-dmy2time64-neutral 5 12 2014)
                      report-list)))
       (test-equal "MSFT totalavg 2015-04-02"
         (/ 9860700/100 2800)
         (cadr (assoc (gnc-dmy2time64-neutral 2 4 2015) report-list)))
       (test-equal "MSFT totalavg 2016-03-11"
         (/ 14637000/100 3700)
         (cadr (assoc (gnc-dmy2time64-neutral 11 3 2016)
                      report-list))))
     (test-end "Microsoft-USD")

     (test-begin "Daimler-DEM")
     (let* ((curraccts (gnc-account-get-descendants-sorted
                        (gnc-get-current-root-account)))
            (report-list
             (gnc:get-commodity-totalavg-prices curraccts
                                                (gnc-dmy2time64 4 7 2016)
                                                DMLR EUR)))
       (test-equal "DMLR totalavg 2012-01-20"
         38347/4000
         (cadr (assoc (gnc-dmy2time64-neutral 20 01 2012)
                      report-list)))
       (test-equal "DMLR totalavg 2012-02-20"
         39753/4000
         (cadr (assoc (gnc-dmy2time64-neutral 20 02 2012)
                      report-list))))
     (test-end "Daimler-DEM"))
   (teardown)))

(define (test-get-commodity-inst-prices)
      (test-group-with-cleanup
   "gnc:get-commodity-inst-prices"
   (let* ((account-alist (setup #f))
         (book  (gnc-get-current-book))
         (comm-table (gnc-commodity-table-get-table book))
         (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
         (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
         (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
         (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
         (MSFT (gnc-commodity-table-lookup comm-table "NASDAQ" "MSFT"))
         (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM"))
         (AAPL (gnc-commodity-table-lookup comm-table "NASDAQ" "AAPL"))
         (RDSA (gnc-commodity-table-lookup comm-table "LSE" "RDSA"))
         (DMLR (gnc-commodity-table-lookup comm-table "FSE" "DMLR")))
     (test-begin "Microsoft-USD")
     (let* ((curraccts (gnc-account-get-descendants-sorted
                        (gnc-get-current-root-account)))
            (report-list
             (gnc:get-commodity-inst-prices curraccts
                                                (gnc-dmy2time64 4 7 2016)
                                                MSFT USD)))
       (test-equal "MSFT inst 2012-01-15" (/ 4216500/100 1500)
                   (cadr (assoc (gnc-dmy2time64-neutral 15 01 2012)
                                report-list)))
       (test-equal "MSFT inst 2014-12-05" (/ 2421000/100 500)
                   (cadr (assoc (gnc-dmy2time64-neutral 5 12 2014)
                                report-list)))
       (test-equal "MSFT inst 2015-04-02" (/ 3223200/100 800)
                   (cadr (assoc (gnc-dmy2time64-neutral 2 4 2015) report-list)))
       (test-equal "MSFT inst 2016-03-11" (/ 4776300/100 900)
                   (cadr (assoc (gnc-dmy2time64-neutral 11 3 2016)
                                report-list))))
     (test-end "Microsoft-USD")

     (test-begin "Daimler-DEM")
     (let* ((curraccts (gnc-account-get-descendants-sorted
                        (gnc-get-current-root-account)))
            (report-list
             (gnc:get-commodity-inst-prices curraccts
                                            (gnc-dmy2time64 4 7 2016)
                                            DMLR EUR)))
       (test-equal "DMLR inst 2012-01-20"
         38347/4000
         (cadr (assoc (gnc-dmy2time64-neutral 20 01 2012)
                      report-list)))
       (test-equal "DMLR inst 2012-02-20"
         41159/4000
         (cadr (assoc (gnc-dmy2time64-neutral 20 02 2012)
                      report-list))))
     (test-end "Daimler-DEM"))
   (teardown)))

(define (test-weighted-average)
  (test-group-with-cleanup "test-weighted-average"
    (let* ((account-alist (setup #f))
           (book  (gnc-get-current-book))
           (comm-table (gnc-commodity-table-get-table book))
           (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
           (GBP (gnc-commodity-table-lookup comm-table "CURRENCY" "GBP"))
           (EUR (gnc-commodity-table-lookup comm-table "CURRENCY" "EUR"))
           (DEM (gnc-commodity-table-lookup comm-table "CURRENCY" "DEM"))
           (MSFT (gnc-commodity-table-lookup comm-table "NASDAQ" "MSFT"))
           (IBM (gnc-commodity-table-lookup comm-table "NYSE" "IBM"))
           (AAPL (gnc-commodity-table-lookup comm-table "NASDAQ" "AAPL"))
           (RDSA (gnc-commodity-table-lookup comm-table "LSE" "RDSA"))
           (DMLR (gnc-commodity-table-lookup comm-table "FSE" "DMLR")))

      (let ((exchange-fn (gnc:case-exchange-time-fn
                          'weighted-average USD
                          (list EUR USD GBP DEM AAPL)
                          (gnc-dmy2time64-neutral 20 02 2016)
                          #f #f)))
        (test-equal "gnc:case-exchange-time-fn weighted-average 20/02/2012"
          307/5
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 20 02 2012))))

        (test-equal "gnc:case-exchange-time-fn weighted-average 20/02/2014"
          9366/125
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 20 02 2014))))

        (test-equal "gnc:case-exchange-time-fn weighted-average 09/09/2013"
          307/5
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 09 09 2013))))

        (test-equal "gnc:case-exchange-time-fn weighted-average 11/08/2014"
          9366/125
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 11 08 2014))))

        (test-equal "gnc:case-exchange-time-fn weighted-average 22/10/2015"
          27663/325
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 22 10 2015))))

        (test-equal "gnc:case-exchange-time-fn weighted-average 24/10/2015"
          27663/325
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 24 10 2015)))))

      (let ((exchange-fn (gnc:case-exchange-time-fn
                          'average-cost USD
                          (list EUR USD GBP DEM AAPL)
                          (gnc-dmy2time64-neutral 20 02 2016)
                          #f #f)))
        (test-equal "gnc:case-exchange-time-fn average-cost 20/02/2012"
          14127/175
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 20 02 2012)))))

      (let ((exchange-fn (gnc:case-exchange-time-fn
                          'pricedb-latest USD
                          (list EUR USD GBP DEM AAPL)
                          (gnc-dmy2time64-neutral 20 02 2016)
                          #f #f)))
        (test-equal "gnc:case-exchange-time-fn pricedb-latest 20/02/2012"
          5791/50
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 20 02 2012)))))

      (let ((exchange-fn (gnc:case-exchange-time-fn
                          'pricedb-nearest USD
                          (list EUR USD GBP DEM AAPL)
                          (gnc-dmy2time64-neutral 20 02 2016)
                          #f #f)))
        (test-equal "gnc:case-exchange-time-fn pricedb-nearest 20/02/2012"
          307/5
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 20 02 2012)))))

      (let ((exchange-fn (gnc:case-exchange-time-fn
                          'actual-transactions USD
                          (list EUR USD GBP DEM AAPL)
                          (gnc-dmy2time64-neutral 20 02 2016)
                          #f #f)))
        (test-equal "gnc:case-exchange-time-fn actual-transactions 20/02/2012"
          307/5
          (gnc:gnc-monetary-amount
           (exchange-fn
            (gnc:make-gnc-monetary AAPL 1)
            USD
            (gnc-dmy2time64-neutral 20 02 2012)))))

      (teardown))))

