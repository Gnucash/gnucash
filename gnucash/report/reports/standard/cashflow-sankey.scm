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

(define-module (gnucash reports standard cashflow-sankey))

(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (gnucash utilities))
(use-modules (gnucash report))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash engine))

(define reportname (N_ "Cash Flow Sankey Chart"))
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-accounts (N_ "Accounts"))
(define optname-currency (N_ "Report Currency"))
(define optname-levels (N_ "Show Accounts until level"))

(define (options-generator)
  (let ((options (gnc-new-optiondb)))

    (gnc-register-account-list-option
     options gnc:pagename-accounts optname-accounts "a"
     (G_ "Report on these accounts.") '())

    (gnc:options-add-account-levels!
     options gnc:pagename-accounts optname-levels "c"
     (N_ "Show accounts to this depth and not further.") 2)

    (gnc:options-add-date-interval!
     options gnc:pagename-general optname-from-date optname-to-date "a")

    (gnc:options-add-currency!
     options gnc:pagename-general optname-currency "b")

    options))

(define (sankey-renderer report-obj)

  (define options
    (gnc:report-options report-obj))

  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc-optiondb-lookup-value options section name))

  (let* ((to-date (gnc:time64-end-day-time
                   (gnc:date-option-absolute-time
                    (get-option gnc:pagename-general optname-to-date))))
         (from-date (gnc:time64-start-day-time
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general optname-from-date))))
         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (account-levels (get-option gnc:pagename-accounts optname-levels))
         (tree-depth (if (eq? account-levels 'all) +inf.0 account-levels))
         (report-currency (get-option gnc:pagename-general optname-currency))
         (document (gnc:make-html-document))
         (totals-hash (make-hash-table))
         (splits
          (let ((query (qof-query-create-for-splits)))
            (qof-query-set-book query (gnc-get-current-book))
            (xaccQueryAddAccountMatch query accounts QOF-GUID-MATCH-ANY QOF-QUERY-AND)
            (xaccQueryAddDateMatchTT query #t from-date #t to-date QOF-QUERY-AND)
            (let ((rv (xaccQueryGetSplitsUniqueTrans query)))
              (qof-query-destroy query)
              rv))))

    (define (in-accounts? split)
      (member (xaccSplitGetAccount split) accounts))
    (define (trading? split)
      (equal? (xaccAccountGetType (xaccSplitGetAccount split)) ACCT-TYPE-TRADING))

    (gnc:html-document-set-title! document reportname)

    (gnc:html-document-add-object!
     document (gnc:html-render-options-changed options))

    ;; the following will scan splits to populate the totals-hash, a
    ;; hash-list of account => total-amount pairs. Starts from all
    ;; splits from selected accounts within dates specified.
    (let lp ((splits splits))
      (match splits
        (() #f)
        ((this . rest)
         ;; all splits->parent-txns are scanned, ignoring trading and
         ;; splits from selected accounts. we only want the splits
         ;; feeding in or out of selected accounts.
         (define txn (xaccSplitGetParent this))
         (define txn-date (time64CanonicalDayTime (xaccTransGetDate txn)))
         (let lp1 ((txn-splits (xaccTransGetSplitList txn)))
           (match txn-splits
             (() (lp rest))
             (((? trading?) . rest) (lp1 rest))
             (((? in-accounts?) . rest) (lp1 rest))
             ((txn-split . rest-splits)
              ;; the feed-in/out account and ancestors, including
              ;; root, are processed to tally the split->amount.
              (define amt
                (gnc:gnc-monetary-amount
                 (gnc:exchange-by-pricedb-nearest
                  (gnc:make-gnc-monetary
                   (xaccAccountGetCommodity (xaccSplitGetAccount txn-split))
                   (xaccSplitGetAmount txn-split))
                  report-currency txn-date)))
              (let lp2 ((acc (xaccSplitGetAccount txn-split)))
                (cond
                 ((gnc-account-is-root acc) (lp1 rest-splits))
                 (else
                  (hash-set! totals-hash acc (+ (hash-ref totals-hash acc 0) amt))
                  (lp2 (gnc-account-get-parent acc)))))))))))

    (let ((chart (gnc:make-html-chart)))
      (gnc:html-chart-set-type! chart 'sankey)
      (gnc:html-chart-set-title! chart reportname)
      (gnc:html-chart-set! chart '(options animation) #f)
      (gnc:html-chart-add-data-series!
       chart
       #f
       (filter-map
        (match-lambda
          ((account . flow)
           (let* ((parent (gnc-account-get-parent account))
                  (acc-name (xaccAccountGetName account))
                  (oth-name (if (gnc-account-is-root parent) ""
                                (xaccAccountGetName parent))))
             ;; the following specifies account -> parent or parent ->
             ;; account flows depending on the amount sign, ignoring
             ;; accounts deeper than specified depth, and accounts with
             ;; net zero in/outflows.
             (cond
              ((> (gnc-account-get-current-depth account) tree-depth) #f)
              ((> 0 flow) `((from . ,acc-name) (to . ,oth-name) (flow . ,(- flow))))
              ((< 0 flow) `((from . ,oth-name) (to . ,acc-name) (flow . ,flow)))
              (else #f)))))
        (hash-map->list cons totals-hash))
       #f
       'colorFrom 'red
       'colorTo 'green)
      (gnc:html-document-add-object! document chart))

    document))

;; Here we define the actual report
(gnc:define-report
 'version 1
 'name reportname
 'report-guid "ea5eec0515894cab9d85a9826cfaad7a"
 'menu-path (list gnc:menuname-experimental)
 'options-generator options-generator
 'renderer sankey-renderer)
