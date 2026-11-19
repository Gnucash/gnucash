;; -*- scheme -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-asset-allocation.scm
;; Asset allocation by accounts and holdings regression tests using synthetic books.
;; Copyright (C) 2026 Brent McBride (mcbridebt@hotmail.com)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash reports standard asset-allocation-by-accounts))
(use-modules (gnucash reports standard asset-allocation))
(use-modules (gnucash report stylesheets plain))
(use-modules (tests test-engine-extras))
(use-modules (tests test-report-extras))
(use-modules (tests srfi64-extras))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-13))
(use-modules (srfi srfi-64))
(use-modules (sxml simple))
(use-modules ((sxml xpath) #:select (sxpath)))
(use-modules (ice-9 format))

(setlocale LC_ALL "C")

(define accounts-report-guid "b965c7d638424d7da3be73eb6b32ae18")
(define holdings-report-guid "9cd88bff015943a8b26de06a4cae5d91")

(define (lookup options section name)
  (gnc-optiondb-lookup-value options section name))

(define (set-chart-type! options type)
  (gnc-set-option options "Chart" "Chart Type"
                  (case type
                    ((horizontal-bar grouped-bar) 'bars)
                    ((bar) 'stacked-bar)
                    (else type))))

(define (set-category-data! options definitions)
  (gnc-set-option options "Categories" "Categories"
                  (format #f "~s" definitions)))

(define (restore-options guid options)
  (eval
   (call-with-input-string
       (format #f "(let ((options (gnc:make-report-options ~s))) ~a options)"
               guid (gnc:generate-restore-forms options "options"))
     read)
   (current-module)))

(define (make-options guid)
  (let ((options (gnc:make-report-options guid)))
    (when (string=? guid holdings-report-guid)
      (gnc-set-option options "Accounts" "Accounts"
        (filter (lambda (account)
                  (memv (xaccAccountGetType account)
                        (list ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)))
                (lookup options "Accounts" "Accounts"))))
    (gnc-set-option options "General" "Start Date"
                    (cons 'absolute (gnc-dmy2time64 1 1 2020)))
    (gnc-set-option options "General" "End Date"
                    (cons 'absolute (gnc-dmy2time64 3 1 2020)))
    (gnc-set-option options "General" "Step Size" 'DayDelta)
    options))

(define (render guid options)
  (let* ((report (gnc-report-find (gnc:make-report guid options)))
         (renderer (gnc:report-template-renderer (gnc:find-report-template guid)))
         (document (renderer report)))
    (gnc:html-document-set-style-sheet! document (gnc:report-stylesheet report))
    document))

(define (document-object document predicate)
  (find predicate (map gnc:html-object-data (gnc:html-document-objects document))))

(define (chart document)
  (document-object document gnc:html-chart?))

(define (chart-get document path)
  (and (chart document) (gnc:html-chart-get (chart document) path)))

(define (series document index)
  (chart-get document `(data datasets (,index) data)))

(define (default-currency-format? document)
  (and (string=? (gnc:html-chart-format-style (chart document)) "currency")
       (not (assq-ref (chart-get document '(options)) 'numberFormat))))

(define (table-sxml document)
  (let ((table-document (gnc:make-html-document)))
    (gnc:html-document-set-style-sheet!
     table-document (gnc:html-document-style-sheet document))
    (gnc:html-document-set-objects!
     table-document
     (filter (lambda (object) (gnc:html-table? (gnc:html-object-data object)))
             (gnc:html-document-objects document)))
    (xml->sxml (gnc:html-document-render table-document) #:trim-whitespace? #t)))

(define (row document index)
  (let* ((sxml (table-sxml document))
         (rows ((sxpath '(// table tbody *)) sxml)))
    (if (zero? index)
        ((sxpath '(// th // *text*)) sxml)
        (append-map
         (sxpath '(// *text*))
         ((sxpath '(td))
          (list-ref rows (if (negative? index) (+ (length rows) index) (1- index))))))))

(define (negative-cell-texts document)
  (let ((sxml (table-sxml document)))
    (append-map
     (lambda (cell)
       (if (any (lambda (classes) (member "neg" (string-tokenize classes)))
                ((sxpath '(@ class *text*)) cell))
           ((sxpath '(// *text*)) cell) '()))
     ((sxpath '(// td)) sxml))))

(define (assert-message guid options message)
  (let ((document (render guid options)))
    (test-assert message
      (string-contains (gnc:html-document-render document) message))
    (test-assert "invalid input has no chart" (not (chart document)))))

(define (named-account env name type commodity parent)
  (let ((account (env-create-account env type commodity parent)))
    (xaccAccountSetName account name)
    account))

(define (account-fixture)
  (let* ((book (gnc-get-current-book))
         (env (create-test-env))
         (currency (gnc-default-report-currency))
         (root (gnc-get-current-root-account))
         (assets (named-account env "Patrimoine" ACCT-TYPE-ASSET currency root))
         (parent (named-account env "Parent" ACCT-TYPE-ASSET currency assets))
         (child (named-account env "Child" ACCT-TYPE-BANK currency parent))
         (other (named-account env "Other" ACCT-TYPE-CASH currency assets))
         (equity (named-account env "Equity" ACCT-TYPE-EQUITY currency root)))
    (env-transfer env 1 1 2020 equity parent 100)
    (env-transfer env 1 1 2020 equity child 200)
    (env-transfer env 1 1 2020 equity other 300)
    (list env assets parent child other equity)))

(define (account-definitions . categories)
  (cons 'explicit
        (map (lambda (category)
               (cons (car category) (map gncAccountGetGUID (cdr category))))
             categories)))

(define (test-account-membership)
  (let* ((fixture (account-fixture))
         (env (list-ref fixture 0))
         (assets (list-ref fixture 1))
         (parent (list-ref fixture 2))
         (child (list-ref fixture 3))
         (other (list-ref fixture 4))
         (options (make-options accounts-report-guid))
         (defaults (call-with-input-string
                       (lookup options "Categories" "Categories") read)))
    (test-equal "default category targets 100% and explicitly includes all asset accounts"
      (list 'explicit-targets
            (append '("Uncategorized" 100)
                    (sort (map gncAccountGetGUID
                               (list assets parent child other)) string<?)))
      defaults)
    (let* ((document (render accounts-report-guid options))
           (note (gnc:make-html-document)))
      (test-equal "initial account allocation combines each own balance once"
        #(600) (series document 0))
      (gnc:html-document-set-style-sheet!
       note (gnc:html-document-style-sheet document))
      (gnc:html-document-set-objects!
       note (take (gnc:html-document-objects document) 1))
      (let ((sxml (xml->sxml (gnc:html-document-render note)
                             #:trim-whitespace? #t)))
        (test-equal "account setup link precedes the chart"
          '("Set up your asset allocation")
          ((sxpath '(// p a *text*)) sxml))
        (test-equal "account setup guidance refers to accounts, not holdings"
          ": Create categories, assign accounts, and set target weights."
          (string-trim-both
           (string-concatenate ((sxpath '(// p *text*)) sxml))))
        (test-assert "account setup link opens the Categories tab"
          (any (lambda (url) (string-suffix? "#Categories" url))
               ((sxpath '(// p a @ href *text*)) sxml)))))
    (gnc-set-option options "Chart" "Measure" 'drift)
    (test-equal "initial 100% account target has zero drift"
      #(0) (series (render accounts-report-guid options) 1))
    (gnc-set-option options "Chart" "Measure" 'allocation)
    (set-chart-type! options 'doughnut)
    (let* ((late (named-account env "Later" ACCT-TYPE-BANK
                                (gnc-default-report-currency)
                                (gnc-get-current-root-account)))
           (fresh (gnc:make-report-options accounts-report-guid)))
      (env-transfer env 1 1 2020 (list-ref fixture 5) late 50)
      (test-assert "new defaults include eligible accounts outside Assets"
        (member (gncAccountGetGUID late)
                (cddadr (call-with-input-string
                            (lookup fresh "Categories" "Categories") read))))
      (test-equal "saved defaults retain explicit members after new accounts"
        defaults
        (call-with-input-string
            (lookup (restore-options accounts-report-guid options)
                    "Categories" "Categories") read))
      (test-equal "later accounts do not enter an existing Uncategorized"
        #(600) (series (render accounts-report-guid options) 0)))
    (set-category-data! options
      (list 'explicit-targets
            (list "Uncategorized" 100 (gncAccountGetGUID parent))))
    (test-equal "Uncategorized is not an automatic catch-all for accounts"
      #(100) (series (render accounts-report-guid options) 0))
    (for-each
     (lambda (members)
       (let ((name "<b>Cash & \"Shares\"</b>"))
         (set-category-data! options
           (cons 'explicit (list (cons name (map gncAccountGetGUID members)))))
         (test-equal "category markup is displayed literally, with or without a link"
           name (car (row (render accounts-report-guid options) 1)))
         (gnc-set-option options "Chart" "Time Frame" 'time-series)
         (test-assert "history table headings display category markup literally"
           (member name (row (render accounts-report-guid options) 0)))
         (gnc-set-option options "Chart" "Time Frame" 'snapshot)))
     (list (list parent) (list parent child)))
    (for-each
     (lambda (name)
       (set-category-data! options
         (account-definitions (list name parent child) (list "Custom cash" other)))
       (let ((stored (lookup options "Categories" "Categories")))
         (for-each
          (lambda (view)
            (gnc-set-option options "Chart" "Time Frame" view)
            (let* ((document (render accounts-report-guid options))
                   (history? (eq? view 'time-series)))
              (test-assert "category names remain literal despite matching an account"
                (member name (row document (if history? 0 1))))
              (test-assert "unrelated custom category names remain unchanged"
                (member "Custom cash" (row document (if history? 0 2))))
              (test-equal "chart labels use the literal category name"
                name
                (chart-get document (if history?
                                        '(data datasets (0) label)
                                        '(data labels (0)))))
              (test-equal "rendering never rewrites saved category names" stored
                (lookup options "Categories" "Categories"))
              (test-equal "literal category names survive saved restoration"
                stored
                (lookup (restore-options accounts-report-guid options)
                        "Categories" "Categories"))))
          '(snapshot time-series))))
     (list "Parent" (gnc-account-get-full-name parent)))
    (test-assert "account categories have no full-account-name option"
      (not (gnc-lookup-option options "Display" "Show full account names")))
    (set-category-data! options
      (account-definitions (list "Parent" other child parent)))
    (for-each
     (lambda (view)
       (gnc-set-option options "Chart" "Time Frame" view)
       (let* ((document (render accounts-report-guid options))
        (label "Parent")
              (history? (eq? view 'time-series)))
         (test-assert "literal category names are independent of member order"
           (member label (row document (if history? 0 1))))
         (test-equal "chart names do not resolve matching accounts"
           label
           (chart-get document (if history? '(data datasets (0) label) '(data labels (0)))))))
     '(snapshot time-series))
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (set-category-data! options
      (account-definitions (list "Own balance" parent) (list "Child balance" child)))
    (let ((document (render accounts-report-guid options)))
      (test-equal "selecting a parent does not implicitly include descendants"
        '("Own balance" "$100.00" "33.3%") (row document 1))
      (test-equal "a child can be classified separately from its parent"
        '("Child balance" "$200.00" "66.7%") (row document 2))
      (test-equal "explicit holdings are counted once" #(100 200) (series document 0)))
    (set-category-data! options
      (account-definitions (list "Combined" parent child) (list "Other" other)))
    (let ((document (render accounts-report-guid options)))
      (test-equal "multiple explicit members sum within a category"
        '("Combined" "$300.00" "50.0%") (row document 1))
      (test-equal "total includes each selected account exactly once"
        '("Total" "$600.00" "100.0%") (row document -1)))
    (set-category-data! options '(explicit))
    (assert-message accounts-report-guid options "Create categories and select their accounts")
    (set-category-data! options
      (account-definitions (list "Funded" parent) (list "Empty")))
    (let ((document (render accounts-report-guid options)))
      (test-assert "an empty account category does not prevent an allocation chart"
        (chart document))
      (test-equal "an empty account category has zero value and allocation"
        '("Empty" "$0.00" "0.0%") (row document 2)))
    (test-assert "custom account categories hide initial setup guidance"
      (not (string-contains
            (gnc:html-document-render (render accounts-report-guid options))
            "Set up your asset allocation")))
    (for-each
     (lambda (definitions)
       (set-category-data! options definitions)
       (assert-message accounts-report-guid options "unique, nonempty name"))
     (list '(explicit ("   " "missing"))
           (account-definitions (list "Duplicate" parent) (list "Duplicate" child))
           '(explicit ("Improper" . "missing"))))
    (for-each
     (lambda (definitions)
       (set-category-data! options definitions)
       (assert-message accounts-report-guid options "only one category"))
     (list (account-definitions (list "One" parent parent))
           (account-definitions (list "One" parent) (list "Two" parent))))
    (set-category-data! options
      '(explicit ("Deleted" "0123456789abcdef0123456789abcdef")))
    (assert-message accounts-report-guid options "account that no longer exists")
    (gnc-set-option options "Categories" "Categories" "(")
    (assert-message accounts-report-guid options "unique, nonempty name")
    (gnc-set-option options "Categories" "Categories" "#.(error \"never evaluate\")")
    (assert-message accounts-report-guid options "unique, nonempty name")
    (set-category-data! options (account-definitions (list "Other" other)))
    (gnc-set-option options "Chart" "Time Frame" 'time-series)
    (gnc-set-option options "General" "Start Date"
                    (cons 'absolute (gnc-dmy2time64 4 1 2020)))
    (assert-message accounts-report-guid options "Start Date must not be after End Date")
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (test-equal "snapshot ignores a Start Date after End Date"
      #(300) (series (render accounts-report-guid options) 0))))

(define* (new-security symbol #:optional (namespace "ALLOCATION-TEST"))
  (let* ((book (gnc-get-current-book))
         (security (gnc-commodity-new book symbol namespace symbol "" 1000)))
    (gnc-commodity-table-insert (gnc-commodity-table-get-table book) security)
    security))

(define (add-price security day value)
  (let* ((book (gnc-get-current-book))
         (price (gnc-price-create book)))
    (gnc-price-begin-edit price)
    (gnc-price-set-commodity price security)
    (gnc-price-set-currency price (gnc-default-report-currency))
    (gnc-price-set-time64 price (gnc-dmy2time64 day 1 2020))
    (gnc-price-set-source price PRICE-SOURCE-XFER-DLG-VAL)
    (gnc-price-set-typestr price "test")
    (gnc-price-set-value price value)
    (gnc-price-commit-edit price)
    (gnc-pricedb-add-price (gnc-pricedb-get-db book) price)
    (gnc-price-unref price)))

(define (buy env cash account day value quantity)
  (env-create-multisplit-transaction
   env day 1 2020
   (list (vector cash (- value) (- value)) (vector account value quantity))
   #:pricedb? #f))

(define* (security-fixture #:optional (quote-day 1))
  (let* ((book (gnc-get-current-book))
         (env (create-test-env))
         (currency (gnc-default-report-currency))
         (root (gnc-get-current-root-account))
         (assets (named-account env "Investments" ACCT-TYPE-ASSET currency root))
         (broker (named-account env "Broker" ACCT-TYPE-BANK currency assets))
         (security-a (new-security "AAA"))
         (security-b (new-security "BBB"))
         (stock-a (named-account env "First AAA" ACCT-TYPE-STOCK security-a broker))
         (stock-b (named-account env "Second AAA" ACCT-TYPE-STOCK security-a assets))
         (mutual (named-account env "BBB fund" ACCT-TYPE-MUTUAL security-b broker))
         (currency-stock (named-account env "Currency" ACCT-TYPE-STOCK currency broker))
         (trading (named-account env "Trading" ACCT-TYPE-TRADING security-a root)))
    (buy env broker stock-a 1 20 2)
    (buy env broker stock-b 1 30 3)
    (buy env broker mutual 1 80 4)
    (when quote-day
      (add-price security-a quote-day 10)
      (add-price security-b quote-day 20))
    (list env broker stock-a stock-b mutual security-a security-b currency-stock trading)))

(define (security-definitions . categories)
  (cons 'explicit
        (map (lambda (category)
               (cons (car category) (map gnc-commodity-get-unique-name (cdr category))))
             categories)))

(define (test-saved-default-membership)
  (let* ((fixture (account-fixture))
         (env (list-ref fixture 0))
         (parent (list-ref fixture 2))
         (equity (list-ref fixture 5))
         (options (make-options accounts-report-guid))
         (saved (lookup options "Categories" "Categories"))
         (added (named-account env "Added later" ACCT-TYPE-BANK
                               (gnc-default-report-currency) parent)))
    (env-transfer env 1 1 2020 equity added 1000)
    (let ((restored (restore-options accounts-report-guid options)))
      (test-equal "untouched saved categories do not acquire new descendants"
        saved (lookup restored "Categories" "Categories"))
      (test-equal "restoring untouched defaults preserves allocation values"
        #(600) (series (render accounts-report-guid restored) 0))))
  (gnc-clear-current-session)
  (let* ((fixture (security-fixture))
         (env (list-ref fixture 0))
         (broker (list-ref fixture 1))
         (security (list-ref fixture 5))
         (options (make-options holdings-report-guid))
         (saved (map gncAccountGetGUID (lookup options "Accounts" "Accounts")))
         (categories (lookup options "Categories" "Categories"))
         (added (named-account env "Added later" ACCT-TYPE-STOCK security broker)))
    (buy env broker added 1 1000 100)
    (let ((restored (restore-options holdings-report-guid options)))
      (test-equal "untouched saved holdings exclude accounts created later"
        saved (map gncAccountGetGUID (lookup restored "Accounts" "Accounts")))
      (test-equal "untouched saved holding categories remain explicit"
        categories (lookup restored "Categories" "Categories"))))
  (gnc-clear-current-session))

(define (test-holding-defaults)
  (define (customization-links document)
    (let ((link-document (gnc:make-html-document)))
      (gnc:html-document-set-style-sheet! link-document (gnc:html-document-style-sheet document))
      (gnc:html-document-set-objects! link-document
        (filter (lambda (object) (not (gnc:html-chart? (gnc:html-object-data object))))
                (gnc:html-document-objects document)))
      (filter (lambda (anchor)
                (equal? ((sxpath '(*text*)) anchor) '("Set up your asset allocation")))
              ((sxpath '(// a))
               (xml->sxml (gnc:html-document-render link-document) #:trim-whitespace? #t)))))
  (define (assert-setup-note document)
    (let ((note-document (gnc:make-html-document)))
      (gnc:html-document-set-style-sheet! note-document (gnc:html-document-style-sheet document))
      (gnc:html-document-set-objects! note-document
        (take (gnc:html-document-objects document) 1))
      (let ((sxml (xml->sxml (gnc:html-document-render note-document) #:trim-whitespace? #t)))
        (test-equal "the default setup link is in the first paragraph above the chart"
          '("Set up your asset allocation") ((sxpath '(// p a *text*)) sxml))
        (test-equal "the default setup note explains category customization"
          ": Create categories, assign holdings, and set target weights."
          (string-trim-both (string-concatenate ((sxpath '(// p *text*)) sxml)))))
      (test-equal "the default setup has exactly one customization link"
        1 (length (customization-links document)))))
  (let* ((fixture (security-fixture))
         (env (car fixture))
         (broker (list-ref fixture 1))
         (assets (gnc-account-get-parent broker))
         (currency (gnc-default-report-currency))
         (fund (new-security "CCC" "FUND"))
         (stock (new-security "DDD" "NASDAQ"))
         (fund-account (named-account env "Fund" ACCT-TYPE-MUTUAL fund assets))
         (stock-account (named-account env "Shares" ACCT-TYPE-STOCK stock assets))
         (cash (named-account env "Cash" ACCT-TYPE-CASH currency assets))
         (equity (named-account env "Funding" ACCT-TYPE-EQUITY currency
                                (gnc-get-current-root-account))))
    (new-security "Unused" "UNUSED")
    (env-transfer env 1 1 2020 equity broker 500)
    (env-transfer env 1 1 2020 broker cash 25)
    (buy env broker fund-account 1 40 2)
    (buy env broker stock-account 1 30 3)
    (add-price fund 1 20)
    (add-price stock 1 10)
    (let* ((options (gnc:make-report-options holdings-report-guid))
           (accounts (lookup options "Accounts" "Accounts"))
           (definitions (lookup options "Categories" "Categories")))
      (gnc-set-option options "General" "End Date"
                      (cons 'absolute (gnc-dmy2time64 3 1 2020)))
      (test-equal "first-run accounts include all eligible cash and security accounts"
        (sort (map gncAccountGetGUID
                   (append (list assets broker cash fund-account stock-account)
                           (take (drop fixture 2) 3))) string<?)
        (sort (map gncAccountGetGUID accounts) string<?))
      (test-equal "first-run Uncategorized targets 100% and includes each eligible holding once"
        (list 'explicit-targets
              (append '("Uncategorized" 100)
                      (sort (map gnc-commodity-get-unique-name
                                 (list (list-ref fixture 5) (list-ref fixture 6)
                                       currency fund stock)) string<?)))
        (call-with-input-string definitions read))
      (let ((document (render holdings-report-guid options)))
        (assert-setup-note document)
        (test-equal "first-run chart combines all eligible cash and security holdings"
          #(500) (series document 0))
        (test-equal "parent accounts do not double-count descendant balances"
          '("Total" "$500.00" "100.0%") (row document -1))
        (test-assert "default setup links directly to the Categories options tab"
          (any (lambda (anchor)
                 (any (lambda (url)
                        (and (string-prefix? "gnc-options:report-id=" url)
                             (string-suffix? "#Categories" url)))
                      ((sxpath '(@ href *text*)) anchor)))
               (customization-links document))))
      (gnc-set-option options "Chart" "Measure" 'drift)
      (let ((document (render holdings-report-guid options)))
        (assert-setup-note document)
        (test-equal "first-run drift renders zero for the 100% Uncategorized target"
          #(0) (series document 1))
        (test-assert "first-run drift has no missing-target warning"
          (not (string-contains (gnc:html-document-render document)
                                "Allocation targets are missing")))
        (test-equal "default drift retains the customization link"
          1 (length (customization-links document))))
      (gnc-set-option options "Chart" "Measure" 'allocation)
      (let* ((entry (cadr (call-with-input-string definitions read)))
             (members (cddr entry)))
        (for-each
         (lambda (uncategorized)
           (set-category-data! options uncategorized)
           (let ((document (render holdings-report-guid options)))
             (assert-setup-note document)
             (test-equal "unassigned holdings still fall back into the sole Uncategorized category"
               #(1) (series document 0))
             (test-equal "clearing category membership does not exclude selected account balances"
               '("Total" "$500.00" "100.0%") (row document -1)))
           (test-equal "rendering the default-like setup preserves its explicit holding selection"
             uncategorized (call-with-input-string (lookup options "Categories" "Categories") read))
           (test-equal "cleared or partial Uncategorized memberships survive saved restoration"
             uncategorized
             (call-with-input-string
                 (lookup (restore-options holdings-report-guid options) "Categories" "Categories") read)))
         (list (list 'explicit-targets (append '("Uncategorized" 100.0) (reverse members)))
               (list 'explicit-targets (append '("Uncategorized" 100) (cdr members)))
               '(explicit-targets ("Uncategorized" 100))))
        (gnc-set-option options "Chart" "Measure" 'drift)
        (let ((document (render holdings-report-guid options)))
          (assert-setup-note document)
          (test-equal "cleared Uncategorized membership still shows zero drift and setup guidance"
            #(0) (series document 1)))
        (gnc-set-option options "Chart" "Measure" 'allocation)
        (for-each
         (lambda (customized)
           (set-category-data! options customized)
           (test-equal "customized categories do not show the default setup link"
             '() (customization-links (render holdings-report-guid options)))
           (test-equal "saved custom categories and targets survive the new default"
             customized
             (call-with-input-string
                 (lookup (restore-options holdings-report-guid options) "Categories" "Categories") read)))
         (list (list 'explicit-targets (cons "My portfolio" (cdr entry)))
               (list 'explicit-targets (append '("Uncategorized" 50) members))
               (security-definitions
                (list "ALLOCATION-TEST" (list-ref fixture 5) (list-ref fixture 6))
                (list "Currencies" currency) (list "FUND" fund) (list "NASDAQ" stock)))))
      (gnc-set-option options "Categories" "Categories" definitions)
      (named-account env "Added later" ACCT-TYPE-STOCK
                     (new-security "LATE" "NEW-NAMESPACE") assets)
      (let ((restored (restore-options holdings-report-guid options)))
        (test-equal "saved default category and target do not acquire later holdings"
          definitions (lookup restored "Categories" "Categories"))
        (test-equal "saved default account selection excludes accounts created later"
          (map gncAccountGetGUID accounts)
          (map gncAccountGetGUID (lookup restored "Accounts" "Accounts"))))))
  (gnc-clear-current-session)
  (let* ((fixture (account-fixture))
         (options (gnc:make-report-options holdings-report-guid)))
    (gnc-set-option options "General" "End Date"
                    (cons 'absolute (gnc-dmy2time64 3 1 2020)))
    (test-equal "cash-only books default to Uncategorized with a 100% target"
      (list 'explicit-targets
            (list "Uncategorized" 100 (gnc-commodity-get-unique-name (gnc-default-report-currency))))
      (call-with-input-string (lookup options "Categories" "Categories") read))
    (test-equal "cash-only first-run chart includes each account's own balance once"
      #(600) (series (render holdings-report-guid options) 0))
    (test-equal "cash-only default setup also offers category customization"
      1 (length (customization-links (render holdings-report-guid options)))))
  (gnc-clear-current-session)
  (let ((options (gnc:make-report-options holdings-report-guid)))
    (test-equal "books without eligible accounts do not show a category customization link"
      '() (customization-links (render holdings-report-guid options)))))

(define (test-saved-fallback-language)
  (let* ((fixture (security-fixture))
         (options (make-options holdings-report-guid))
         (module (resolve-module '(gnucash reports standard asset-allocation)))
         (translate (module-ref module 'G_)))
    (set-category-data! options '(explicit-targets ("Uncategorized" 100)))
    (gnc-set-option options "Chart" "Measure" 'drift)
    (dynamic-wind
      (lambda ()
        (module-set! module 'G_
          (lambda (text)
            (if (string=? text "Uncategorized")
                "Sans categorie" (translate text)))))
      (lambda ()
        (let* ((restored (restore-options holdings-report-guid options))
               (document (render holdings-report-guid restored)))
          (test-equal "the saved fallback key survives a language change"
            "Uncategorized"
            (lookup restored "Categories" "Uncategorized Category"))
          (test-equal "unassigned holdings retain the saved fallback target"
            #(0) (series document 1))
          (test-equal "a language change does not add a targetless category"
            '("Uncategorized" "100.00%" "100.00%" "0.00%"
              "$130.00" "$130.00" "$0.00")
            (row document 1))
          (for-each
           (lambda (name)
             (GncOption-reset-default-value
              (gnc-lookup-option restored "Categories" name)))
           '("Categories" "Uncategorized Category"))
          (test-equal "reset defaults restores a nonempty current-language key"
            "Sans categorie"
            (lookup restored "Categories" "Uncategorized Category"))
          (test-equal "reset defaults keeps fallback membership and targets aligned"
            #(0) (series (render holdings-report-guid restored) 1))))
      (lambda () (module-set! module 'G_ translate)))))

(define (test-security-membership)
  (let* ((fixture (security-fixture))
         (broker (list-ref fixture 1))
         (stock-a (list-ref fixture 2))
         (stock-b (list-ref fixture 3))
         (mutual (list-ref fixture 4))
         (security-a (list-ref fixture 5))
         (security-b (list-ref fixture 6))
         (options (make-options holdings-report-guid))
         (defaults (lookup (gnc:make-report-options holdings-report-guid)
                           "Accounts" "Accounts"))
         (definitions (security-definitions
                       '("Uncategorized") (list "Equity" security-a) (list "Bonds" security-b))))
    (test-equal "security-only fixture excludes parents, currency accounts and Trading"
      (sort (map gncAccountGetGUID (list stock-a stock-b mutual)) string<?)
      (sort (map gncAccountGetGUID (lookup options "Accounts" "Accounts")) string<?))
    (test-equal "default Uncategorized deduplicates securities and currency holdings"
      (list 'explicit-targets
            (append '("Uncategorized" 100)
                    (sort (map gnc-commodity-get-unique-name
                               (list security-a security-b (gnc-default-report-currency))) string<?)))
      (call-with-input-string (lookup options "Categories" "Categories") read))
    (set-category-data! options definitions)
    (let ((document (render holdings-report-guid options)))
      (test-equal "empty Uncategorized is allowed and has a zero table value"
        '("Uncategorized" "$0.00" "0.0%") (row document 1))
      (test-equal "same security is combined across selected accounts"
        '("Equity" "$50.00" "38.5%") (row document 2))
      (test-equal "mutual funds are included"
        '("Bonds" "$80.00" "61.5%") (row document 3))
      (test-equal "snapshot omits zero-valued categories" #(50 80) (series document 0))
      (test-equal "rendering does not modify security category definitions"
        definitions (call-with-input-string (lookup options "Categories" "Categories") read)))
    (gnc-set-option options "Accounts" "Accounts" (list stock-a
      (list-ref fixture 7)))
    (test-equal "subset ignores currency-denominated stock accounts"
      #(20) (series (render holdings-report-guid options) 0))
    (let ((equity (named-account (car fixture) "Funding" ACCT-TYPE-EQUITY
                                (gnc-default-report-currency) (gnc-get-current-root-account))))
      (env-create-multisplit-transaction (car fixture) 1 1 2020
        (list (vector broker 200 200) (vector equity -200 -200)) #:pricedb? #f))
    (set-category-data! options
      (security-definitions (list "Equity" security-a)
                            (list "Cash" (gnc-default-report-currency))))
    (gnc-set-option options "Accounts" "Accounts" (list stock-a broker))
    (test-equal "cash is included only from explicitly selected accounts"
      #(20 70) (series (render holdings-report-guid options) 0))
    (test-equal "currency categories survive saved-option restoration"
      #(20 70) (series (render holdings-report-guid (restore-options holdings-report-guid options)) 0))
    (gnc-set-option options "Accounts" "Accounts" (list stock-a))
    (test-equal "changing accounts does not remove a saved currency category"
      '("Cash" "$0.00" "0.0%") (row (render holdings-report-guid options) 2))
    (gnc-set-option options "Accounts" "Accounts" (list broker))
    (test-equal "cash-only selections are supported"
      #(70) (series (render holdings-report-guid options) 0))
    (let* ((euro (gnc-commodity-table-lookup
                  (gnc-commodity-table-get-table (gnc-get-current-book)) "CURRENCY" "EUR"))
           (foreign-cash (named-account (car fixture) "Euro Cash" ACCT-TYPE-BANK euro
                                        (gnc-get-current-root-account))))
      (add-price euro 1 2)
      (buy (car fixture) broker foreign-cash 1 20 10)
      (set-category-data! options
        (security-definitions (list "Equity" security-a)
                              (list "Domestic Cash" (gnc-default-report-currency))
                              (list "Foreign Cash" euro)))
      (gnc-set-option options "Accounts" "Accounts" (list stock-a broker foreign-cash))
      (test-equal "distinct currency categories convert cash to the report currency"
        #(20 50 20) (series (render holdings-report-guid options) 0))
      (test-equal "foreign currency accounts and assignments survive reopening"
        #(20 50 20) (series (render holdings-report-guid (restore-options holdings-report-guid options)) 0))
      (gnc-set-option options "Chart" "Time Frame" 'time-series)
      (let ((document (render holdings-report-guid options)))
        (test-equal "domestic cash history uses currency balances"
          #(5/9 5/9 5/9) (series document 1))
        (test-equal "foreign cash history uses converted balances"
          #(2/9 2/9 2/9) (series document 2)))
      (gnc-set-option options "Chart" "Time Frame" 'snapshot))
    (set-category-data! options definitions)
    (gnc-set-option options "Accounts" "Accounts" '())
    (test-equal "Clear All uses the default accounts like other reports"
      defaults (lookup options "Accounts" "Accounts"))
    (test-equal "Clear All renders the default account allocation"
      #(50 50 80) (series (render holdings-report-guid options) 0))
    (let ((restored (restore-options holdings-report-guid options)))
      (test-equal "Clear All preserves the default accounts after save/reopen"
        defaults (lookup restored "Accounts" "Accounts"))
      (test-equal "categories survive save/reopen after Clear All"
        definitions (call-with-input-string (lookup restored "Categories" "Categories") read)))
    (gnc-set-option options "Accounts" "Accounts" (list stock-b mutual))
    (let* ((restored (restore-options holdings-report-guid options))
           (document (render holdings-report-guid restored)))
      (test-equal "saved subset and security categories render correctly after reopen"
        #(30 80) (series document 0)))
    (gnc-set-option options "Accounts" "Accounts" (list stock-a stock-b mutual))
    (set-category-data! options (security-definitions (list "Equity" security-a)))
    (test-equal "unassigned securities get an appended Uncategorized category"
      '("Uncategorized" "$80.00" "61.5%") (row (render holdings-report-guid options) 2))
    (set-category-data! options
      (security-definitions (list "Uncategorized" security-a)))
    (let ((document (render holdings-report-guid options)))
      (test-equal "unassigned holdings merge into existing Uncategorized"
        '("Uncategorized" "$130.00" "100.0%") (row document 1))
      (test-equal "merge does not create another category"
        '("Total" "$130.00" "100.0%") (row document 2)))
    (set-category-data! options '(explicit))
    (test-equal "no explicit security categories groups everything as Uncategorized"
      #(130) (series (render holdings-report-guid options) 0))
    (for-each
     (lambda (definitions)
       (set-category-data! options definitions)
       (assert-message holdings-report-guid options "unique, nonempty name"))
     (list '(explicit ("   ")) '(explicit ("Same") ("Same"))
           '(explicit ("Bad" 42)) '(explicit ("Bad" . "AAA"))))
    (for-each
     (lambda (definitions)
       (set-category-data! options definitions)
       (assert-message holdings-report-guid options "only one category"))
     (list (security-definitions (list "One" security-a security-a))
           (security-definitions (list "One" security-a) (list "Two" security-a))
           (security-definitions (list "One" (gnc-default-report-currency))
                 (list "Two" (gnc-default-report-currency)))))
    (for-each
     (lambda (definitions)
       (set-category-data! options definitions)
       (assert-message holdings-report-guid options "holding that no longer exists"))
     (list '(explicit ("Missing" "ALLOCATION-TEST::MISSING"))))
    (gnc-set-option options "Categories" "Categories" "(")
    (assert-message holdings-report-guid options "unique, nonempty name")))

(define (test-targets guid)
  (let* ((security? (string=? guid holdings-report-guid))
         (fixture (if security? (security-fixture) (account-fixture)))
         (options (make-options guid))
         (definitions
          (if security?
              (security-definitions (list "First" (list-ref fixture 5))
                                    (list "Second" (list-ref fixture 6)))
              (account-definitions (list "First" (list-ref fixture 2))
                                   (list "Second" (list-ref fixture 3) (list-ref fixture 4))))))
    (for-each
     (lambda (targets)
       (let ((data (cons 'explicit-targets
                         (map (lambda (entry target)
                                (cons (car entry) (cons target (cdr entry))))
                              (cdr definitions) targets))))
         (set-category-data! options data)
         (let ((stored (lookup options "Categories" "Categories"))
               (document (render guid options)))
           (test-assert "optional targets do not block ordinary allocation" (chart document))
           (test-equal "target metadata does not change category membership"
             (if security? #(50 80) #(100 500)) (series document 0))
           (test-equal "rendering preserves targets" stored
             (lookup options "Categories" "Categories"))
           (test-equal "targets survive saved-option restoration" stored
             (lookup (restore-options guid options) "Categories" "Categories"))
           (test-equal "ordinary allocation does not add target or difference columns"
             3 (length (row document 0)))
           (test-assert "ordinary allocation does not produce drift warnings"
             (not (string-contains (gnc:html-document-render document) "Allocation Drift"))))))
     '((60 40) (0 100) (#f #f) (60 #f) (20 30)))
    (unless security?
      (for-each
       (lambda (name)
         (set-category-data! options
           (account-definitions
            (list name (list-ref fixture 2) (list-ref fixture 3))
            (list "Custom cash" (list-ref fixture 4))))
         (set-targets! options '(60 40))
         (gnc-set-option options "Chart" "Measure" 'drift)
         (for-each
          (lambda (view)
            (gnc-set-option options "Chart" "Time Frame" view)
            (let ((document (render guid options)))
              (test-assert "literal account category names retain their targets"
                (chart document))
              (when (chart document)
                (test-equal "account drift is independent of matching names"
                  (if (eq? view 'snapshot) #(-1/10 1/10)
                      #(-1/10 -1/10 -1/10))
                  (series document 0)))))
          '(snapshot time-series)))
       (list "Parent" (gnc-account-get-full-name (list-ref fixture 2)))))
    (for-each
     (lambda (target)
       (set-category-data! options
         (list 'explicit-targets (list "Invalid" target)))
       (assert-message guid options "unique, nonempty name"))
     '(-1 101 +inf.0 +nan.0 "60"))))

(define (set-targets! options targets)
  (let ((data (allocation-read-category-data (lookup options "Categories" "Categories"))))
    (set-category-data! options
      (cons 'explicit-targets
            (map (lambda (entry target) (cons (car entry) (cons target (cdr entry))))
                 (cdr data) targets)))))

(define (test-drift guid)
  (let* ((security? (string=? guid holdings-report-guid))
         (fixture (if security? (security-fixture) (account-fixture)))
         (options (make-options guid))
         (amounts (if security? '(50 80) '(300 300)))
         (total (apply + amounts))
         (expected (map (lambda (amount target) (- (* 100 (/ amount total)) target))
                        amounts '(60 40))))
    (set-category-data! options
      (if security?
          (security-definitions (list "First" (list-ref fixture 5))
                                (list "Second" (list-ref fixture 6)))
          (account-definitions (list "First" (list-ref fixture 2) (list-ref fixture 3))
                               (list "Second" (list-ref fixture 4)))))
    (set-targets! options '(60 40))
    (gnc-set-option options "Chart" "Measure" 'drift)
    (set-chart-type! options 'horizontal-bar)
    (let ((document (render guid options)))
      (test-equal "snapshot drift uses vertical bars" 'bar (gnc:html-chart-type (chart document)))
      (test-equal "snapshot drift bar orientation" "x" (chart-get document '(options indexAxis)))
      (test-equal "drift tooltip percentages are signed differences"
        (list->vector (map (lambda (value) (/ value 100)) expected))
        (chart-get document '(data datasets (0) allocationPercentages)))
      (test-equal "drift tooltip amounts are signed differences in currency"
        (if security? #(-28 28) #(-60 60))
        (chart-get document '(data datasets (0) allocationAmounts)))
      (test-assert "snapshot drift uses the report tooltip renderer"
        (let ((object (find (lambda (object) (eq? (gnc:html-object-data object) (chart document)))
                            (gnc:html-document-objects document))))
          (eq? (gnc:html-object-renderer object)
               (@@ (gnucash reports standard asset-allocation)
                   allocation-chart-render))))
      (test-equal "snapshot drift categories are on the x-axis" 'category
        (chart-get document '(options scales x type)))
      (test-equal "snapshot drift names its category axis" "Category"
        (chart-get document '(options scales x title text)))
      (test-equal "snapshot drift category axis title is readable" 14
        (chart-get document '(options scales x title font size)))
      (test-equal "snapshot drift numeric axis title matches the category title size" 14
        (chart-get document '(options scales y title font size)))
      (test-equal "snapshot drift percentages are on the y-axis" "linear"
        (chart-get document '(options scales y type)))
      (test-equal "snapshot drift keeps both positive and negative deviations"
        (list->vector (map (lambda (value) (/ value 100)) expected)) (series document 0))
      (test-equal "snapshot drift retains category colors regardless of sign"
        (list->vector (gnc:assign-colors (length expected)))
        (chart-get document '(data datasets (0) backgroundColor)))
      (test-equal "drift y-axis ticks follow number-cell stylesheet styles" #t
        (chart-get document '(options scales y ticks useNumberCellStyle)))
      (test-assert "drift ticks no longer hardcode a negative color"
        (not (assq 'negativeColor (chart-get document '(options scales y ticks)))))
      (test-assert "rendered chart selects tick-label color from its signed value"
        (string-contains (string-concatenate (gnc:html-chart-render (chart document) document))
                         "context.tick.value < 0"))
      (test-assert "rendered chart derives number-cell styling from CSS"
        (string-contains (string-concatenate (gnc:html-chart-render (chart document) document))
                         "window.getComputedStyle(numberCells"))
      (test-equal "snapshot drift formats fractions as percentages" "percent"
        (gnc:html-chart-format-style (chart document)))
      (test-equal "snapshot drift axis describes the comparison" "Drift from Target"
        (chart-get document '(options scales y title text)))
      (test-assert "snapshot drift y-axis uses the shared percentage formatter"
        (string-contains (string-concatenate (gnc:html-chart-render (chart document) document))
                         "chartjsoptions.options.scales.y.ticks.callback = yAxisDisplay"))
      (test-assert "snapshot drift x-axis uses category labels"
        (string-contains (string-concatenate (gnc:html-chart-render (chart document) document))
                         "chartjsoptions.options.scales.x.ticks.callback = xAxisDisplay"))
      (test-assert "percentage y-axis ticks use number formatting, including zero"
        (string-contains (string-concatenate (gnc:html-chart-render (chart document) document))
                         "function yAxisDisplay(value,index,values) { return numformat(value); }"))
      (test-equal "snapshot drift tooltips retain two-decimal precision" 2
        (chart-get document '(options numberFormat maximumFractionDigits)))
      (test-equal "snapshot drift tooltip describes the comparison" "Drift from Target"
        (chart-get document '(data datasets (0) label)))
      (test-equal "snapshot drift table has targets and deviations"
        '("Category" "Target %" "Actual %" "Difference %"
          "Target Value" "Actual Value" "Difference") (row document 0))
      (test-equal "snapshot drift shows target values and negative differences"
        (if security?
            '("First" "60.00%" "38.46%" "-21.54%" "$78.00" "$50.00" "-$28.00")
            '("First" "60.00%" "50.00%" "-10.00%" "$360.00" "$300.00" "-$60.00"))
        (row document 1))
      (test-equal "snapshot drift shows positive value and percentage-point differences"
        (if security?
            '("Second" "40.00%" "61.54%" "21.54%" "$52.00" "$80.00" "$28.00")
            '("Second" "40.00%" "50.00%" "10.00%" "$240.00" "$300.00" "$60.00"))
        (row document 2))
      (test-assert "negative percentage differences use the negative-value style"
        (member (format #f "~,2f%" (car expected)) (negative-cell-texts document)))
      (test-assert "positive percentage differences do not use the negative-value style"
        (not (member (format #f "~,2f%" (cadr expected)) (negative-cell-texts document))))
      (test-equal "drift axis is symmetric around zero"
        (- (chart-get document '(options scales y suggestedMax)))
        (chart-get document '(options scales y suggestedMin))))
    (gnc-set-option options "Chart" "Units" 'value)
    (let ((document (render guid options)))
      (test-equal "snapshot monetary drift plots surplus and shortfall"
        (if security? #(-28 28) #(-60 60)) (series document 0))
      (test-assert "snapshot monetary drift uses default currency formatting"
        (default-currency-format? document))
      (test-equal "snapshot monetary drift suggests a symmetric currency range"
        (if security? 28 60) (chart-get document '(options scales y suggestedMax)))
      (test-equal "snapshot monetary drift keeps its negative suggested range"
        (- (chart-get document '(options scales y suggestedMax)))
        (chart-get document '(options scales y suggestedMin)))
      (test-assert "monetary drift leaves endpoint rounding to Chart.js"
        (let ((axis (chart-get document '(options scales y))))
          (and (not (assq 'min axis)) (not (assq 'max axis)))))
      (test-equal "snapshot monetary drift retains the percentage columns"
        '("Category" "Target %" "Actual %" "Difference %"
          "Target Value" "Actual Value" "Difference") (row document 0))
      (test-equal "saved snapshot drift retains the selected Values mode" 'value
        (lookup (restore-options guid options) "Chart" "Units")))
    (gnc-set-option options "Chart" "Units" 'percent)
    (for-each
     (lambda (targets)
       (set-targets! options targets)
       (for-each
        (lambda (mode)
          (gnc-set-option options "Chart" "Units" mode)
          (test-assert "editor-style inexact targets render complete drift HTML in either mode"
            (string-contains (gnc:html-document-render (render guid options)) "Target Value")))
        '(value percent)))
     '((60.0 40.0) (56.3 43.7) (0.0 100.0) (0.0000000001 99.9999999999)))
    (set-targets! options '(56.3 43.7))
    (test-equal "fractional editor targets round currency values and differences correctly"
      (if security?
          '("First" "56.30%" "38.46%" "-17.84%" "$73.19" "$50.00" "-$23.19")
          '("First" "56.30%" "50.00%" "-6.30%" "$337.80" "$300.00" "-$37.80"))
      (row (render guid options) 1))
    (set-targets! options '(60 40))
    (test-equal "snapshot drift saves its bars selection" 'bars
      (lookup (restore-options guid options) "Chart" "Chart Type"))
    (test-equal "drift measure survives saved-option restoration" 'drift
      (lookup (restore-options guid options) "Chart" "Measure"))
    (gnc-set-option options "Chart" "Time Frame" 'time-series)
    (gnc-set-option options "Chart" "Measure" 'drift)
    (set-chart-type! options 'unstacked-line)
    (for-each
     (lambda (values-mode)
       (gnc-set-option options "Chart" "Units" values-mode)
       (let ((document (render guid options)))
         (test-equal "history drift uses lines" 'line (gnc:html-chart-type (chart document)))
         (test-equal "drift lines are not stacked" #f (chart-get document '(options scales y stacked)))
         (test-equal "drift lines are not filled" #f (chart-get document '(data datasets (0) fill)))
         (test-equal "drift legend can wrap below the plot" 'bottom
           (chart-get document '(options plugins legend position)))
         (test-equal "history drift axis describes the comparison" "Drift from Target"
           (chart-get document '(options scales y title text)))
         (test-equal "history drift formats ticks and tooltips in the selected units"
           (if (eq? values-mode 'percent) "percent" "currency")
           (gnc:html-chart-format-style (chart document)))
         (if (eq? values-mode 'percent)
             (test-equal "history percentage drift retains two-decimal precision" 2
               (chart-get document '(options numberFormat maximumFractionDigits)))
             (test-assert "history monetary drift keeps default currency precision"
               (default-currency-format? document)))
         (test-equal "history drift y-axis ticks follow number-cell stylesheet styles" #t
           (chart-get document '(options scales y ticks useNumberCellStyle)))
         (for-each
          (lambda (index value amount)
            (test-equal "history drift plots the selected percentage or monetary difference"
              (make-vector 3 (if (eq? values-mode 'percent) (/ value 100) amount))
              (series document index)))
          '(0 1) expected (if security? '(-28 28) '(-60 60)))
         (test-equal "history drift table labels its units"
           '("Date / Difference %" "First" "Second") (row document 0))
         (test-equal "history differences use the same percentage format as snapshots"
           (cons "2020-01-01" (map (lambda (value) (format #f "~,2f%" value)) expected))
           (row document 1))
         (test-equal "negative history drift values use the negative-value style"
           (make-list 3 (format #f "~,2f%" (car expected)))
           (negative-cell-texts document))))
     '(percent value))
    (test-equal "history drift saves its unstacked lines selection" 'unstacked-line
      (lookup (restore-options guid options) "Chart" "Chart Type"))
    (for-each
     (lambda (targets expected-message)
       (set-targets! options targets)
       (assert-message guid options expected-message)
       (let ((document (render guid options)))
         (test-equal "invalid targets retain the history drift table"
           '("Date / Difference %" "First" "Second") (row document 0))
         (when (any not targets)
           (test-equal "missing history targets are listed as separate bullets in category order"
             (filter-map (lambda (name target) (and (not target) name))
                         '("First" "Second") targets)
             ((sxpath '(// ul li // *text*))
              (xml->sxml (gnc:html-document-render document) #:trim-whitespace? #t))))
         (test-assert "invalid target warning links to report options"
           (string-contains (gnc:html-document-render document) "gnc-options:report-id="))))
     '((#f #f) (60 #f) (60 30) (60 50) (60 41) (60.0 41.0) (0 0)
       (60 40.00001) (60.0 40.99999999999999) (60 41.5) (60 41.25))
     '("Allocation targets are missing for these categories:"
       "Allocation targets are missing for these categories:"
       "Allocation targets total 90%, but must total 100%"
       "Allocation targets total 110%, but must total 100%"
       "Allocation targets total 101%, but must total 100%"
       "Allocation targets total 101%, but must total 100%"
       "Allocation targets total 0%, but must total 100%"
       "Allocation targets total 100.00001%, but must total 100%"
       "Allocation targets total 101%, but must total 100%"
       "Allocation targets total 101.5%, but must total 100%"
       "Allocation targets total 101.25%, but must total 100%"))
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (for-each
     (lambda (targets expected-row)
       (set-targets! options targets)
       (let ((document (render guid options)))
         (test-assert "invalid snapshot targets still suppress the chart" (not (chart document)))
         (when (any not targets)
           (test-equal "missing snapshot targets are listed as separate bullets in category order"
             (filter-map (lambda (name target) (and (not target) name))
                         '("First" "Second") targets)
             ((sxpath '(// ul li // *text*))
              (xml->sxml (gnc:html-document-render document) #:trim-whitespace? #t))))
         (test-equal "invalid snapshot targets retain all drift columns"
           '("Category" "Target %" "Actual %" "Difference %"
             "Target Value" "Actual Value" "Difference") (row document 0))
         (test-equal "invalid snapshot targets retain entered targets or blanks and actual values"
           expected-row (row document 2))))
     '((60 41) (60 #f) (#f #f))
     (if security?
         '(("Second" "41.00%" "61.54%" "20.54%" "$53.30" "$80.00" "$26.70")
           ("Second" "61.54%" "$80.00") ("Second" "61.54%" "$80.00"))
         '(("Second" "41.00%" "50.00%" "9.00%" "$246.00" "$300.00" "$54.00")
           ("Second" "50.00%" "$300.00") ("Second" "50.00%" "$300.00"))))
    (let* ((definitions (call-with-input-string (lookup options "Categories" "Categories") read))
           (name "<b>Cash & \"Shares\"</b>"))
      (set-category-data! options
        (cons (car definitions)
              (cons (cons name (cdadr definitions)) (cddr definitions))))
      (set-targets! options '(#f 100))
      (test-equal "missing-target bullets display category markup literally"
        (list name)
        ((sxpath '(// ul li // *text*))
         (xml->sxml (gnc:html-document-render (render guid options)) #:trim-whitespace? #t)))
      (set-category-data! options definitions))
    (set-targets! options '(0 100))
    (test-assert "an explicit zero target is valid for drift" (chart (render guid options)))
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (test-equal "a zero target has zero target value and the full actual value as its difference"
      (if security?
          '("First" "0.00%" "38.46%" "38.46%" "$0.00" "$50.00" "$50.00")
          '("First" "0.00%" "50.00%" "50.00%" "$0.00" "$300.00" "$300.00"))
      (row (render guid options) 1))
    (gnc-set-option options "Chart" "Time Frame" 'time-series)
    (set-targets! options (map (lambda (amount) (* 100 (/ amount total))) amounts))
    (test-equal "a perfectly on-target allocation still renders a zero drift chart"
      #(0 0 0) (series (render guid options) 0))
    (when security?
      (set-category-data! options
        (security-definitions (list "First" (list-ref fixture 5))))
      (set-targets! options '(100))
      (assert-message guid options "Allocation targets are missing for these categories:"))
    (set-category-data! options
      (if security?
        (security-definitions (list "First" (list-ref fixture 5))
                              (list "Uncategorized" (list-ref fixture 6))
                              (list "Empty"))
        (account-definitions (list "First" (list-ref fixture 2) (list-ref fixture 3))
                             (list "Second" (list-ref fixture 4))
                             (list "Empty"))))
    (set-targets! options '(40 40 20))
    (for-each
     (lambda (mode expected)
       (gnc-set-option options "Chart" "Units" mode)
       (test-equal "an empty category's drift is its negative target in the selected units"
         expected (series (render guid options) 2)))
     '(percent value)
     (list #(-1/5 -1/5 -1/5)
           (if security? #(-26 -26 -26) #(-120 -120 -120))))
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (test-equal "an empty category retains its target and negative drift in the snapshot table"
      (if security?
          '("Empty" "20.00%" "0.00%" "-20.00%" "$26.00" "$0.00" "-$26.00")
          '("Empty" "20.00%" "0.00%" "-20.00%" "$120.00" "$0.00" "-$120.00"))
      (row (render guid options) 3))
    (gnc-set-option options "Display" "Show Table" #f)
    (test-assert "drift respects Show Table"
      (not (document-object (render guid options) gnc:html-table?)))))

(define (test-chart-modes guid)
  (let* ((security? (string=? guid holdings-report-guid))
         (fixture (if security? (security-fixture) (account-fixture)))
         (options (make-options guid))
         (amounts (if security? '(50 80) '(300 300)))
         (total (apply + amounts)))
    (set-category-data! options
      (if security?
          (security-definitions (list "First" (list-ref fixture 5))
                                (list "Second" (list-ref fixture 6)))
          (account-definitions (list "First" (list-ref fixture 2) (list-ref fixture 3))
                               (list "Second" (list-ref fixture 4)))))
    (gnc-set-option options "Chart" "Units" 'value)
    (test-equal "snapshot tooltip defaults to name, percentage and value"
      'name-percent-value (lookup options "Chart" "Tooltip"))
    (test-equal "snapshot slice label defaults to percentage"
      'percent (lookup options "Chart" "Data Labels"))
    (for-each
     (lambda (type)
       (set-chart-type! options type)
       (for-each
        (lambda (tooltip)
          (gnc-set-option options "Chart" "Tooltip" tooltip)
          (for-each
           (lambda (label)
             (gnc-set-option options "Chart" "Data Labels" label)
             (let* ((restored (restore-options guid options))
                    (document (render guid restored)))
               (test-equal "saved snapshot retains tooltip and label choices"
                 (list tooltip label)
                 (list (lookup restored "Chart" "Tooltip")
                       (lookup restored "Chart" "Data Labels")))
               (test-equal "circular chart receives the tooltip choice"
                 (symbol->string tooltip)
                 (chart-get document '(options allocationTooltip)))
               (test-equal "circular chart receives the slice label choice"
                 (symbol->string label)
                 (chart-get document '(options allocationDataLabels)))))
           '(none percent value both)))
        '(name name-percent name-value name-percent-value)))
     '(pie doughnut))
    (gnc-set-option options "Chart" "Tooltip" 'name-percent-value)
    (gnc-set-option options "Chart" "Data Labels" 'percent)
    (for-each
     (lambda (type)
       (set-chart-type! options type)
       (let ((document (render guid options)))
         (test-equal "snapshot chart type" (if (eq? type 'horizontal-bar) 'bar type)
           (gnc:html-chart-type (chart document)))
         (test-equal "snapshot chart values" (list->vector amounts) (series document 0))
         (test-equal "snapshot amounts use default currency precision"
           #t (default-currency-format? document))
         (test-equal "snapshot labels contain category names only"
           #("First" "Second")
           (chart-get document '(data labels)))
         (test-equal "snapshot tooltips retain monetary amounts"
           (list->vector amounts)
           (chart-get document '(data datasets (0) allocationAmounts)))
         (test-equal "snapshot tooltips retain allocation percentages"
           (list->vector (map (lambda (amount) (/ amount total)) amounts))
           (chart-get document '(data datasets (0) allocationPercentages)))
         (test-assert "snapshot tooltip callback displays amount and percentage"
           (string-contains (gnc:html-document-render document)
                            "' (' + percent.format(fraction) + ')' : ''"))
         (test-assert "snapshot draws allocation percentages on slices"
           (string-contains (gnc:html-document-render document)
                            "context.fillText(text, horizontal, position)"))
         (test-assert "snapshot labels support arcs and stacked bars"
           (string-contains (gnc:html-document-render document)
                            "stacked) &&"))
         (test-assert "slice labels stay within the plot, not slice boundaries"
           (let ((html (gnc:html-document-render document)))
             (and (string-contains html "area.right - insetX")
                  (string-contains html "area.bottom - insetY"))))
         (test-assert "slice labels have opaque dark text on 50% white"
           (let ((html (gnc:html-document-render document)))
             (and (string-contains
                   html "context.fillStyle = 'rgba(255, 255, 255, 0.5)'")
                  (string-contains html "context.fillRect(horizontal")
                  (string-contains html "context.fillStyle = '#222'"))))
         (test-equal "snapshot initializes its local plugin list"
           #() (chart-get document '(plugins)))
         (test-equal "chart has no redundant title" #f
           (chart-get document '(options plugins title display)))
         (test-equal "chart width follows percentage default" '(percent . 100)
           (gnc:html-chart-width (chart document)))
         (test-equal "chart height follows percentage default" '(percent . 75)
           (gnc:html-chart-height (chart document)))
         (test-equal "snapshot Total label and both values are bold without changing font size"
           (make-list 3 '("font-weight: bold;"))
           (map (lambda (cell) ((sxpath '(@ style *text*)) cell))
                ((sxpath '(td))
                 (last ((sxpath '(// table tbody *)) (table-sxml document))))))
         (when (eq? type 'horizontal-bar)
           (test-equal "snapshot bars index categories on x" "x"
             (chart-get document '(options indexAxis)))
           (test-equal "snapshot category axis" "category"
             (chart-get document '(options scales x type)))
           (test-equal "snapshot numeric axis" "linear"
             (chart-get document '(options scales y type)))
           (test-equal "snapshot bars start at zero" #t
             (chart-get document '(options scales y beginAtZero)))
           (test-equal "snapshot bars label the category axis" "Category"
             (chart-get document '(options scales x title text)))
           (test-equal "snapshot bars label the monetary axis"
             (gnc-commodity-get-mnemonic (gnc-default-report-currency))
             (chart-get document '(options scales y title text)))
           (test-assert "snapshot bars retain category labels on x"
             (string-contains (string-concatenate (gnc:html-chart-render (chart document) document))
                              "ticks.callback = xAxisDisplay"))
           (test-assert "snapshot bars use the currency formatter on y"
             (string-contains (string-concatenate (gnc:html-chart-render (chart document) document))
                              "ticks.callback = yAxisDisplay"))
           (test-equal "snapshot bars omit redundant single-series legend" #f
             (chart-get document '(options plugins legend display))))))
    '(doughnut pie horizontal-bar))
    (test-equal "bars survive saved-option restoration" 'bars
      (lookup (restore-options guid options) "Chart" "Chart Type"))
    (test-equal "restored bar selections render vertically" "x"
      (chart-get (render guid (restore-options guid options)) '(options indexAxis)))
    (gnc-set-option options "Chart" "Units" 'percent)
    (let ((document (render guid options)))
      (test-equal "snapshot percentage bars plot allocation fractions"
        (list->vector (map (lambda (amount) (/ amount total)) amounts))
        (series document 0))
      (test-equal "snapshot percentage axes retain percentage formatting"
        #f (default-currency-format? document))
      (test-equal "snapshot percentage bars label their monetary replacement axis"
        "Percent" (chart-get document '(options scales y title text)))
      (test-equal "snapshot percentage bars have a 100-percent maximum"
        1 (chart-get document '(options scales y max)))
      (test-equal "snapshot percentage bars retain monetary tooltip amounts"
        (list->vector amounts)
        (chart-get document '(data datasets (0) allocationAmounts))))
    (set-chart-type! options 'stacked-bar)
    (for-each
     (lambda (mode)
       (gnc-set-option options "Chart" "Units" mode)
       (for-each
        (lambda (label)
          (gnc-set-option options "Chart" "Data Labels" label)
          (let* ((restored (restore-options guid options))
                 (document (render guid restored)))
            (test-equal "saved snapshot stack retains its data label choice"
              label (lookup restored "Chart" "Data Labels"))
            (test-equal "snapshot stack receives its data label choice"
              (symbol->string label)
              (chart-get document '(options allocationDataLabels)))
            (test-equal "snapshot stack receives the tooltip choice"
              "name-percent-value"
              (chart-get document '(options allocationTooltip)))))
        '(none percent value both))
       (gnc-set-option options "Chart" "Data Labels" 'percent)
       (let ((document (render guid options)))
         (test-equal "snapshot stack uses bars" 'bar
           (gnc:html-chart-type (chart document)))
         (test-equal "monetary snapshot stacks use default currency precision"
           (eq? mode 'value) (default-currency-format? document))
         (test-equal "snapshot stack has one total column" #("Total")
           (chart-get document '(data labels)))
         (test-equal "snapshot stack contains one series per category" 2
           (vector-length (chart-get document '(data datasets))))
         (test-equal "snapshot stack keeps category legend order"
           '("First" "Second")
           (map (lambda (index) (chart-get document `(data datasets (,index) label)))
                '(0 1)))
         (test-equal "snapshot stack shows the category legend" #t
           (chart-get document '(options plugins legend display)))
         (test-equal "snapshot stack legend sits below the plot" "bottom"
           (chart-get document '(options plugins legend position)))
         (for-each
          (lambda (axis)
            (test-equal "snapshot stack enables stacking on both axes" #t
              (chart-get document `(options scales ,axis stacked))))
          '(x y))
         (for-each
          (lambda (index amount color)
            (test-equal "snapshot stack plots each category in the selected units"
              (vector (if (eq? mode 'percent) (/ amount total) amount))
              (series document index))
            (test-equal "snapshot stack retains category colors" (vector color)
              (chart-get document `(data datasets (,index) backgroundColor)))
            (test-equal "snapshot stack retains monetary tooltip amounts" (vector amount)
              (chart-get document `(data datasets (,index) allocationAmounts)))
            (test-equal "snapshot stack retains percentage tooltip values"
              (vector (/ amount total))
              (chart-get document `(data datasets (,index) allocationPercentages)))
            (test-equal "snapshot stack retains category links" 1
              (vector-length (chart-get document `(data datasets (,index) urls)))))
          '(0 1) amounts (gnc:assign-colors 2))
         (test-equal "snapshot stack labels its selected units"
           (if (eq? mode 'percent) "Percent"
               (gnc-commodity-get-mnemonic (gnc-default-report-currency)))
           (chart-get document '(options scales y title text)))
         (test-assert "snapshot stack tooltips identify their category"
           (string-contains (gnc:html-document-render document)
                            "items[0].dataset.label"))
         (test-equal "snapshot stack selection survives saved-option restoration"
           'stacked-bar (lookup (restore-options guid options) "Chart" "Chart Type"))))
     '(value percent))
    (gnc-set-option options "Chart" "Time Frame" 'time-series)
    (for-each
     (lambda (type)
       (set-chart-type! options type)
       (for-each
        (lambda (values-mode)
          (gnc-set-option options "Chart" "Units" values-mode)
          (let ((document (render guid options)))
            (test-equal "history chart type"
              (case type ((unstacked-line) 'line) ((grouped-bar) 'bar) (else type))
              (gnc:html-chart-type (chart document)))
            (test-equal "monetary history axes use default currency precision"
              (eq? values-mode 'value) (default-currency-format? document))
            (test-equal "inclusive daily samples" #("2020-01-01" "2020-01-02" "2020-01-03")
              (chart-get document '(data labels)))
            (for-each
             (lambda (index amount)
               (test-equal "history tooltip amounts ignore selected axis units"
                 (make-vector 3 amount)
                 (chart-get document
                            `(data datasets (,index) allocationAmounts)))
               (test-equal "history tooltips retain each category's total share"
                 (make-vector 3 (/ amount total))
                 (chart-get document
                            `(data datasets (,index) allocationPercentages)))
               (test-assert "history amounts or exact percentage ratios"
                 (every (lambda (actual)
                          (= actual (if (eq? values-mode 'percent) (/ amount total) amount)))
                        (vector->list (series document index)))))
             '(0 1) amounts)
            (test-equal "history tooltips retain date headings" #t
              (chart-get document '(options allocationHistory)))
            (test-assert "history tooltips do not enable snapshot data labels"
              (not (assq 'allocationDataLabels
                         (chart-get document '(options)))))
            (for-each
             (lambda (tooltip)
               (gnc-set-option options "Chart" "Tooltip" tooltip)
               (let ((restored (restore-options guid options)))
                 (test-equal "history retains and applies every tooltip mode"
                   (symbol->string tooltip)
                   (chart-get (render guid restored)
                              '(options allocationTooltip)))))
             '(name name-percent name-value name-percent-value))
            (test-equal "history category order" "First"
              (chart-get document '(data datasets (0) label)))
            (test-equal "stacked charts keep legend in category order"
              (if (memq type '(bar line)) #t #f)
              (chart-get document '(options plugins legend reverse)))
            (for-each
             (lambda (index)
               (test-equal "stacking puts first category at the top without moving datasets"
                 (if (memq type '(bar line)) (- index) #f)
                 (assq-ref (chart-get document `(data datasets (,index))) 'order)))
             '(0 1))
            (test-equal "y axis stacking follows chart choice"
              (not (memq type '(unstacked-line grouped-bar)))
              (chart-get document '(options scales y stacked)))
            (test-equal "x axis stacking follows chart choice"
              (not (memq type '(unstacked-line grouped-bar)))
              (chart-get document '(options scales x stacked)))
            (when (memq type '(line unstacked-line))
              (test-equal "line gaps are not bridged" #f
                (chart-get document '(data datasets (0) spanGaps)))
              (test-equal "only stacked lines have area fill"
                (if (eq? type 'line) "stack" #f)
                (chart-get document '(data datasets (0) fill))))
            (when (eq? values-mode 'percent)
              (test-equal "percentage axis maximum" 1
                (chart-get document '(options scales y max))))))
        '(percent value)))
    '(bar line unstacked-line grouped-bar))
    (for-each
     (lambda (type)
       (set-chart-type! options type)
       (test-equal "unstacked chart choices survive saved-option restoration"
         (if (eq? type 'grouped-bar) 'bars type)
         (lookup (restore-options guid options) "Chart" "Chart Type")))
     '(unstacked-line grouped-bar))
    (gnc-set-option options "Display" "Show Table" #f)
    (let ((document (render guid options)))
      (test-assert "hiding table retains chart" (chart document))
      (test-assert "hiding table omits table object"
        (not (document-object document gnc:html-table?))))
    (gnc-set-option options "Chart" "Plot Width" '(pixels . 700))
    (test-equal "custom pixel width propagates to chart" '(pixels . 700)
      (gnc:html-chart-width (chart (render guid options))))))

(define* (holding-options guid fixture #:optional (single? #f))
  (let ((options (make-options guid)))
    (set-category-data! options
      (if (string=? guid holdings-report-guid)
          (if single?
              (security-definitions (list "Equity" (list-ref fixture 5)))
              (security-definitions (list "Equity" (list-ref fixture 5))
                                    (list "Bonds" (list-ref fixture 6))))
          (if single?
              (account-definitions (list "Equity" (list-ref fixture 2)))
              (account-definitions (list "Equity" (list-ref fixture 2) (list-ref fixture 3))
                                   (list "Bonds" (list-ref fixture 4))))))
    (when (and single? (string=? guid holdings-report-guid))
      (gnc-set-option options "Accounts" "Accounts" (list (list-ref fixture 2))))
    options))

(define (test-missing-prices guid)
  (let* ((fixture (security-fixture 2))
         (options (holding-options guid fixture))
         (pricedb (gnc-pricedb-get-db (gnc-get-current-book)))
         (prices-before (gnc-pricedb-get-num-prices pricedb)))
    (gnc-set-option options "General" "Use transaction prices when missing" #f)
    (gnc-set-option options "Chart" "Time Frame" 'time-series)
    (set-chart-type! options 'line)
    (gnc-set-option options "Chart" "Units" 'value)
    (let ((document (render guid options)))
      (test-equal "missing price gaps the first category" #(null 50 50) (series document 0))
      (test-equal "missing price gaps every category at the same date"
        #(null 80 80) (series document 1))
      (test-equal "partial-valuations table is clearly labelled"
        '("Date" "Equity" "Bonds" "Priced Total") (row document 0))
      (test-equal "unpriced history does not silently inflate totals"
        '("2020-01-01" "$0.00" "$0.00" "$0.00") (row document 1))
      (test-equal "later priced history is still rendered"
        '("2020-01-02" "$50.00" "$80.00" "$130.00") (row document 2)))
    (gnc-set-option options "Chart" "Units" 'percent)
    (let ((document (render guid options)))
      (test-equal "missing prices also gap percentage history" 'null
        (vector-ref (series document 0) 0))
      (test-equal "incomplete percentages are omitted from the table"
        '("2020-01-01") (row document 1)))
    (set-chart-type! options 'unstacked-line)
    (let ((document (render guid options)))
      (test-equal "unstacked lines retain incomplete-date gaps" 'null
        (vector-ref (series document 0) 0))
      (test-equal "unstacked lines do not bridge incomplete dates" #f
        (chart-get document '(data datasets (0) spanGaps))))
    (set-chart-type! options 'grouped-bar)
    (let ((document (render guid options)))
      (test-equal "grouped bars retain incomplete-date gaps" 'null
        (vector-ref (series document 0) 0))
      (test-equal "grouped bars gap all categories at incomplete dates" 'null
        (vector-ref (series document 1) 0)))
    (when (string=? guid holdings-report-guid)
      (set-targets! options '(60 40))
      (gnc-set-option options "Chart" "Measure" 'drift)
    (set-chart-type! options 'unstacked-line)
      (let ((document (render guid options)))
        (test-equal "drift retains incomplete-date gaps" 'null
          (vector-ref (series document 0) 0))
        (test-equal "drift gaps every category at incomplete dates" 'null
          (vector-ref (series document 1) 0))
        (test-equal "drift table omits incomplete deviations" '("2020-01-01")
          (row document 1))))
    (when (string=? guid holdings-report-guid)
      (gnc-set-option options "Chart" "Measure" 'allocation))
    (set-chart-type! options 'line)
    (gnc-set-option options "General" "Use transaction prices when missing" #t)
    (gnc-set-option options "Chart" "Units" 'value)
    (let ((document (render guid options)))
      (test-equal "transaction fallback fills missing historical prices"
        #(50 50 50) (series document 0))
      (test-equal "fallback prices all affected categories" #(80 80 80) (series document 1)))
    (test-equal "rendering and fallback never insert prices into the book"
      prices-before (gnc-pricedb-get-num-prices pricedb))
    (gnc-set-option options "General" "Use transaction prices when missing" #f)
    (gnc-set-option options "General" "End Date"
                    (cons 'absolute (gnc-dmy2time64 1 1 2020)))
    (assert-message guid options "No dates have complete, nonnegative category values")
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (assert-message guid options "Some holdings have no usable nonzero price")
    (test-assert "missing-price snapshots retain their value table"
      (document-object (render guid options) gnc:html-table?))))

(define (test-transaction-price-cache)
  (let ((last-trade (@@ (gnucash reports standard asset-allocation)
                        allocation-last-trade))
        (trades #(#(10) #(20) #(20) #(30))))
    (test-equal "an empty trade index has no usable price" -1
      (last-trade #() 20))
    (test-equal "trade lookup respects dates and chooses the last equal date"
      '(-1 0 0 2 2 3 3)
        (map (lambda (date) (last-trade trades date)) '(0 10 15 20 25 30 40))))
  (let* ((fixture (security-fixture #f))
         (stock (list-ref fixture 2))
         (security (list-ref fixture 5))
         (currency (gnc-default-report-currency))
         (date (gnc:time64-end-day-time (gnc-dmy2time64 3 1 2020)))
         (make-fallback (@@ (gnucash reports standard asset-allocation)
                            make-transaction-price-fn))
         (fallback (make-fallback (list stock) date))
         (conversions 0)
         (convert (lambda (foreign domestic)
                    (set! conversions (1+ conversions)) foreign)))
    (for-each
     (lambda (quantity)
       (test-equal "cached unit prices preserve each holding's quantity"
         (* quantity 10)
         (gnc:gnc-monetary-amount
          (fallback (gnc:make-gnc-monetary security quantity)
                    currency date convert))))
     '(2 5 7))
    (test-equal "a repeated commodity/date resolves its price once" 1 conversions)
    (let ((missing (make-fallback (list stock) date)))
      (set! conversions 0)
      (for-each
       (lambda (ignored)
         (test-equal "failed exchange prices remain missing" #f
           (missing (gnc:make-gnc-monetary security 2) currency date
                    (lambda (foreign domestic)
                      (set! conversions (1+ conversions))
                      (gnc:make-gnc-monetary domestic 0)))))
       '(first second))
      (test-equal "missing converted prices are cached too" 1 conversions)))
  (gnc-clear-current-session))

(define (test-precise-valuations guid)
  (let* ((fixture (security-fixture))
         (env (car fixture))
         (broker (list-ref fixture 1))
         (stock (list-ref fixture 2))
         (security (list-ref fixture 5))
         (dust-security (new-security "DUST"))
         (dust (named-account env "Dust" ACCT-TYPE-STOCK dust-security broker))
         (euro (gnc-commodity-table-lookup
                (gnc-commodity-table-get-table (gnc-get-current-book))
                "CURRENCY" "EUR"))
         (cash (named-account env "Euro" ACCT-TYPE-BANK euro broker)))
    (buy env broker dust 1 1 1/1000)
    (buy env broker cash 1 1 1)
    (add-price dust-security 1 1)
    (add-price euro 1 1/1000)
    (let ((options (make-options guid)))
      (if (string=? guid holdings-report-guid)
          (begin
            (gnc-set-option options "Accounts" "Accounts" (list stock dust cash))
            (set-category-data! options
              (security-definitions (list "Regular" security)
                                    (list "Dust" dust-security)
                                    (list "Foreign cash" euro))))
          (set-category-data! options
            (account-definitions (list "Regular" stock) (list "Dust" dust)
                                 (list "Foreign cash" cash))))
      (for-each
       (lambda (source)
         (gnc-set-option options "General" "Price Source" source)
         (for-each
          (lambda (fallback?)
            (gnc-set-option options "General"
                            "Use transaction prices when missing" fallback?)
            (let ((document (render guid options)))
              (test-equal "quoted sub-cent values remain exact with either fallback setting"
                #(20 1/1000 1/1000) (series document 0))
              (test-assert "sub-cent values do not produce missing-price warnings"
                (not (string-contains (gnc:html-document-render document)
                                     "Some holdings have no usable nonzero price")))))
          '(#f #t)))
       '(pricedb-before pricedb-nearest pricedb-latest))
      (add-price security 2 10000000001/10000000000)
      (add-price euro 2 10000000003/10000000001)
      (if (string=? guid holdings-report-guid)
          (begin
            (gnc-set-option options "Accounts" "Accounts" (list stock cash))
            (set-category-data! options
              (security-definitions (list "Large fraction" security euro))))
          (set-category-data! options
            (account-definitions (list "Large fraction" stock cash))))
      (gnc-set-option options "Chart" "Time Frame" 'time-series)
      (gnc-set-option options "Chart" "Units" 'value)
      (gnc-set-option options "General" "Price Source" 'pricedb-latest)
      (test-equal "large exact valuation fractions round before engine conversion"
        '("2020-01-02" "$3.00" "$3.00")
        (row (render guid options) 2)))))

(define (test-transaction-prices guid)
  (let* ((fixture (security-fixture #f))
         (env (list-ref fixture 0))
         (broker (list-ref fixture 1))
         (stock-a (list-ref fixture 2))
         (stock-b (list-ref fixture 3))
         (security (list-ref fixture 5))
         (options (holding-options guid fixture #t)))
    (buy env broker stock-b 2 100 1)
    (buy env broker stock-a 4 1000 1)
    (test-equal "fallback ignores future transactions and unselected accounts"
      #(20) (series (render guid options) 0))
    (for-each
     (lambda (view)
       (gnc-set-option options "Chart" "Time Frame" view)
       (test-assert "transaction fallback adds no notice above the chart"
         (not (string-contains (gnc:html-document-render (render guid options))
                               "Transaction prices were used"))))
     '(snapshot time-series))
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (buy env broker stock-a 2 30 1)
    (test-equal "fallback uses the latest eligible transaction, not the first"
      #(90) (series (render guid options) 0))
    (add-price security 1 5)
    (test-equal "available market prices take precedence over transaction prices"
      #(15) (series (render guid options) 0))
    (test-assert "market-priced allocations do not display an estimate warning"
      (not (string-contains (gnc:html-document-render (render guid options))
                            "Transaction prices were used")))
    (gnc-set-option options "General" "Use transaction prices when missing" #f)
    (test-equal "disabling fallback does not disable market prices"
      #(15) (series (render guid options) 0))
    (add-price security 3 0)
    (assert-message guid options "Some holdings have no usable nonzero price")
    (gnc-set-option options "General" "Use transaction prices when missing" #t)
    (test-equal "zero quotes can use transaction fallback"
      #(90) (series (render guid options) 0))))

(define (test-currency-fallback guid)
  (let* ((fixture (security-fixture #f))
         (env (list-ref fixture 0))
         (broker (list-ref fixture 1))
         (stock (list-ref fixture 2))
         (options (holding-options guid fixture #t))
         (euro (gnc-commodity-table-lookup
                (gnc-commodity-table-get-table (gnc-get-current-book)) "CURRENCY" "EUR")))
    (add-price euro 1 2)
    (env-create-multisplit-transaction
     env 2 1 2020 (list (vector broker -30 -30) (vector stock 30 1))
     #:currency euro #:pricedb? #f)
    (buy env broker stock 3 999 0)
    (test-equal "fallback converts transaction currency and skips zero-share splits"
      #(180) (series (render guid options) 0))
    (buy env broker stock 3 0 1)
    (test-equal "fallback skips unusable zero transaction prices"
      #(240) (series (render guid options) 0))))

(define (test-zero-and-negative-values)
  (let* ((fixture (account-fixture))
         (env (list-ref fixture 0))
         (parent (list-ref fixture 2))
         (child (list-ref fixture 3))
         (equity (list-ref fixture 5))
         (options (make-options accounts-report-guid))
         (holdings-options (make-options holdings-report-guid)))
    (env-transfer env 1 1 2020 parent equity 100)
    (env-transfer env 2 1 2020 equity parent 100)
    (env-transfer env 3 1 2020 parent equity 100)
    (set-category-data! options (account-definitions (list "Cash" parent)))
    (gnc-set-option options "Chart" "Time Frame" 'time-series)
    (test-equal "zero-total percentages become gaps" #(null 1 null)
      (series (render accounts-report-guid options) 0))
    (gnc-set-option options "Chart" "Units" 'value)
    (test-equal "amount history retains genuine zero values" #(0 100 0)
      (series (render accounts-report-guid options) 0))
    (set-targets! options '(100))
    (gnc-set-option holdings-options "Accounts" "Accounts" (list parent))
    (set-category-data! holdings-options
      (security-definitions (list "Cash" (gnc-default-report-currency))))
    (set-targets! holdings-options '(100))
    (gnc-set-option holdings-options "Chart" "Time Frame" 'time-series)
    (gnc-set-option holdings-options "Chart" "Measure" 'drift)
    (set-chart-type! holdings-options 'unstacked-line)
    (test-equal "zero totals gap drift, while a positive on-target value is zero"
      #(null 0 null) (series (render holdings-report-guid holdings-options) 0))
    (gnc-set-option holdings-options "General" "End Date"
      (cons 'absolute (gnc-dmy2time64 1 1 2020)))
    (assert-message holdings-report-guid holdings-options
      "No dates have complete, nonnegative values and a positive total")
    (test-equal "an entirely unchartable drift history still retains its table"
      '("2020-01-01") (row (render holdings-report-guid holdings-options) 1))
    (gnc-set-option holdings-options "General" "End Date"
      (cons 'absolute (gnc-dmy2time64 3 1 2020)))
    (set-chart-type! options 'line)
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (let ((document (render accounts-report-guid options)))
      (test-assert "all-zero snapshot has no chart" (not (chart document)))
      (test-equal "all-zero snapshot retains zero balance and blank percentage"
        '("Cash" "$0.00") (row document 1)))
    (env-transfer env 1 1 2020 parent equity 100)
    (env-transfer env 2 1 2020 equity parent 200)
    (set-category-data! options
      (account-definitions (list "Cash" parent) (list "Other" child)))
    (gnc-set-option options "Chart" "Time Frame" 'time-series)
    (set-chart-type! options 'line)
    (let ((document (render accounts-report-guid options)))
      (test-equal "negative values gap the affected category" #(null 200 100)
        (series document 0))
      (test-equal "a negative category gaps every series for that date" #(null 200 200)
        (series document 1)))
    (set-targets! options '(50 50))
    (test-equal "negative allocations gap drift rather than fabricating percentages" 'null
      (vector-ref (series (render holdings-report-guid holdings-options) 0) 0))
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (gnc-set-option options "General" "End Date"
                    (cons 'absolute (gnc-dmy2time64 1 1 2020)))
    (assert-message accounts-report-guid options "One or more categories have negative values")
    (test-assert "negative snapshots retain signed balances"
      (string-contains (cadr (row (render accounts-report-guid options) 1)) "-"))))

(define (test-options guid)
  (let* ((book (gnc-get-current-book))
         (options (gnc:make-report-options guid))
         (report-name
          (if (string=? guid holdings-report-guid)
              "Asset Allocation"
              "Asset Allocation (by Accounts)"))
         (definitions
          (if (string=? guid holdings-report-guid)
              '(explicit ("Uncategorized") ("Equity \"Core\"" "TEST::AAA")
                         ("Bonds" "TEST::BBB"))
              '(explicit ("Cash \"Reserve\"" "0123456789abcdef0123456789abcdef")
                         ("Investments" "abcdef0123456789abcdef0123456789")))))
    (let ((document (gnc:make-html-document)))
      (gnc:html-document-set-style-sheet! document
        (gnc:html-document-style-sheet (render guid options)))
      (gnc:html-document-add-object! document
        (gnc:make-html-text (gnc:html-make-options-link 17)
                           (gnc:html-make-options-link 17 "Accounts")))
      (test-equal "Options links preserve their legacy URL and support an Accounts destination"
        '("gnc-options:report-id=17#" "gnc-options:report-id=17#Accounts")
        ((sxpath '(// a @ href *text*))
         (xml->sxml (gnc:html-document-render document) #:trim-whitespace? #t))))
    (test-eqv "missing report IDs preserve the legacy result"
      (gnc:html-make-options-link #f)
      (gnc:html-make-options-link #f "Accounts"))
    (test-equal "original report GUID resolves to the renamed report" report-name
      (gnc:report-template-name (gnc:find-report-template guid)))
    (test-equal "default report title uses the renamed report" report-name
      (lookup options gnc:pagename-general gnc:optname-reportname))
    (test-equal "snapshot default" 'snapshot
      (lookup options "Chart" "Time Frame"))
    (test-equal "default snapshot chart remains doughnut" 'doughnut
      (lookup options "Chart" "Chart Type"))
    (test-equal "percentage default" 'percent
      (lookup options "Chart" "Units"))
    (test-equal "both reports default to the allocation measure" 'allocation
      (lookup options "Chart" "Measure"))
    (gnc-set-option options "Chart" "Chart Type" 'drift)
    (test-equal "drift is a measure rather than a chart type" 'doughnut
      (lookup options "Chart" "Chart Type"))
    (test-equal "width default" '(percent . 100)
      (lookup options "Chart" "Plot Width"))
    (test-equal "height default" '(percent . 75)
      (lookup options "Chart" "Plot Height"))
    (when (string=? guid holdings-report-guid)
      (test-assert "Accounts option was actually registered"
        (gnc-lookup-option options "Accounts" "Accounts"))
      (test-equal "no holding accounts in an empty book" '()
        (lookup options "Accounts" "Accounts")))
    (set-category-data! options definitions)
    (gnc-set-option options "Chart" "Time Frame" 'time-series)
    (gnc-set-option options "Chart" "Units" 'value)
    (set-chart-type! options 'line)
    (gnc-set-option options gnc:pagename-general gnc:optname-reportname
                    "My allocation review")
    (let ((restored (restore-options guid options)))
      (test-equal "saved custom report title survives the rename"
        "My allocation review"
        (lookup restored gnc:pagename-general gnc:optname-reportname))
      (test-equal "saved categories retain order, members, empty categories and escaped names"
        (lookup options "Categories" "Categories")
        (lookup restored "Categories" "Categories"))
      (for-each
       (lambda (name)
         (test-equal (format #f "saved chart setting: ~a" name)
           (lookup options "Chart" name) (lookup restored "Chart" name)))
      '("Time Frame" "Units" "Chart Type")))))

(define (test-pending-chart-controls guid)
  (let* ((module (resolve-module '(gnucash reports standard asset-allocation)))
         (register (module-ref module 'gnc-register-multichoice-callback-option))
         (selectable (module-ref module 'gnc-optiondb-set-option-selectable-by-name))
         (filter-choices (module-ref module 'gnc-optiondb-set-option-choices))
         (lookup-choice (module-ref module 'gnc-optiondb-lookup-choice))
         (callbacks (make-hash-table))
         (choices (make-hash-table))
         (visible-choices (make-hash-table))
         (ui-values (make-hash-table))
         (controls (make-hash-table)))
    (define (choose name value)
      (hash-set! ui-values name value)
      ((hash-ref callbacks name) value))
    (define (check expected choices)
      (test-equal "compatible chart types follow pending controls" choices
        (hash-ref visible-choices "Chart Type"))
      (test-equal "Units follows the pending chart type" expected
        (hash-ref controls "Units")))
    (define (check-labels labels?)
      (test-equal "Data Labels follows the pending chart and view" labels?
        (hash-ref controls "Data Labels"))
      (test-equal "Tooltip remains enabled in every chart mode" #t
        (hash-ref controls "Tooltip")))
    (dynamic-wind
      (lambda ()
        (module-set! module 'gnc-register-multichoice-callback-option
          (lambda args
            (hash-set! callbacks (list-ref args 2) (last args))
            (hash-set! choices (list-ref args 2) (list-ref args 6))
            (apply register args)))
        (module-set! module 'gnc-optiondb-lookup-choice
          (lambda args
            (or (hash-ref ui-values (list-ref args 2))
                (apply lookup-choice args))))
        (module-set! module 'gnc-optiondb-set-option-selectable-by-name
          (lambda args
            (hash-set! controls (list-ref args 2) (last args))
            (apply selectable args)))
        (module-set! module 'gnc-optiondb-set-option-choices
          (lambda args
            (let* ((name (list-ref args 2))
                   (allowed (list-ref args 3))
                   (selected (or (hash-ref ui-values name)
                                 (lookup (car args) "Chart" name))))
              (hash-set! visible-choices name allowed)
              (hash-set! ui-values name
                (if (memq selected allowed) selected (last args))))
            (apply filter-choices args))))
      (lambda ()
        (let ((options (make-options guid)))
          (test-equal "combined chart choices are alphabetical"
            '("Bars" "Doughnut" "Pie" "Stacked Bars" "Stacked Lines" "Unstacked Lines")
            (map (lambda (choice) (vector-ref choice 1))
                 (hash-ref choices "Chart Type")))
          (check #f '(bars doughnut pie stacked-bar))
          (check-labels #t)
          (choose "Chart Type" 'bars)
          (check #t '(bars doughnut pie stacked-bar))
          (check-labels #f)
          (test-equal "chart callback does not apply the saved selection"
            'doughnut (lookup options "Chart" "Chart Type"))
          (choose "Chart Type" 'pie)
          (check-labels #t)
          (choose "Time Frame" 'time-series)
          (check #t '(bars stacked-bar line unstacked-line))
          (check-labels #f)
          (test-equal "invalid history chart defaults to stacked lines" 'line
            (hash-ref ui-values "Chart Type"))
          (test-equal "time frame callback does not apply the saved selection"
            'snapshot (lookup options "Chart" "Time Frame"))
          (choose "Chart Type" 'stacked-bar)
          (check-labels #f)
          (choose "Time Frame" 'snapshot)
          (check-labels #t)
          (test-equal "compatible stacked bars survive the time-frame change" 'stacked-bar
            (hash-ref ui-values "Chart Type"))
          (choose "Chart Type" 'pie)
          (begin
            (choose "Measure" 'drift)
            (check #t '(bars stacked-bar))
            (check-labels #f)
            (test-equal "invalid snapshot drift defaults to bars" 'bars
              (hash-ref ui-values "Chart Type"))
            (test-equal "measure callback does not apply the stored selection"
              'allocation (lookup options "Chart" "Measure"))
            (choose "Measure" 'allocation)
            (test-equal "switching back does not restore pie" 'bars
              (hash-ref ui-values "Chart Type")))
          (choose "Chart Type" 'pie)
          (check #f '(bars doughnut pie stacked-bar))
          (check-labels #t)))
      (lambda ()
        (module-set! module 'gnc-register-multichoice-callback-option register)
        (module-set! module 'gnc-optiondb-set-option-selectable-by-name selectable)
        (module-set! module 'gnc-optiondb-lookup-choice lookup-choice)
        (module-set! module 'gnc-optiondb-set-option-choices filter-choices)))))

(define (test-measure-rendering guid)
  (let* ((security? (string=? guid holdings-report-guid))
         (fixture (if security? (security-fixture) (account-fixture)))
         (options (make-options guid))
         (difference (if security? -14/65 -1/10))
         (value-difference (if security? -28 -60)))
    (set-category-data! options
      (if security?
          (security-definitions (list "First" (list-ref fixture 5))
                                (list "Second" (list-ref fixture 6)))
          (account-definitions
           (list "First" (list-ref fixture 2) (list-ref fixture 3))
           (list "Second" (list-ref fixture 4)))))
    (set-targets! options '(60 40))
    (gnc-set-option options "Chart" "Measure" 'drift)
    (render guid options)
    (test-equal "drift replaces an incompatible snapshot with bars" 'bars
      (lookup options "Chart" "Chart Type"))
    (for-each
     (lambda (view types)
       (gnc-set-option options "Chart" "Time Frame" view)
       (for-each
        (lambda (type)
          (set-chart-type! options type)
          (for-each
           (lambda (units)
             (gnc-set-option options "Chart" "Units" units)
             (let* ((restored (restore-options guid options))
                    (document (render guid restored))
                    (value (if (eq? units 'percent)
                               difference value-difference)))
               (test-equal "new measure survives saved-option restoration" 'drift
                 (lookup restored "Chart" "Measure"))
               (test-equal "measure controls the table independently of chart type"
                 (if (eq? view 'snapshot) "Category" "Date / Difference %")
                 (car (row document 0)))
               (test-equal "measure controls signed chart values in either unit"
                 (if (eq? view 'snapshot)
                     (if (eq? type 'horizontal-bar)
                     (vector value (- value))
                     (vector value))
                   (make-vector 3 value))
                 (series document 0))
               (test-assert "all new drift modes render complete HTML"
                 (string-contains (gnc:html-document-render document) "new Chart"))))
           '(percent value)))
        types))
     '(snapshot time-series)
     '((horizontal-bar stacked-bar) (grouped-bar bar line unstacked-line)))
    (gnc-set-option options "Chart" "Time Frame" 'snapshot)
    (set-chart-type! options 'horizontal-bar)
    (gnc-set-option options "Chart" "Measure" 'allocation)
    (let ((document (render guid options)))
      (test-equal "switching back retains the compatible bars selection" 'bars
        (lookup options "Chart" "Chart Type"))
      (test-equal "switching back renders allocation rather than drift"
        (if security? #(50 80) #(300 300))
        (series document 0)))))

(define (test-drift-chart-types)
  (let* ((make-point (@@ (gnucash reports standard asset-allocation) make-allocation-point))
         (make-chart (@@ (gnucash reports standard asset-allocation) make-allocation-drift-chart))
         (categories '(("A") ("B") ("C") ("D")))
         (points (list (make-point 1 '(10 20 30 40) '() #t)
                       (make-point 2 '(20 40 60 80) '() #t)
                       (make-point 3 '(0 0 0 0) '("missing") #f)))
         (drifts '((-10 -20 10 20) (-10 -20 10 20) (null null null null))))
    (for-each
     (lambda (type)
       (let* ((history? (if (memq type '(horizontal-bar stacked-bar)) #f #t))
              (stacked? (memq type '(stacked-bar bar line)))
              (values (if stacked? '(-491/10 -491/10 491/10 491/10)
                          '(-491/5 491/5 0 0)))
              (chart (make-chart categories (list (car points)) (list values)
                                 car #:history? history? #:chart-type type))
              (axis (gnc:html-chart-get chart '(options scales y))))
         (test-equal "percentage drift suggests a range covering 98.2 percent" 99/100
           (assq-ref axis 'suggestedMax))
         (test-equal "percentage drift keeps symmetric suggested bounds" -99/100
           (assq-ref axis 'suggestedMin))
         (test-assert "percentage drift leaves endpoints free for regular ticks"
           (and (not (assq 'min axis)) (not (assq 'max axis))))))
     '(horizontal-bar stacked-bar grouped-bar bar line unstacked-line))
    (for-each
     (lambda (percent?)
       (for-each
        (lambda (type)
          (let* ((stacked? (if (memq type '(bar line)) #t #f))
                 (chart (make-chart categories points drifts car
                                    #:history? #t #:percent? percent?
                                    #:chart-type type))
                 (get (lambda (path) (gnc:html-chart-get chart path))))
            (test-equal "drift chart type follows the selected presentation"
              (if (memq type '(line unstacked-line)) 'line 'bar)
              (gnc:html-chart-type chart))
            (test-equal "drift stacking follows the selected presentation" stacked?
              (get '(options scales y stacked)))
            (test-equal "all drift types retain per-date units and gaps"
              (if percent? '#(-1/10 -1/10 null) '#(-10 -20 null))
              (get '(data datasets (0) data)))
            (test-equal "stacked drift limits cover signed stack totals"
              (if percent? (if stacked? 3/10 1/5) (if stacked? 60 40))
              (get '(options scales y suggestedMax)))
            (when (eq? type 'line)
              (test-equal "diverging stacked lines fill from zero in stack draw order" "origin"
                (get '(data datasets (0) fill))))))
        '(grouped-bar bar line unstacked-line))
       (let ((chart (make-chart categories (list (car points)) (list (car drifts))
                                car #:percent? percent?
                                #:chart-type 'stacked-bar)))
         (test-equal "snapshot drift stacks categories in a single column" '#("Total")
           (gnc:html-chart-get chart '(data labels)))
         (test-equal "snapshot stacked drift keeps signed values"
           (if percent? '#(-1/10) '#(-10))
           (gnc:html-chart-get chart '(data datasets (0) data)))
         (test-equal "snapshot drift stack enables the legend" #t
           (gnc:html-chart-get chart '(options plugins legend display)))))
     '(#t #f))))

(define (test-snapshot-drift-colors)
  (let ((select-chart (@@ (gnucash reports standard asset-allocation) allocation-select-chart)))
    (test-equal "invalid time-series allocation defaults to stacked lines" 'line
      (select-chart 'pie #:history? #t))
    (test-equal "invalid time-series drift defaults to unstacked lines" 'unstacked-line
      (select-chart 'doughnut #:history? #t #:drift? #t))
    (test-equal "invalid snapshot allocation defaults to doughnut" 'doughnut
      (select-chart 'line))
    (test-equal "invalid snapshot drift defaults to bars" 'bars
      (select-chart 'pie #:drift? #t))
    (for-each
     (lambda (selected)
       (for-each
        (lambda (history?)
          (for-each
           (lambda (drift?)
             (test-equal "compatible bars persist across all view changes" selected
               (select-chart selected #:history? history? #:drift? drift?)))
           '(#f #t)))
        '(#f #t)))
     '(bars stacked-bar)))
  (test-drift-chart-types)
  (let* ((make-chart (@@ (gnucash reports standard asset-allocation) make-allocation-drift-chart))
         (chart (make-chart '(("Underweight") ("Slightly Underweight") ("On Target")
                              ("Slightly Overweight") ("Overweight") ("Almost On Target") ("Missing"))
                            '(#f) '((-2 -1 0 1 2 1/100000 null)) (lambda (category) ""))))
    (test-equal "drift bars use the category palette, not sign or magnitude"
      (list->vector (gnc:assign-colors 7))
      (gnc:html-chart-get chart '(data datasets (0) backgroundColor)))
    (test-equal "zero circles use the same category palette as bars"
      (gnc:html-chart-get chart '(data datasets (0) backgroundColor))
      (gnc:html-chart-get chart '(data datasets (1) backgroundColor)))
    (test-equal "zero is omitted from bars to avoid duplicate tooltip values"
      '#(-1/50 -1/100 null 1/100 1/50 1/10000000 null)
      (gnc:html-chart-get chart '(data datasets (0) data)))
    (test-equal "zero marker appears only for exact zero, never rounded zero or gaps"
      '#(null null 0 null null null null)
      (gnc:html-chart-get chart '(data datasets (1) data)))
    (test-equal "zero markers use a point-only dataset" 'line
      (gnc:html-chart-get chart '(data datasets (1) type)))
    (test-equal "zero markers do not connect categories" #f
      (gnc:html-chart-get chart '(data datasets (1) showLine)))
    (test-equal "zero markers remain visible" 5
      (gnc:html-chart-get chart '(data datasets (1) pointRadius)))
    (test-equal "zero markers retain category links" '#("" "" "" "" "" "" "")
      (gnc:html-chart-get chart '(data datasets (1) urls)))))

(define (test-drift-amount-data)
  (let* ((make-point (@@ (gnucash reports standard asset-allocation) make-allocation-point))
         (make-chart (@@ (gnucash reports standard asset-allocation) make-allocation-drift-chart))
         (categories '(("Underweight") ("On Target") ("Overweight")))
         (points (list (make-point 0 '(25 50 125) '() #t)
                       (make-point 86400 '(50 100 250) '() #t)
                       (make-point 172800 '(0 0 0) '(missing) #f)))
         (drifts '((-25 0 25) (-25 0 25) (null null null)))
         (anchor (lambda (category) (car category)))
         (history (make-chart categories points drifts anchor
                              #:history? #t #:percent? #f))
         (snapshot (make-chart categories (list (car points)) (list (car drifts))
                               anchor #:percent? #f))
         (zero (make-chart categories (list (car points)) '((0 0 0))
                           anchor #:percent? #f))
         (tiny (make-chart categories (list (car points))
                           '((-1/100000 0 1/100000)) anchor #:percent? #f)))
    (for-each
     (lambda (index expected)
       (test-equal "monetary history drift uses each date's total and preserves gaps"
         expected (gnc:html-chart-get history `(data datasets (,index) data))))
     '(0 1 2) '(#(-50 -100 null) #(0 0 null) #(50 100 null)))
    (test-equal "monetary history uses a symmetric amount extent" 100
      (gnc:html-chart-get history '(options scales y suggestedMax)))
    (test-equal "monetary snapshot bars omit exact zero" #(-50 null 50)
      (gnc:html-chart-get snapshot '(data datasets (0) data)))
    (test-equal "monetary snapshot retains exact-zero circles" #(null 0 null)
      (gnc:html-chart-get snapshot '(data datasets (1) data)))
    (test-equal "monetary zero markers retain category colors"
      (gnc:html-chart-get snapshot '(data datasets (0) backgroundColor))
      (gnc:html-chart-get snapshot '(data datasets (1) backgroundColor)))
    (test-equal "monetary zero markers retain category links"
      #("Underweight" "On Target" "Overweight")
      (gnc:html-chart-get snapshot '(data datasets (1) urls)))
    (test-assert "perfectly on-target monetary drift has a nondegenerate axis"
      (< (gnc:html-chart-get zero '(options scales y suggestedMin)) 0
        (gnc:html-chart-get zero '(options scales y suggestedMax))))
    (test-equal "tiny monetary deviations remain nonzero bars" #(-1/50000 null 1/50000)
      (gnc:html-chart-get tiny '(data datasets (0) data)))
    (test-equal "tiny monetary deviations are not marked as exact zero" #(null 0 null)
      (gnc:html-chart-get tiny '(data datasets (1) data)))))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (qof-date-format-set QOF-DATE-FORMAT-ISO)
  (test-begin "allocation")
  (test-snapshot-drift-colors)
  (test-drift-amount-data)
  (test-group-with-cleanup "measure rendering"
    (for-each
     (lambda (guid)
       (test-measure-rendering guid)
       (gnc-clear-current-session))
     (list accounts-report-guid holdings-report-guid))
    (gnc-clear-current-session))
  (for-each
   (lambda (guid)
     (test-group-with-cleanup guid
       (test-options guid)
       (test-pending-chart-controls guid)
       (gnc-clear-current-session)))
   (list accounts-report-guid holdings-report-guid))
  (test-group-with-cleanup "account membership and validation"
    (test-transaction-price-cache)
    (test-saved-default-membership)
    (test-account-membership)
    (gnc-clear-current-session))
  (test-group-with-cleanup "first-run holding defaults"
    (test-holding-defaults)
    (gnc-clear-current-session)
    (test-saved-fallback-language)
    (gnc-clear-current-session))
  (test-group-with-cleanup "security membership and validation"
    (test-security-membership)
    (gnc-clear-current-session))
  (for-each
   (lambda (guid)
     (test-group-with-cleanup (string-append guid " targets")
       (test-targets guid)
       (gnc-clear-current-session))
     (test-group-with-cleanup (string-append guid " allocation drift")
       (test-drift guid)
       (gnc-clear-current-session))
     (test-group-with-cleanup (string-append guid " chart modes")
       (test-chart-modes guid)
       (gnc-clear-current-session))
     (test-group-with-cleanup (string-append guid " missing prices")
       (test-missing-prices guid)
       (gnc-clear-current-session))
     (test-group-with-cleanup (string-append guid " transaction prices")
       (test-precise-valuations guid)
       (gnc-clear-current-session)
       (test-transaction-prices guid)
       (gnc-clear-current-session))
     (test-group-with-cleanup (string-append guid " foreign transaction prices")
       (test-currency-fallback guid)
       (gnc-clear-current-session)))
   (list accounts-report-guid holdings-report-guid))
  (test-group-with-cleanup "zero and negative values"
    (test-zero-and-negative-values)
    (gnc-clear-current-session))
  (test-end "allocation"))