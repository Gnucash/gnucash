;; -*-scheme-*-
;; owner-billing.scm
;; Owner bills calculate the share related owner amount
;; derived from the property amount.
;; Respects selectable account structure, billing period.
;; Enables stylesheet based layout
;; with property management specific naming, logo and address.

;; based on balsheet-eg.scm
;; by Chris Dennis  chris@starsoftanalysis.co.uk

;; Author makes no implicit or explicit guarantee of accuracy of
;; these calculations and accepts no responsibility for direct
;; or indirect losses incurred as a result of using this software.
;;
;; Created by: Ralf Zerres <ralf.zerres@mail.de>
;; Copyright (c) 2022 Ralf Zerres <ralf.zerres@mail.de>
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


(define-module (gnucash reports standard owner-billing))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))

(use-modules (ice-9 local-eval))  ; for the-environment
(use-modules (srfi srfi-1))       ; list library
(use-modules (srfi srfi-9))       ; defining Record Types
(use-modules (srfi srfi-13))      ; for extra string functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define option constants

(define reportname (N_ "Owner Billing Report"))
(define menutip (N_ "Property management assigned owner bill (using eguile template)"))

;; Tab identifiers
(define page-accounts    gnc:pagename-accounts)
;;(define page-commodities (N_ "Commodities"))
(define page-display     gnc:pagename-display)
(define page-general     gnc:pagename-general)
(define page-notes       (N_ "Notes"))

;; General page
(define optname-date (N_ "Billing Date"))
(define opthelp-date (N_ "Due date that is advertised on the single invoice."))
(define optname-date-begin (N_ "Start Date"))
(define optname-date-end (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

;; Accounts page
;; (define optname-omit-zero-balance-accounts (N_ "Exclude accounts with zero total balances"))
;; (define opthelp-omit-zero-balance-accounts
;;   (N_ "Exclude non-top-level accounts with zero balance and no non-zero sub-accounts."))

;; (define optname-account-links (N_ "Display accounts as hyperlinks"))
;; (define opthelp-account-links
;;   (N_ "Shows each account in the table as a hyperlink to its register window."))

;; (define optname-depth-limit (N_ "Levels of Subaccounts"))
;; (define opthelp-depth-limit
;;   (N_ "Maximum number of account levels per billing category displayed as tree."))
;; (define optname-flatten? (N_ "Flatten list to depth limit"))
;; (define opthelp-flatten?
;;   (N_ "Displays accounts which exceed the depth limit at the depth limit."))
;; (define optname-accounts (N_ "Accounts"))
;; (define opthelp-accounts (N_ "Report on these accounts, if display depth allows."))
(define optname-subacct (N_ "Include Sub-Accounts"))
(define optname-internal (N_ "Exclude transactions between selected accounts"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

;; Notes page
(define optname-extra-notes (N_ "Extra Notes"))
(define opthelp-extra-notes
  ;; (N_ "Notes added at end of the bill -- may contain HTML markup."))
  (N_ "(Development version -- don't rely on the numbers on this report
without double-checking them.<br>Change the 'Extra Notes' option to
get rid of this message)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Options assignment

;;(debug-enable 'backtrace)

(define (options-generator)
  (let* ((options (gnc:new-options))
         ;; register a configuration option for the report
         (register-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    ;; General page
    ;; (gnc:options-add-report-date!
    ;;   options gnc:pagename-general optname-date "g1")

    (gnc:options-add-date-interval!
     options gnc:pagename-general optname-date-begin optname-date-end "a")

    (gnc:options-add-interval-choice!
     options gnc:pagename-general optname-stepsize "b" 'YearDelta)

    (gnc:options-add-currency!
     options gnc:pagename-general optname-report-currency "c")

    (gnc:options-add-price-source!
     options gnc:pagename-general optname-price-source "d" 'weighted-average)

    ;; (gnc:options-add-price-source!
    ;;  options page-general optname-price-source "b" 'average-cost)

    ;; (add-option (gnc:make-simple-boolean-option
    ;;           options page-general optname-foreign-show
    ;;           "g" opthelp-foreign-show #t))

    ;; ;; General tab
    ;; (gnc:options-add-date-interval!
    ;;  options gnc:pagename-general optname-date-begin optname-date-end "a")

    ;; (gnc:options-add-interval-choice!
    ;;  options gnc:pagename-general optname-stepsize "b" 'MonthDelta)

    ;; Report's currency
    ;; (gnc:options-add-currency!
    ;;  options gnc:pagename-general optname-report-currency "c")

    ;; Account tab
    (register-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-subacct
      "a" (N_ "Include sub-accounts of all selected accounts.") #t))

    (register-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-internal
      "b"
      (N_ "Exclude transactions that only involve two accounts, both of which are selected below. This only affects the profit and loss columns of the table.")
      #f))

    ;; account(s) to do report on
    (register-option
     (gnc:make-account-list-option
      gnc:pagename-accounts (N_ "Accounts")
      "c" (N_ "Do transaction report on this account.")
      (lambda ()
        ;; FIXME : gnc:get-current-accounts disappeared
        (let ((current-accounts '()))
          ;; If some accounts were selected, use those
          (cond ((not (null? current-accounts))
                 current-accounts)
                (else
                 ;; otherwise get some accounts -- here as an
                 ;; example we get the asset and liability stuff
                 (gnc:filter-accountlist-type
                  (list ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CREDIT
                        ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY
                        ACCT-TYPE-PAYABLE ACCT-TYPE-RECEIVABLE)
                  (gnc-account-get-children-sorted (gnc-get-current-root-account)))))))
      #f #t))

    ;; Display tab
    (register-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display (N_ "Show table")
      "a" (N_ "Display a table of the selected data.") #f))

    (register-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display (N_ "Show plot")
      "b" (N_ "Display a graph of the selected data.") #t))

    (register-option
     (gnc:make-list-option
      gnc:pagename-display (N_ "Plot Type")
      "c" (N_ "The type of graph to generate.") (list 'AvgBalPlot)
      (list
       (vector 'AvgBalPlot (N_ "Average"))
       (vector 'GainPlot (N_ "Profit"))
       (vector 'GLPlot (N_ "Gain/Loss")))))

    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "d" (cons 'percent 100.0) (cons 'percent 100.0))

    ;; ;; Notes pages
    ;; (add-option
    ;;  (gnc:make-text-option
    ;;   gnc:pagename-notes optname-extra-notes
    ;;   "a" opthelp-extra-notes))

    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)

    options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper uitilities

(define columns
  ;; Watch out -- these names should be consistent with the display
  ;; option where you choose them, otherwise users are confused.
  (list (G_ "Period start") (G_ "Period end") (G_ "Average")
        (G_ "Maximum") (G_ "Minimum") (G_ "Gain")
        (G_ "Loss") (G_ "Profit") ))


(define (analyze-splits splits balances daily-dates interval-dates
internal-included exchange-fn report-currency)
  ;; this is a tight loop. start with: daily-balances & daily-dates,
  ;; interval-dates, and the splitlist. traverse the daily balances
  ;; and splitlist until we cross an interval date boundary, then
  ;; summarize the interval-balances and interval-amounts
  (define work-to-do (length splits))
  (let loop ((results '())
             (interval-bals '())
             (interval-amts '())
             (splits splits)
             (work-done 0)
             (daily-balances (cdr balances))
             (daily-dates (cdr daily-dates))
             (interval-start (car interval-dates))
             (interval-dates (cdr interval-dates)))

    (cond
     ;; daily-dates finished. job done. add details for last-interval
     ;; which must be handled separately, and return to caller
     ((null? daily-dates)
      (reverse
       (cons (list
              (qof-print-date interval-start)
              (qof-print-date (car interval-dates))
              (/ (apply + interval-bals)
                 (length interval-bals))
              (apply max interval-bals)
              (apply min interval-bals)
              (apply + (filter positive? interval-amts))
              (- (apply + (filter negative? interval-amts)))
              (apply + interval-amts))
             results)))

     ;; first daily-date > first interval-date -- crossed interval
     ;; boundary -- add interval details to results
     ((> (car daily-dates) (car interval-dates))
      (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
      (loop (cons (list
                   (qof-print-date interval-start)
                   (qof-print-date (decdate (car interval-dates)
                                            DayDelta))
                   (/ (apply + interval-bals)
                      (length interval-bals))
                   (apply max interval-bals)
                   (apply min interval-bals)
                   (apply + (filter positive? interval-amts))
                   (- (apply + (filter negative? interval-amts)))
                   (apply + interval-amts))
                  results)    ;process interval amts&bals
            '()               ;reset interval-bals
            '()               ;and interval-amts
            splits
            work-done
            daily-balances
            daily-dates
            (car interval-dates)
            (cdr interval-dates)))

     ;; we're still within interval, no more splits left within
     ;; current interval. add daily balance to interval.
     ((or (null? splits)
          (> (xaccTransGetDate (xaccSplitGetParent (car splits)))
             (car interval-dates)))
      (loop results
            (cons (car daily-balances) interval-bals)
            interval-amts
            splits
            work-done
            (cdr daily-balances)
            (cdr daily-dates)
            interval-start
            interval-dates))

     ;; we're still within interval. 'internal' is disallowed; there
     ;; are at least 2 splits remaining, both from the same
     ;; transaction. skip them. NOTE we should really expand this
     ;; conditional whereby all splits are internal, however the
     ;; option is labelled as 2-splits only. best maintain behaviour.
     ((and (not internal-included)
           (pair? (cdr splits))
           (= 2 (xaccTransCountSplits (xaccSplitGetParent (car splits))))
           (equal? (xaccSplitGetParent (car splits))
                   (xaccSplitGetParent (cadr splits))))
      (loop results
            interval-bals
            interval-amts ;interval-amts unchanged
            (cddr splits) ;skip two splits.
            (+ 2 work-done)
            daily-balances
            daily-dates
            interval-start
            interval-dates))

     ;; we're still within interval. there are splits remaining. add
     ;; split details to interval-amts
     (else
      (loop results
            interval-bals
            (cons (gnc:gnc-monetary-amount
                   (exchange-fn
                    (gnc:make-gnc-monetary
                     (xaccAccountGetCommodity
                      (xaccSplitGetAccount (car splits)))
                     (xaccSplitGetAmount (car splits)))
                    report-currency
                    (car interval-dates)))
                  interval-amts) ;add split amt to list
            (cdr splits)         ;and loop to next split
            (1+ work-done)
            daily-balances
            daily-dates
            interval-start
            interval-dates)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define types and their associated methods used in this report

(define-record-type <billrec>
  (billrec-obj account code description placeholder? namelink commodity
               balance-num depth treedepth non-zero?
               property-amount distribution-key distribution-total
               owner-share owner-amount
               summary? subtotal-cc sublist)
  billrec?
  ;; account and associated entities
  (account billrec-account billrec-set-account!)
  (code billrec-code billrec-set-code!)
  (description billrec-description billrec-set-description!)
  (placeholder? billrec-placeholder? billrec-set-placeholder?!)
  (namelink billrec-namelink billrec-set-namelink!)
  (commodity billrec-commodity billrec-set-commodity!)
  (balance-num billrec-balance-num billrec-set-balance-num!)
  (depth billrec-depth billrec-set-depth!)
  (treedepth billrec-treedepth billrec-set-treedepth!)
  (non-zero? billrec-non-zero? billrec-set-non-zero?!)

  ;; essential owner-bill entities
  (property-amount billrec-property-amount billrec-set-property-amount!)
  (distribution-key billrec-distribution-key billrec-set-distribution-key!)
  (distribution-total billrec-distribution-total billrec-set-distribution-total!)
  (owner-share billrec-distribution-share billrec-set-owner-share!)
  (owner-amount billrec-owner-amount billrec-set-owner-amount!)

  ;; derived values
  (summary? billrec-summary? billrec-set-summary?!)
  (subtotal-cc billrec-subtotal-cc billrec-set-subtotal-cc!)
  (sublist billrec-sublist billrec-set-sublist!))

(define (billrec-balance-mny billrec)
  (gnc:make-gnc-monetary (billrec-commodity billrec) (billrec-balance-num billrec)))

(define (billrec-new)
  (billrec-obj
   ;; account
   #f
   ;; code
   ""
   ;; description
   ""
   ;; placeholder
   #f
   ;; namelink
   ""
   ;; commodity
   (gnc-default-currency)
   ;; balance-num
   0
   ;; depth
   0
   ;; treedepth
   0
   ;; non-zero
   #f
   ;; property-amount
   0
   ;; distribution-key
   ""
   ;; distribution-total
   0
   ;; owner-share
   0
   ;; owner-amount
   0
   ;; summary
   #f
   ;; subtoltal-cc
   (gnc:make-commodity-collector)
   ;; sublist
   #f))

;; (define (billrec-debug billrec port)
;;   ;; Reports debug output (using HTML for pretty-printing)
;;   (set-current-output-port port)
;;   (display "billrec:- ")
;;   (display " account: ")     (display (dump (billrec-account billrec)))
;;   (display " code: ")        (display (billrec-code billrec))
;;   (display " placeholder: ") (display (dump (billrec-placeholder? billrec)))
;;   (display " namelink: ")    (display (billrec-namelink billrec))
;;   (display " commodity: ")
;;   (if (billrec-commodity billrec)
;;       (display (gnc-commodity-get-mnemonic (billrec-commodity billrec)))
;;       (display "#f"))
;;   (display " balance-num: ")
;;   (if (billrec-balance-num billrec)
;;       (display (gnc-numeric-to-double (billrec-balance-num billrec)))
;;       (display "#f"))
;;   (display " depth: ")       (display (billrec-depth billrec))
;;   (display " treedepth: ")   (display (billrec-treedepth billrec))
;;   (display " non-zero?: ")   (display (billrec-non-zero? billrec))
;;   (display " summary?: ")    (display (billrec-summary? billrec))
;;   (display " subtotal-cc: ")
;;   (if (billrec-subtotal-cc billrec)
;;       ;;(display (get-commodity-collector-total (billrec-subtotal-cc billrec) #f))
;;       ;;(display (format-commodity-collector (billrec-subtotal-cc billrec)))
;;       (display
;;        (string-concatenate
;;      (map-in-order
;;       (lambda (mny)
;;         (string-append (gnc:monetary->string mny) " "))
;;       ((billrec-subtotal-cc billrec) 'format gnc:make-gnc-monetary #f))))
;;       (display "#f"))
;;   (display " sublist: ")
;;   (if (billrec-sublist billrec)
;;       (begin
;;      (display "\n<ul>")
;;      (for-each
;;       (lambda (sub-billrec)
;;         (display "\n<li>")
;;         (billrec-printer sub-billrec port)
;;         (display "</li>"))
;;       (billrec-sublist billrec))
;;      (display "</ul>"))
;;       (display "#f"))
;;   (display " property-amount: ")   (display (billrec-treedepth billrec))
;;   (display " distribution-key: ")   (display (billrec-distribution-key billrec))
;;   (display " distribution-total: ")   (display (billrec-distribution-total billrec))
;;   (display " owner-share: ")   (display (billrec-owner-share billrec))
;;   (display " owner-total: ")   (display (billrec-owner-total billrec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Render the report
;;; Return type 'document' (<html-doc> or plain HTML)

(define (report-renderer report-obj)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Report definition
;;; the report consumes the options-generator and renderer

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "f084f935271b49dcb88682959c7af256"
 'menu-name reportname
 'menu-tip menutip
 'menu-path (list gnc:menuname-property-mgmt)
 'options-generator options-generator
 'renderer report-renderer)
