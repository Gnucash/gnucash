;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; net-barchart.scm : Display a time series for either net worth or
;; net profit.
;;
;; By Robert Merkel <rgmerk@mira.net>
;; and Christian Stimming <stimming@tu-harburg.de>
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

(define-module (gnucash report standard-reports budget-barchart))

(use-modules (srfi srfi-1))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "Budget Barchart"))

(define optname-accounts (N_ "Accounts"))
(define optname-budget (N_ "Budget"))

(define optname-running-sum (N_ "Running Sum"))

;(define (options-generator inc-exp?)
(define (options-generator)
  (let* (
      (options (gnc:new-options)) 
      ;; This is just a helper function for making options.
      ;; See gnucash/src/scm/options.scm for details.
      (add-option 
        (lambda (new-option)
          (gnc:register-option options new-option)))
    )
    ;; Option to select Budget
    (add-option (gnc:make-budget-option
        gnc:pagename-general optname-budget
        "a" (N_ "Budget")))

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general 
      optname-running-sum
      "b"
      (N_ "Calculate as running sum?")
      #t))

    ;; Option to select the accounts to that will be displayed
    (add-option (gnc:make-account-list-option
        gnc:pagename-accounts optname-accounts
        "a" (N_ "Report on these accounts")
        (lambda ()
	  (gnc:filter-accountlist-type
	    (list ACCT-TYPE-BANK ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY)
	    (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
        #f #t))

    ;; Set default page
    (gnc:options-set-default-section options gnc:pagename-general)

    ;; Return options
    options
))


;; For each period in the budget:
;; Retrive the budgeted running sum and actual running sum
;; for bac chart.
;;
;; Create bar and and vaules
;;
(define (gnc:chart-create-budget-actual budget acct running-sum)
  (let* ((chart (gnc:make-html-barchart)))

    ;; Setup barchart
    (gnc:html-barchart-set-title! chart (xaccAccountGetName acct))
    (gnc:html-barchart-set-width! chart 700)
    (gnc:html-barchart-set-height! chart 400)
    (gnc:html-barchart-set-row-labels-rotated?! chart #t)
    (gnc:html-barchart-set-col-labels! 
      chart (list (_ "Budget") (_ "Actual")))
    (gnc:html-barchart-set-col-colors! 
      chart '("blue" "red"))

    ;; Prepair vars for running sums, and to loop though periods
    (let* (
        (num-periods (gnc-budget-get-num-periods budget))
        (period 0)
        (bgt-sum 0)
        (act-sum 0)
        (date 0)
        (bgt-vals '())
        (act-vals '())
        (date-list '())
      )

      ;; Loop though periods
      (while (< period num-periods)

        ;; Add calc new running sum and add to list
	(if running-sum 
          (set! bgt-sum (+ bgt-sum 
            (gnc-numeric-to-double
              (gnc-budget-get-account-period-value budget acct period))))
          
	  (set! bgt-sum 
            (gnc-numeric-to-double
              (gnc-budget-get-account-period-value budget acct period)))
        )
        (set! bgt-vals (append bgt-vals (list bgt-sum)))

	(if running-sum
	  (set! act-sum (+ act-sum
            (gnc-numeric-to-double
              (gnc-budget-get-account-period-actual-value budget acct period))))
	  
	  (set! act-sum
            (gnc-numeric-to-double
              (gnc-budget-get-account-period-actual-value budget acct period)))
	)
        (set! act-vals (append act-vals (list act-sum)))

	;; Add period to date list
        (set! date (gnc-budget-get-period-start-date budget period))
        (set! date-list (append date-list (list (gnc-print-date date))))

	(set! period (+ period 1))
      )

      ;; Add data to chart
      (gnc:html-barchart-append-column! chart bgt-vals)
      (gnc:html-barchart-append-column! chart act-vals)
      (gnc:html-barchart-set-row-labels! chart date-list)
      (if running-sum
        (gnc:html-barchart-set-subtitle! chart
          (string-append "Bgt:" (number->string bgt-sum) "\n Act:" (number->string act-sum))))
    )

    ;; Reutrn newly created chart
    chart
))


;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (net-renderer report-obj)

  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value 
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (let* (
      (budget (get-option gnc:pagename-general optname-budget))
      (budget-valid? (and budget (not (null? budget))))
      (running-sum (get-option gnc:pagename-general optname-running-sum))
      (accounts (get-option gnc:pagename-accounts optname-accounts))
      (report-title (get-option gnc:pagename-general
        gnc:optname-reportname))
      (document (gnc:make-html-document))
    )
    (cond
      ((null? accounts)
        ;; No accounts selected
        (gnc:html-document-add-object!
          document
            (gnc:html-make-no-account-warning 
              report-title (gnc:report-id report-obj))))

      ((not budget-valid?)
        ;; No budget selected.
        (gnc:html-document-add-object!
          document (gnc:html-make-generic-budget-warning reportname)))

      ;; Else create chart for each account
      (else
        (for-each (lambda (acct)
          (if (null? (gnc-account-get-descendants acct))
            (gnc:html-document-add-object! document
              (gnc:chart-create-budget-actual budget acct running-sum))))
          accounts))
    ) ;; end cond
    
    document
))

;; Here we define the actual report
(gnc:define-report
 'version 1
 'name (N_ "Budget Barchart")
 'report-guid "415cd38d39054d9e9c4040455290c2b1"
 'menu-path (list gnc:menuname-budget)
 'options-generator (lambda () (options-generator))
 'renderer (lambda (report-obj) (net-renderer report-obj)))
