;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; account-summary.scm : brief account listing 
;; Copyright 2000 Bill Gribble <grib@gnumatic.com>
;; 
;; Original version by  Terry D. Boldt (tboldt@attglobal.net>
;;   Author makes no implicit or explicit guarantee of accuracy of
;;   these calculations and accepts no responsibility for direct
;;   or indirect losses incurred as a result of using this software.
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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "report/account-summary.scm")
(gnc:depend  "report-html.scm")

;; account summary report 
;; prints a simple table of account information with clickable 
;; links to open the corresponding register window.

(let () 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; options generator
  ;; select accounts to report on, whether to show subaccounts,
  ;; whether to include subtotaled subaccount balances in the report,
  ;; and what date to show the summary for.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-options-generator)
    (let* ((options (gnc:new-options))
           (opt-register 
            (lambda (opt)
              (gnc:register-option options opt))))
      
      ;; date at which to report balance
      (opt-register 
       (gnc:make-date-option
        (_ "General") (_ "Date")
        "a" (_ "Select a date to report on")
        (lambda () (cons 'absolute (cons (current-time) 0)))
        #f 'absolute #f))
      
      ;; set of accounts to do report on 
      (opt-register 
       (gnc:make-account-list-option
        (_ "General") (_ "Account")
        "b" (_ "Report on these account(s)")
        (lambda ()
          (let ((current-accounts (gnc:get-current-accounts)))
            (cond ((not (null? current-accounts)) current-accounts)
                  (else
                   (gnc:group-get-account-list (gnc:get-current-group))))))
        #f #t))
      
      (opt-register 
       (gnc:make-simple-boolean-option
        (_ "General") (_ "Sub-Accounts")
        "c" (_ "Include sub-accounts of each selected account?") #f))      
      
      (opt-register 
       (gnc:make-simple-boolean-option
        (_ "General") (_ "Include Sub-Account balances")
        "d" (_ "Include sub-account balances in printed balance?") #f))      
      
      options))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; build-acct-table
  ;; does the dirty work of building a table for a set of accounts. 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (build-acct-table accounts end-date do-subs? sub-balances?)
    (let ((table (gnc:make-html-table)))
      ;; column 1: account names 
      (gnc:html-table-append-column!
       table 
       (map (lambda (acct)
              (gnc:make-html-text (gnc:html-markup-anchor
                                   (string-append 
                                    "gnc-register:account=" 
                                    (gnc:account-get-full-name acct))
                                   (gnc:account-get-name acct))))
            accounts))
      
      ;; column 2 (optional): subaccount info
      (if do-subs? 
          (let* ((has-subs? #f)
                 (data 
                  (map (lambda (acct)
                         (let* ((children 
                                 (gnc:account-get-immediate-subaccounts acct)))
                           (if (not (null? children))
                               (begin 
                                 (set! has-subs? #t)
                                 (build-acct-table children end-date 
                                                   #t sub-balances?))
                               #f)))
                       accounts)))
            (if has-subs? 
                (begin 
                  (gnc:html-table-append-column! table data)))))
      
      ;; column 3: balances 
      (gnc:html-table-append-column! 
       table
       (map (lambda (acct)
              (let ((pair 
		     ((gnc:account-get-comm-balance-at-date 
		       acct end-date sub-balances?) 
		      'getpair (gnc:account-get-commodity acct) #f)))
		;; pair is a list of one gnc:commodity and 
		;; one gnc:numeric value
		 (gnc:commodity-amount->string 
		  (cadr pair) 
		  (gnc:commodity-print-info (car pair) #t))))
	    accounts))
      
      ;; set column and table styles 
      (let ((bal-col (- (gnc:html-table-num-columns table) 1)))
        (gnc:html-table-set-col-style! 
         table 0 "td" 
         'attribute '("align" "left"))
        (gnc:html-table-set-col-style! 
         table 0 "th" 
         'attribute '("align" "left"))        
        
        (gnc:html-table-set-col-style! 
         table bal-col "td" 
         'attribute '("align" "right"))
        (gnc:html-table-set-col-style! 
         table bal-col "th" 
         'attribute '("align" "right"))
        
        (gnc:html-table-set-style! 
         table "td" 
         'attribute '("valign" "top")))
      
      table))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; accsum-renderer
  ;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accsum-renderer options)
    (define (get-option optname)
      (gnc:option-value
       (gnc:lookup-option options (_ "General") optname)))
    
    (let ((accounts (get-option (_ "Account")))
          (date-tp (vector-ref (get-option (_ "Date")) 1))
          (do-subs? (get-option (_ "Sub-Accounts")))
          (do-subtotals? (get-option (_ "Include Sub-Account balances")))
          (doc (gnc:make-html-document)))
      
      (gnc:html-document-set-title! doc "Account Summary")
      (if (not (null? accounts))
          (let ((table (build-acct-table accounts date-tp 
                                         do-subs? do-subtotals?)))
            ;; set the column headers 
            (if (= (gnc:html-table-num-columns table) 3)
                (begin 
                  (gnc:html-table-set-col-style! 
                   table 1 "table" 
                   'attribute '("width" "100%"))
                  (gnc:html-table-set-col-headers!
                   table 
                   (list (_ "Account name") (_ "Sub-Accounts") (_ "Balance"))))
                (begin 
                  (gnc:html-table-set-col-headers!
                   table (list (_ "Account name") (_ "Balance")))))
            ;; add the table 
            (gnc:html-document-add-object! doc table))
          
          ;; error condition: no accounts specified
          (let ((p (gnc:make-html-text)))
            (gnc:html-text-append! 
             p 
             (gnc:html-markup-h2 (_ "No accounts selected"))
             (gnc:html-markup-p
              (_ "This report requires accounts to be selected.")))
            (gnc:html-document-add-object! doc p)))      
      doc))
  
  (gnc:define-report 
   'version 1
   'name (_ "Account Summary")
   'options-generator accsum-options-generator
   'renderer accsum-renderer))


          
      
          