;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; options-utilities.scm: Useful option helper functions.
;; 
;; By Christian Stimming <stimming@tu-harburg.de>
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

(gnc:support "options-utilities.scm")

(gnc:depend "options.scm")

;; These are just a bunch of options which were useful in several
;; reports and hence they got defined in a seperate function.

;; This is one single end-date of a report.
(define (gnc:options-add-report-date!
	 options pagename optname sort-tag)
  (gnc:register-option 
    options  
    (gnc:make-date-option
     pagename optname 
     sort-tag (_ "Select a date to report on")
     (lambda ()
       (cons 'absolute 
	     (gnc:timepair-end-day-time     
	      (gnc:secs->timepair 
	       (car (mktime (localtime (current-time))))))))
     #f 'absolute #f)))

;; This is a date-interval for a report.
(define (gnc:options-add-date-interval!
	 options pagename name-from name-to sort-tag)
  (begin
    (gnc:register-option 
     options  
     (gnc:make-date-option
      pagename name-from 
      (string-append sort-tag "a")
      (_ "Start of reporting period")
      (lambda ()
	(cons 'absolute 
	      (gnc:get-start-cal-year)))
      #f 'absolute #f))
    (gnc:register-option 
     options  
     (gnc:make-date-option
      pagename name-to
      (string-append sort-tag "b")
      (_ "End of reporting period")
      (lambda ()
	(cons 'absolute 
	      (gnc:timepair-end-day-time     
	       (gnc:secs->timepair 
		(car (mktime (localtime (current-time))))))))
      #f 'absolute #f))))

;; These help for selecting a bunch of accounts.
(define (gnc:options-add-account-selection! 
	 options pagename 
	 name-display-depth name-show-subaccounts name-accounts
	 sort-tag default-depth default-accounts)
  (begin
    (gnc:register-option 
     options  
     (gnc:make-multichoice-option
      pagename name-display-depth
      (string-append sort-tag "a")
      (_ "Show accounts to this depth, overriding any other option.") 
      default-depth
      (list (list->vector
	     (list 'all (_ "All") (_ "Show all accounts")))
	    (list->vector
	     (list 1 "1" (_ "Top-level")))
	    (list->vector
	     (list 2 "2" (_ "Second-level")))
	    (list->vector
	     (list 3 "3" (_ "Third-level")))
	    (list->vector
	     (list 4 "4" (_ "Fourth-level")))
	    (list->vector
	     (list 5 "5" (_ "Fifth-level"))))))
    
    (gnc:register-option 
     options  
     (gnc:make-simple-boolean-option
      pagename name-show-subaccounts
      (string-append sort-tag "b")
      (_ "Override account-selection and show sub-accounts of all selected accounts?") 
      #t))

    ;; Semantics of the account selection, as used in the
    ;; gnc:html-build-acct-table: An account shows up if ( the
    ;; tree-depth is large enough AND ( it is selected in the account
    ;; selector OR ( always show sub-accounts is selected AND one of
    ;; the parents is selected in the account selector. )))
    (gnc:register-option 
     options  
     (gnc:make-account-list-option
      pagename name-accounts
      (string-append sort-tag "c")
      (_ "Report on these accounts, if display depth allows.")
      default-accounts
      #f #t))))

;; The single checkbox whether to include the sub-account balances
;; into the other balances.
(define (gnc:options-add-include-subaccounts!
	 options pagename optname sort-tag)
  (gnc:register-option 
    options  
    (gnc:make-simple-boolean-option
     pagename optname
     sort-tag (_ "Include sub-account balances in printed balance?") #t)))

;; These are common options for the selection of the report's
;; currency/commodity.
(define (gnc:options-add-currency-selection!
	 options pagename 
	 name-show-foreign name-report-currency sort-tag)
  (begin
    (gnc:register-option 
     options 
     (gnc:make-simple-boolean-option
      pagename name-show-foreign
      (string-append sort-tag "a")
      (_ "Display the account's foreign currency amount?") #f))
    
    (gnc:register-option 
     options 
     (gnc:make-currency-option 
      pagename name-report-currency
      (string-append sort-tag "b")
      (_ "All other currencies will get converted to this currency.")
      (gnc:option-value
       (gnc:lookup-global-option "International"
				 "Default Currency"))))))
