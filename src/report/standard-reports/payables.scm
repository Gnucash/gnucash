;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; payables.scm : accounts payable aging report
;;  
;; By Derek Atkins <warlord@MIT.EDU>
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report payables))

(use-modules (ice-9 slib))
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash report aging))

(define opt-pay-acc (N_ "Payables Account"))
(define sect-acc (N_ "Accounts"))

(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-account-list-option
      sect-acc opt-pay-acc
      "a" (N_ "Account where payables are stored.")
      ;; FIXME: Have a global preference for the payables account??
      ;; default-getter
      (lambda ()
	(define (find-first-payable current-group num-accounts this-account-ind)
	  (if
	   (>= this-account-ind num-accounts) 
	   #f
	   (let* 
	       ((this-account 
		 (gnc:group-get-account current-group this-account-ind))
		(account-type (gw:enum-<gnc:AccountType>-val->sym
			       (gnc:account-get-type this-account) #f)))
	     (begin 
	       (gnc:debug "this-account" this-account)
	       (gnc:debug "account-type" account-type)
	       (if (eq? account-type 'payable)
		   (begin 
		     (gnc:debug "this-account selected" this-account)
		      this-account)
		   (find-first-payable 
		    current-group num-accounts (+ this-account-ind 1)))))))

	(let* ((current-group (gnc:get-current-group))
	      (num-accounts (gnc:group-get-num-accounts
			     current-group)))
	  (if (> num-accounts 0)
	      (let ((first-payable (find-first-payable
				      current-group
				      num-accounts
				      0)))
		(gnc:debug "first-payable" first-payable)
		(if first-payable
		    (list first-payable)
		    (list (gnc:group-get-account current-group 0))))
	      '())))
     ;; value-validator
     (lambda (account-list)
	(let ((first-account) (car account-list))
	  (gnc:debug "account-list" account-list)
	  (if first-account
	    (let ((account-type (gw:enum-<gnc:AccountType>-val->sym 
				 (gnc:account-get-type first-account))))
	      (if (eq? 'payable account-type) 
	  
		  (cons #t  (list first-account))
		  (cons #f  (_ "The payables account must be a payable account"))))
	    ;; FIXME: until we can select a default account I need 
	    ;; to catch this at the report-writing stage
	    (#t '()))))
      #f))

    (aging-options-generator options)))

(define (payables-renderer report-obj)

  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  (let* ((payables-account (car (op-value sect-acc opt-pay-acc))))
    (aging-renderer report-obj payables-account #f)))

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 
 ;; The version of this report.
 'version 1
 
 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name (N_ "Payable Aging")

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip (N_ "Amount owed, grouped by creditors and age.")

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-asset-liability)

 ;; The options generator function defined above.
 'options-generator options-generator
 
 ;; The rendering function defined above.
 'renderer payables-renderer)
