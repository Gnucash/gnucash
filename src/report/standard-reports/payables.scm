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
(use-modules (gnucash report standard-reports))

(define this-acc "this-account")

(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-account-list-option
      "__reg" this-acc
      "" ""
      (lambda () '())
      #f
      #f))

    (aging-options-generator options)))

(define (find-first-payable-account)
  (define (find-first-payable group num-accounts index)
    (if (>= index num-accounts)
	#f
	(let* ((this-account (gnc:group-get-account (group index)))
	       (account-type (gw:enum-<gnc:AccountType>-val->sym
			      (gnc:account-get-type this-account) #f)))
	  (if (eq? account-type 'payable)
	      this-account
	      (find-first-payable group num-accounts (+ index 1))))))

  (let* ((current-group (gnc:get-current-group))
	 (num-accounts (gnc:group-get-num-accounts
			current-group)))
    (if (> num-accounts 0)
	(find-first-payable current-group num-accounts 0)
	#f)))

(define (payables-renderer report-obj)

  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  (let* ((payables-account (op-value "__reg" this-acc)))
    (gnc:debug "payables-account" payables-account)

    (if (null? payables-account)
	(set! payables-account (find-first-payables-account))
	(set! payables-account (car payables-account)))

    (aging-renderer report-obj payables-account #f)))

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 'version 1
 'name (N_ "Payable Aging")
 'options-generator options-generator
 'renderer payables-renderer
 'in-menu? #f)

(define (payables-report-create-internal acct)
  (let* ((options (gnc:make-report-options "Payable Aging"))
	 (acct-op (gnc:lookup-option options "__reg" this-acc)))

    (gnc:option-set-value acct-op (list acct))
    (gnc:make-report "Payable Aging" options)))

(define (gnc:payables-report-create-internal
	 account split query journal? double? title
	 debit-string credit-string)
  (payables-report-create-internal account))

(gnc:register-report-hook 'payable #f
			  gnc:payables-report-create-internal)
