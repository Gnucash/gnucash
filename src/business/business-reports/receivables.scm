;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; receivables.scm : accounts receivable aging report
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

(define-module (gnucash report receivables))

(use-modules (ice-9 slib))
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/business-gnome" 0)

(use-modules (gnucash report aging))
(use-modules (gnucash report standard-reports))

(define this-acc "Receivables Account")

(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

;    (add-option
;     (gnc:make-internal-option "__reg" this-acc #f))

    (add-option
     (gnc:make-account-list-limited-option
      "Account" this-acc
      "" ""
      (lambda () '())
      #f
      #f
      '(receivable)))

    (aging-options-generator options)))

(define (find-first-receivable-account)
  (define (find-first-receivable group num-accounts index)
    (if (>= index num-accounts)
	#f
	(let* ((this-account (gnc:group-get-account group index))
	       (account-type (gw:enum-<gnc:AccountType>-val->sym
			      (gnc:account-get-type this-account) #f)))
	  (if (eq? account-type 'receivable)
	      this-account
	      (find-first-receivable group num-accounts (+ index 1))))))

  (let* ((current-group (gnc:get-current-group))
	 (num-accounts (gnc:group-get-num-accounts
			current-group)))
    (if (> num-accounts 0)
	(find-first-receivable current-group num-accounts 0)
	#f)))

(define (receivables-renderer report-obj)

  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  (let* ((receivables-account (op-value "Account" this-acc)))
    (gnc:debug "receivables-account" receivables-account)

    (if (null? receivables-account)
	(set! receivables-account (find-first-receivable-account))
	(set! receivables-account (car receivables-account)))

    (aging-renderer report-obj receivables-account #t)))

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 'version 1
 'name (N_ "Receivable Aging")
 'menu-path (list gnc:menuname-business-reports)
 'options-generator options-generator
 'renderer receivables-renderer
 'in-menu? #t)

(define (receivables-report-create-internal acct)
  (let* ((options (gnc:make-report-options "Receivable Aging"))
	 (acct-op (gnc:lookup-option options "Account" this-acc)))

    (gnc:option-set-value acct-op (list acct))
    (gnc:make-report "Receivable Aging" options)))

(define (gnc:receivables-report-create-internal
	 account split query journal? double? title
	 debit-string credit-string)
  (receivables-report-create-internal account))

(gnc:register-report-hook 'receivable #f
			  gnc:receivables-report-create-internal)
