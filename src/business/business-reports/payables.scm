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
(gnc:module-load "gnucash/business-gnome" 0)

(use-modules (gnucash report aging))
(use-modules (gnucash report standard-reports))

(define acc-page gnc:pagename-general)
(define this-acc (N_ "Payable Account"))

(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-account-sel-limited-option
      acc-page this-acc
      (N_ "The payable account you wish to examine") "zz"
      #f #f '(payable)))

    (aging-options-generator options)))

(define (payables-renderer report-obj)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (let ((payables-account (opt-val acc-page this-acc)))
    (gnc:debug "payables-account" payables-account)
    (aging-renderer report-obj payables-account #f)))

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 'version 1
 'name (N_ "Payable Aging")
 'menu-path (list gnc:menuname-business-reports)
 'options-generator options-generator
 'renderer payables-renderer
 'in-menu? #t)

(define (payables-report-create-internal acct)
  (let* ((options (gnc:make-report-options (N_ "Payable Aging")))
	 (acct-op (gnc:lookup-option options acc-page this-acc)))

    (gnc:option-set-value acct-op acct)
    (gnc:make-report "Payable Aging" options)))

(define (gnc:payables-report-create-internal
	 account split query journal? double? title
	 debit-string credit-string)
  (payables-report-create-internal account))

(gnc:register-report-hook 'payable #f
			  gnc:payables-report-create-internal)
