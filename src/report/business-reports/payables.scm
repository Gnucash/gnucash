;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; payables.scm : accounts payable aging report
;;  
;; By Derek Atkins <warlord@MIT.EDU>
;; Copyright (c) 2002, 2003 Derek Atkins <warlord@MIT.EDU>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report payables))

(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash report aging))
(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

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
      "w" (N_ "The payable account you wish to examine.") 
      #f #f (list ACCT-TYPE-PAYABLE)))

    (aging-options-generator options)))

(define (payables-renderer report-obj)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (let ((payables-account (opt-val acc-page this-acc)))
    (gnc:debug "payables-account" payables-account)
    (aging-renderer report-obj this-acc payables-account #f)))

(define payables-aging-guid "e57770f2dbca46619d6dac4ac5469b50")

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 'version 1
 'name (N_ "Payable Aging")
 'report-guid payables-aging-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator options-generator
 'renderer payables-renderer
 'in-menu? #t)

(define (payables-report-create-internal acct title show-zeros?)
  (let* ((options (gnc:make-report-options payables-aging-guid))
	 (acct-op (gnc:lookup-option options acc-page this-acc))
	 (zero-op (gnc:lookup-option options acc-page optname-show-zeros))
	 (title-op (gnc:lookup-option options acc-page gnc:optname-reportname)))

    (gnc:option-set-value acct-op acct)
    (if (not (string-null? title))
        (gnc:option-set-value title-op title))
    (gnc:option-set-value zero-op show-zeros?)
    (gnc:make-report payables-aging-guid options)))

(define (gnc:payables-report-create-internal
	 account split query journal? double? title
	 debit-string credit-string)
  (payables-report-create-internal account "" #f))

(gnc:register-report-hook ACCT-TYPE-PAYABLE #f
			  gnc:payables-report-create-internal)

(export payables-report-create-internal)
