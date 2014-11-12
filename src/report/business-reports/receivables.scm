;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; receivables.scm : accounts receivable aging report
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

(define-module (gnucash report receivables))

(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (gnucash report aging))
(use-modules (gnucash report standard-reports))
(use-modules (gnucash report business-reports))

(define acc-page gnc:pagename-general)
(define this-acc (N_ "Receivables Account"))

(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-account-sel-limited-option
      acc-page this-acc
      "w" (N_ "The receivables account you wish to examine.") 
      #f #f (list ACCT-TYPE-RECEIVABLE)))

    (aging-options-generator options)))

(define (receivables-renderer report-obj)
  (define (op-value section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (let* ((receivables-account (op-value acc-page this-acc)))
    (gnc:debug "receivables-account" receivables-account)

    (aging-renderer report-obj this-acc receivables-account #t)))

(define receivables-aging-guid "9cf76bed17f14401b8e3e22d0079cb98")

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 'version 1
 'name (N_ "Receivable Aging")
 'report-guid receivables-aging-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator options-generator
 'renderer receivables-renderer
 'in-menu? #t)

(define (receivables-report-create-internal acct title show-zeros?)
  (let* ((options (gnc:make-report-options receivables-aging-guid))
	 (acct-op (gnc:lookup-option options acc-page this-acc))
	 (zero-op (gnc:lookup-option options acc-page optname-show-zeros))
	 (title-op (gnc:lookup-option options acc-page gnc:optname-reportname)))

    (gnc:option-set-value acct-op acct)
    (if (not (string-null? title))
        (gnc:option-set-value title-op title))
    (gnc:option-set-value zero-op show-zeros?)
    (gnc:make-report receivables-aging-guid options)))

(define (gnc:receivables-report-create-internal
	 account split query journal? double? title
	 debit-string credit-string)
  (receivables-report-create-internal account "" #f))

(gnc:register-report-hook ACCT-TYPE-RECEIVABLE #f
			  gnc:receivables-report-create-internal)

(export receivables-report-create-internal)
