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

(define-module (gnucash reports standard receivables))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash reports aging))

(define acc-page gnc:pagename-general)
(define this-acc (N_ "Receivables Account"))
(define optname-addr-source (N_ "Address Source")) ;; Billing or Shipping addresses

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

    ;; As aging.scm functions are used by both receivables.scm and payables.scm
    ;;  add option "receivable" on hidden page "__hidden" with default value 'R
    ;;  so aging.scm functions can tell if they are reporting on
    ;;  accounts receivable or payable, as customers have a shipping address
    ;;  but vendors do not. The Address Source option therefore only applies
    ;;  to customers.
    (add-option
     (gnc:make-internal-option "__hidden" "receivable-or-payable" 'R))

    (add-option
      (gnc:make-multichoice-option
        gnc:pagename-display
        optname-addr-source
        "a"
        (N_ "Address source.")
        'billing
        (list
         (vector 'billing (N_ "Billing address"))
         (vector 'shipping (N_ "Shipping address")))))

    (aging-options-generator options)))

(define (receivables-renderer report-obj)
  (define (op-value section name)
    (gnc:option-value
     (gnc:lookup-option (gnc:report-options report-obj) section name)))

  (let* ((receivables-account (op-value acc-page this-acc)))
    (gnc:debug "receivables-account" receivables-account)

    (aging-renderer report-obj this-acc receivables-account #t)))

(define receivables-aging-guid "9cf76bed17f14401b8e3e22d0079cb98-old")

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 'version 1
 'name "Receivable Aging (legacy)"
 'report-guid receivables-aging-guid
 'menu-path (list gnc:menuname-business-reports)
 'options-generator options-generator
 'renderer receivables-renderer
 'in-menu? (gnc-prefs-is-extra-enabled))

