;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general-ledger.scm: general ledger report
;; 
;; By David Montenegro <sunrise2000@comcast.net> 2004.07.13
;; 
;;  * BUGS:
;;    
;;    See any "FIXME"s in the code.
;;    
;; Largely borrowed from welcome-to-gnucash.scm by
;;  Bill Gribble <grib@gnumatic.com>
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

(define-module (gnucash report general-ledger))
(export gnc:make-general-ledger-report)
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "General Ledger"))
(define xactrptname "Transaction Report")

;; report constructor

(define (gnc:make-general-ledger-report)
  (let* ((xactrpt (gnc:make-report xactrptname)))
    xactrpt))

;; options generator

(define (general-ledger-options-generator)
  
  (let* ((options (gnc:report-template-new-options/name xactrptname))
	 )
    
    (define pagename-sorting (N_ "Sorting"))
    (define (set-option! section name value)
      (gnc:option-set-value 
       (gnc:lookup-option options section name) value))
    
    ;; set options in the accounts tab...
    (set-option!
     gnc:pagename-accounts (N_ "Filter Type") 'none)
    (set-option!
     gnc:pagename-accounts (N_ "Void Transactions?") 'non-void-only)
    
    ;; set options in the display tab...
    (for-each
     (lambda (l)
       (set-option! gnc:pagename-display (car l) (cadr l)))
     ;; One list per option here with: option-name, default-value
     (list
      (list (N_ "Date") #t)
      (list (N_ "Reconciled Date") #f)
      (list (N_ "Num") #f)
      (list (N_ "Description") #t)
      (list (N_ "Memo") #f)
      (list (N_ "Account Name") #f)
      (list (N_ "Use Full Account Name?") #f)
      (list (N_ "Account Code") #f)
      (list (N_ "Other Account Name") #f)
      (list (N_ "Use Full Other Account Name?") #f)
      (list (N_ "Other Account Code") #f)
      (list (N_ "Shares") #f)
      (list (N_ "Price") #f)
      ;; note the "Amount" multichoice option here
      (list (N_ "Amount") 'double)
      (list (N_ "Running Balance") #t)
      (list (N_ "Totals") #f)
      (list (N_ "Sign Reverses?") 'credit-accounts)
      )
     )
    
    ;; set options in the general tab...
    (set-option!
     gnc:pagename-general (N_ "Style") 'single)
    ;; we can't (currently) set the Report name here
    ;; because it is automatically set to the template
    ;; name... :(
    
    ;; set options in the sorting tab...
    (for-each
     (lambda (l)
       (set-option! pagename-sorting (car l) (cadr l)))
     ;; One list per option here with: option-name, default-value
     (list
      (list (N_ "Primary Key") 'account-code)
      (list (N_ "Show Full Account Name?") #f)
      (list (N_ "Show Account Code?") #t)
      (list (N_ "Primary Subtotal") #t)
      (list (N_ "Primary Subtotal for Date Key") 'none)
      (list (N_ "Primary Sort Order") 'ascend)
      (list (N_ "Secondary Key") 'register-order)
      (list (N_ "Secondary Subtotal") #t)
      (list (N_ "Secondary Subtotal for Date Key") 'none)
      (list (N_ "Secondary Sort Order") 'ascend)
      )
     )
    
    options)
  )

;; report renderer

(define (general-ledger-renderer report-obj)
  ;; just delegate rendering to the Transaction Report renderer...
  ((gnc:report-template-renderer/name xactrptname) report-obj))

(gnc:define-report 
 'version 1
 'name reportname
 'report-guid "2e22929e5c5b4b769f615a815ef0c20f"
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator general-ledger-options-generator
 'renderer general-ledger-renderer
 )

;; END

