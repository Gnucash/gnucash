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

(define-module (gnucash reports standard general-ledger))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

(define reportname (N_ "General Ledger"))
(define xactrptguid "2fe3b9833af044abb929a88d5a59620f")
(define xactrptname "Transaction Report")



;; options generator

(define (general-ledger-options-generator)
  (let ((options (gnc:trep-options-generator)))

    (define pagename-sorting (N_ "Sorting"))
    (define (set-option! section name value)
      (GncOption-set-default-value
       (gnc-lookup-option (gnc:optiondb options) section name) value))
    
    ;; set options in the accounts tab...
    (set-option! gnc:pagename-accounts "Filter Type" 'none)
    (set-option! "Filter" "Void Transactions" 'non-void-only)
    
    ;; set options in the display tab...
    (for-each
     (lambda (l)
       (set-option! gnc:pagename-display (car l) (cadr l)))
     ;; One list per option here with: option-name, default-value
     (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
         (list
          (list (N_ "Date") #t)
          (list (N_ "Reconciled Date") #f)
          (list (N_ "Num/Action") #f)
          (list (N_ "Trans Number") #f)
          (list (N_ "Description") #t)
          (list (N_ "Memo") #f)
          (list (N_ "Account Name") #f)
          (list (N_ "Use Full Account Name") #f)
          (list (N_ "Account Code") #f)
          (list (N_ "Other Account Name") #f)
          (list (N_ "Use Full Other Account Name") #f)
          (list (N_ "Other Account Code") #f)
          (list (N_ "Shares") #f)
          (list (N_ "Price") #f)
          ;; note the "Amount" multichoice option here
          (list (N_ "Amount") 'double)
          (list (N_ "Running Balance") #t)
          (list (N_ "Totals") #f)
          (list (N_ "Sign Reverses") 'credit-accounts)
         )
         (list
          (list (N_ "Date") #t)
          (list (N_ "Reconciled Date") #f)
          (list (N_ "Num") #f)
          (list (N_ "Description") #t)
          (list (N_ "Memo") #f)
          (list (N_ "Account Name") #f)
          (list (N_ "Use Full Account Name") #f)
          (list (N_ "Account Code") #f)
          (list (N_ "Other Account Name") #f)
          (list (N_ "Use Full Other Account Name") #f)
          (list (N_ "Other Account Code") #f)
          (list (N_ "Shares") #f)
          (list (N_ "Price") #f)
          ;; note the "Amount" multichoice option here
          (list (N_ "Amount") 'double)
          (list (N_ "Running Balance") #t)
          (list (N_ "Totals") #f)
          (list (N_ "Sign Reverses") 'credit-accounts)
         )
     )
    )
    
    ;; set options in the general tab...
    (set-option!
     gnc:pagename-display (N_ "Detail Level") 'single)


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
      (list (N_ "Show Full Account Name") #f)
      (list (N_ "Show Account Code") #t)
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
  (let ((document ((gnc:report-template-renderer/report-guid xactrptguid xactrptname)
                   report-obj)))
    (gnc:html-document-set-title! document (G_ reportname))
    document))

(gnc:define-report 
 'version 1
 'name reportname
 'report-guid "2e22929e5c5b4b769f615a815ef0c20f"
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator general-ledger-options-generator
 'renderer general-ledger-renderer
 )

;; END

