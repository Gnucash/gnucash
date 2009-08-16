;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general-journal.scm: general journal report
;; 
;; By David Montenegro <sunrise2000@comcast.net> 2004.07.14
;; 
;;  * BUGS:
;;    
;;    See any "FIXME"s in the code.
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

(define-module (gnucash report standard-reports general-journal))
(export gnc:make-general-journal-report)
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/report/report-system" 0)

(define reportname (N_ "General Journal"))
(define regrptname (N_ "Register"))
(define regrptguid "22104e02654c4adba844ee75a3f8d173")

;; report constructor

(define (gnc:make-general-journal-report)
  (let* ((regrpt (gnc:make-report regrptguid)))
    regrpt))

;; options generator

(define (general-journal-options-generator)
  
  (let* ((options (gnc:report-template-new-options/report-guid regrptguid regrptname))
	 (query (qof-query-create-for-splits))
	 )
    
    (define (set-option! section name value)
      (gnc:option-set-value 
       (gnc:lookup-option options section name) value))
    
    ;; Match, by default, all non-void transactions ever recorded in
    ;; all accounts....  Whether or not to match void transactions,
    ;; however, may be of issue here. Since I don't know if the
    ;; Register Report properly ignores voided transactions, I'll err
    ;; on the side of safety by excluding them from the query....
    (qof-query-set-book query (gnc-get-current-book))
    (gnc:query-set-match-non-voids-only! query (gnc-get-current-book))
    (qof-query-set-sort-order query
			      (list SPLIT-TRANS TRANS-DATE-POSTED)
			      (list QUERY-DEFAULT-SORT)
			      '())
    (qof-query-set-sort-increasing query #t #t #t)

    (xaccQueryAddAccountMatch
     query
     (gnc-account-get-descendants-sorted
      (gnc-book-get-template-root (gnc-get-current-book)))
     QOF-GUID-MATCH-NONE
     QOF-QUERY-AND)

    ;; set the "__reg" options required by the Register Report...
    (for-each
     (lambda (l)
       (set-option! "__reg" (car l) (cadr l)))
     ;; One list per option here with: option-name, default-value
     (list
      (list "query" (gnc-query2scm query)) ;; think this wants an scm...
      (list "journal" #t)
      (list "double" #t)
      (list "debit-string" (_ "Debit"))
      (list "credit-string" (_ "Credit"))
      )
     )
    ;; we'll leave query malloc'd in case this is required by the C side...
    
    ;; set options in the general tab...
    (set-option!
     gnc:pagename-general (N_ "Title") (_ reportname))
    ;; we can't (currently) set the Report name here
    ;; because it is automatically set to the template
    ;; name... :(
    
    ;; set options in the display tab...
    (for-each
     (lambda (l)
       (set-option! gnc:pagename-display (car l) (cadr l)))
     ;; One list per option here with: option-name, default-value
     (list
      (list (N_ "Date") #t)
      (list (N_ "Num") #f)
      (list (N_ "Description") #t)
      (list (N_ "Account") #t)
      (list (N_ "Shares") #f)
      (list (N_ "Price") #f)
      ;; note the "Amount" multichoice option here
      (list (N_ "Amount") 'double)
      (list (N_ "Running Balance") #f)
      (list (N_ "Totals") #f)
      )
     )
    
    options)
  )

;; report renderer

(define (general-journal-renderer report-obj)
  ;; just delegate rendering to the Register Report renderer...
  ((gnc:report-template-renderer/report-guid regrptguid regrptname) report-obj))

(gnc:define-report 
 'version 1
 'name reportname
 'report-guid "25455562bd234dd0b048ecc5a8af9e43"
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator general-journal-options-generator
 'renderer general-journal-renderer
 )

;; END

