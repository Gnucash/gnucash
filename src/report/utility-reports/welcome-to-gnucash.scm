;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; welcome-to-gnucash.scm : introductory report for new users
;; Copyright 2001 Bill Gribble <grib@gnumatic.com>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report welcome-to-gnucash))
(export gnc:make-welcome-report)

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (sw_report_system))

(gnc:module-load "gnucash/report/report-system" 0)

(define (gnc:make-welcome-report)
  (let* ((view (gnc:make-report "Multicolumn View"))
         (sub-welcome (gnc:make-report "Welcome to GnuCash"))
         (sub-accounts (gnc:make-report "Account Summary"))
         (sub-expense-pie (gnc:make-report "Expense Accounts"))
         (sub-income-pie (gnc:make-report "Income Accounts"))
         (sub-bar (gnc:make-report "Income/Expense Chart"))
         (options #f))

    (define (set-option! section name value)
      (gnc:option-set-value 
       (gnc:lookup-option options section name) value))

    (set! options (gnc:report-options (gnc-report-find view)))
    (set-option! "General" "Report name" (_ "Welcome to GnuCash"))
    (set-option! "General" "Number of columns" 2)

    ;; mark the reports as needing to be saved 
    (gnc:report-set-needs-save?! (gnc-report-find sub-welcome) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-accounts) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-expense-pie) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-income-pie) #t)
    (gnc:report-set-needs-save?! (gnc-report-find sub-bar) #t)

    (set-option! "__general" "report-list" 
                 (list (list sub-welcome 1 1 #f)
                       (list sub-accounts 1 1 #f)
                       (list sub-expense-pie 1 1 #f)
                       (list sub-income-pie 1 1 #f)
                       (list sub-bar 2 1 #f)))
    
    (set! options (gnc:report-options (gnc-report-find sub-expense-pie)))
    (set-option! "Display" "Plot Width" 400)
    
    (set! options (gnc:report-options (gnc-report-find sub-income-pie)))
    (set-option! "Display" "Plot Width" 400)
    
    (set! options (gnc:report-options (gnc-report-find sub-bar)))
    (set-option! "Display" "Plot Width" 800)

    view))

(define (options) 
  (gnc:new-options))

(define (renderer report-obj)
  (let ((doc (gnc:make-html-document)))
    (gnc:html-document-add-object! 
     doc
     (gnc:make-html-text 
      (gnc:html-markup-h2 (_ "Welcome to GnuCash 2.0!"))
      (gnc:html-markup-p
       (_ "GnuCash 2.0 has lots of nice features. Here are a few."))))
    doc))

(gnc:define-report 
 'name (N_ "Welcome to GnuCash")
 'version 1
 'report-guid "65135608f2014c6ca8412793a8cdf169"
 'in-menu? #f
 'menu-path (list gnc:menuname-utility)
 'options-generator options
 'renderer renderer)
