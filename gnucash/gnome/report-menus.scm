;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  report-menus.scm
;;  code to initialize the report menus
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-module (gnucash report-menus))
(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash gnome-utils))
(use-modules (gnucash report))
(use-modules (gnucash reports standard dashboard))

(eval-when (compile load eval expand)
  (load-extension "libgnc-gnome" "scm_init_sw_gnome_module"))
(use-modules (sw_gnome))

(export gnc:report-menu-setup)
(export gnc:add-report-template-menu-items)

(define (gnc:add-report-template-menu-items)
  (define *template-items* '())

  (gnc:report-templates-for-each
   (lambda (report-guid template)
     (let ((name (or (gnc:report-template-menu-name template)
                     (gnc:report-template-name template))))
       (set! *template-items* (cons (cons name template) *template-items*)))))

  (for-each
   (lambda (item)
     (let* ((menu-name (car item))
            (template (cdr item))
            (report-guid (gnc:report-template-report-guid template))
            (menu-tip (or (gnc:report-template-menu-tip template)
                          (format #f (G_ "Display the ~a report") (G_ menu-name))))
            (menu-path (append (list gnc:menuname-reports)
                               (or (gnc:report-template-menu-path template)
                                   '()))))
       (gnc-add-scm-extension
        (gnc:make-menu-item
         menu-name report-guid menu-tip menu-path
         (lambda (window)
           (gnc-main-window-open-report
            (gnc:make-report report-guid) window))))))
   (sort (filter (compose gnc:report-template-in-menu? cdr) *template-items*)
         (lambda (a b) (gnc:string-locale>? (car a) (car b))))))

(define (gnc:report-menu-setup)
  (define asset-liability-menu
    (gnc:make-menu gnc:menuname-asset-liability (list gnc:menuname-reports)))
  (define income-expense-menu
    (gnc:make-menu gnc:menuname-income-expense (list gnc:menuname-reports)))
  (define budget-menu
    (gnc:make-menu gnc:menuname-budget (list gnc:menuname-reports)))
  (define example-menu
    (gnc:make-menu gnc:menuname-example (list gnc:menuname-reports)))
  (define experimental-menu
    (gnc:make-menu gnc:menuname-experimental (list gnc:menuname-reports)))
  (define multicolumn-menu
    (gnc:make-menu gnc:menuname-multicolumn (list gnc:menuname-reports)))
  (define tax-menu
    (gnc:make-menu gnc:menuname-taxes (list gnc:menuname-reports)))
  (define business-menu
    (gnc:make-menu gnc:menuname-business-reports (list gnc:menuname-reports)))

  (gnc-add-scm-extension
   (gnc:make-menu-item
    (N_ "Saved Report Configurations")
    "4d3dcdc8890b11df99dd94cddfd72085"
    (N_ "Manage and run saved report configurations")
    (list "Reports/SavedReportConfigs")
    (lambda (window)
      (gnc:debug "called into custom report dialog, window is " window)
      (gnc-ui-custom-report window))))

  ;; (gnc-add-scm-extension tax-menu)
  (gnc-add-scm-extension income-expense-menu)
  (gnc-add-scm-extension asset-liability-menu)
  (gnc-add-scm-extension budget-menu)
  (gnc-add-scm-extension example-menu)
  (gnc-add-scm-extension experimental-menu)
  (gnc-add-scm-extension multicolumn-menu)
  (gnc-add-scm-extension business-menu)

  ;; run report-hook danglers
  (gnc-hook-run HOOK-REPORT '())

  ;; push reports (new items added on top of menu)
  (gnc:add-report-template-menu-items)

  ;; the dashboard report
  (gnc-add-scm-extension
   (gnc:make-menu-item
    (N_ "Dashboard")
    "ad80271c890b11dfa79f2dcedfd72085"
    (N_ "A basic dashboard for your accounting data")
    (list gnc:menuname-reports gnc:menuname-multicolumn)
    (lambda (window)
      (gnc-main-window-open-report (gnc:make-dashboard) window)))))
