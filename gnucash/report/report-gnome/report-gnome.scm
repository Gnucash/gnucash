;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  report-gnome.scm
;;  module definition for the gnome report system code 
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


(define-module (gnucash report report-gnome))
(use-modules (gnucash utilities)) 
(use-modules (gnucash gnc-module))
(use-modules (gnucash gnome-utils))
(use-modules (gnucash gettext))
(use-modules (gnucash report utility-reports))

(eval-when
      (compile load eval expand)
      (load-extension "libgncmod-gnome-utils" "scm_init_sw_gnome_utils_module")
      (load-extension "libgncmod-report-gnome" "scm_init_sw_report_gnome_module"))
(use-modules (sw_report_gnome))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/report/report-system" 0)

(export gnc:report-menu-setup)
(export gnc:add-report-template-menu-items)

(define (gnc:add-report-template-menu-items)
  (define *template-items* '())

  (define (add-template-menu-item name template)
    (if (gnc:report-template-in-menu? template)
        (let ((title (string-append (_ "Report") ": " (_ name)))
              (menu-path (gnc:report-template-menu-path template))
              (menu-tip (gnc:report-template-menu-tip template))
              (item #f))

          ;;(if (not menu-path)
              ;;(set! menu-path '(""))
              ;;(set! menu-path
              ;; (append menu-path '(""))))

          (if (not menu-path)
              (set! menu-path '()))

          (set! menu-path (append (list gnc:menuname-reports) menu-path))

          (if (not menu-tip)
              (set! menu-tip
                    (format #f (_ "Display the ~a report") (_ name))))

          (set! item
                (gnc:make-menu-item
                 name
                 (gnc:report-template-report-guid template)
                 menu-tip
                 menu-path
                 (lambda (window)
                   (let ((report (gnc:make-report
                                  (gnc:report-template-report-guid template))))
                     (gnc-main-window-open-report report window)))))
          (gnc-add-scm-extension item))))

  (define (add-template report-guid template)
    (let ((name (gnc:report-template-name template))
	  (menu-name (gnc:report-template-menu-name template)))
      (if menu-name (set! name menu-name))
      (set! *template-items* (cons (cons name template) *template-items*))))

  (define (sort-templates a b)
    (string>? (car a) (car b)))

  (gnc:report-templates-for-each add-template)
  (for-each
   (lambda (item)
     (add-template-menu-item (car item) (cdr item)))
   (sort *template-items* sort-templates)))


(define (gnc:report-menu-setup)
  (define asset-liability-menu
    (gnc:make-menu gnc:menuname-asset-liability (list gnc:menuname-reports)))
  (define income-expense-menu
    (gnc:make-menu gnc:menuname-income-expense (list gnc:menuname-reports)))
  (define budget-menu
    (gnc:make-menu gnc:menuname-budget (list gnc:menuname-reports)))
  (define utility-menu
    (gnc:make-menu gnc:menuname-utility (list gnc:menuname-reports)))
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
     (gnc:spawn-custom-report-dialog window))))

  ;; (gnc-add-scm-extension tax-menu)
  (gnc-add-scm-extension income-expense-menu)
  (gnc-add-scm-extension asset-liability-menu)
  (gnc-add-scm-extension budget-menu)
  (gnc-add-scm-extension utility-menu)
  (gnc-add-scm-extension business-menu)

  ;; run report-hook danglers
  (gnc-hook-run HOOK-REPORT '())

  ;; push reports (new items added on top of menu)
  (gnc:add-report-template-menu-items)

  ;; the Welcome to GnuCash "extravaganza" report
  (gnc-add-scm-extension
   (gnc:make-menu-item 
    (N_ "Welcome Sample Report")
    "ad80271c890b11dfa79f2dcedfd72085"
    (N_ "Welcome-to-GnuCash report screen")
    (list gnc:menuname-reports gnc:menuname-utility "")
    (lambda (window)
      (gnc-main-window-open-report (gnc:make-welcome-report) window))))
  
)

(define (gnc:spawn-custom-report-dialog window)
  (gnc:debug "called into custom report dialog, window is " window)
  (gnc-ui-custom-report window))
