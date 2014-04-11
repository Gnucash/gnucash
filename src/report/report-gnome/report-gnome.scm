;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  report-gnome.scm
;;  module definition for the gnome report system code 
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report report-gnome))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash gnome-utils))

(use-modules (ice-9 slib))
(require 'printf)

(use-modules (sw_report_gnome))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/report/utility-reports" 0)

(export gnc:report-edit-options)
(export gnc:report-menu-setup)
(export gnc:add-report-template-menu-items)

;; returns a function that takes a list: (options, report),
;; and returns a widget
(define (gnc:report-options-editor report) 
  (if (equal? (gnc:report-type report) "d8ba4a2e89e8479ca9f6eccdeb164588")
      gnc-column-view-edit-options
      gnc-report-window-default-params-editor))

;; do not rely on the return value of this function - it has none.
;; instead, this function's side-effect is to set the report's editor widget.
(define (gnc:report-edit-options report) 
  (let* ((editor-widg (gnc:report-editor-widget report)))
    (if (and editor-widg (not (null? editor-widg)))
        (gnc-report-raise-editor report)
        (begin
          (if (gnc:report-options report) 
              (begin 
                (set! editor-widg
                      ((gnc:report-options-editor report)
                       (gnc:report-options report)
                       report))
                (gnc:report-set-editor-widget! report editor-widg))
              (gnc-warning-dialog '() (_ "This report has no options.")))))))

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
                    (sprintf #f (_ "Display the %s report") (_ name))))

          (set! item
                (gnc:make-menu-item
                 name
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

  (gnc-add-scm-extension 
   (gnc:make-menu-item
   (N_ "Custom Reports")
   (N_ "Manage and run custom reports")
   (list gnc:menuname-reports)
   (lambda (window)
     (gnc:spawn-custom-report-dialog window))))

  ;; (gnc-add-scm-extension tax-menu)
  (gnc-add-scm-extension income-expense-menu)
  (gnc-add-scm-extension asset-liability-menu)
  (gnc-add-scm-extension budget-menu)
  (gnc-add-scm-extension utility-menu)

  ;; run report-hook danglers
  (gnc:hook-run-danglers HOOK-REPORT)

  ;; push reports (new items added on top of menu)
  (gnc:add-report-template-menu-items)

  ;; the Welcome to GnuCash "extravaganza" report
  (gnc-add-scm-extension
   (gnc:make-menu-item 
    (N_ "Welcome Sample Report")
    (N_ "Welcome-to-GnuCash report screen")
    (list gnc:menuname-reports gnc:menuname-utility "")
    (lambda (window)
      (gnc-main-window-open-report (gnc:make-welcome-report) window))))
  
)

(define (gnc:spawn-custom-report-dialog window)
  (gnc:debug "called into custom report dialog, window is " window)
  (gnc-ui-custom-report window))
