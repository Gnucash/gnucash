;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  report-gnome.scm
;;  module definition for the gnome report system code 
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report report-gnome))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))

(use-modules (ice-9 slib))
(require 'printf)

(use-modules (g-wrapped gw-report-gnome))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/report/report-system" 0)

(export gnc:report-edit-options)
(export gnc:report-menu-setup)
(export gnc:add-report-template-menu-items)

(define (gnc:report-options-editor report) 
  (if (equal? (gnc:report-type report) "Multicolumn View")
      gnc:column-view-edit-options
      gnc:default-options-editor))

(define (gnc:report-edit-options report) 
  (let* ((editor-widg (gnc:report-editor-widget report)))
    (if editor-widg
        (gnc:report-raise-editor report)
        (begin
          (if (gnc:report-options report) 
              (begin 
                (set! editor-widg
                      ((gnc:report-options-editor report)
                       (gnc:report-options report)
                       report))
                (gnc:report-set-editor-widget! report editor-widg))
              (gnc:warning-dialog (_ "This report has no options.")))))))

(define (gnc:add-report-template-menu-items)
  (define menu-namer (gnc:new-menu-namer))

  (define (add-template-menu-item name template)
    (if (gnc:report-template-in-menu? template)
        (let ((title (string-append (_ "Report") ": " (_ name)))
              (menu-path (gnc:report-template-menu-path template))
              (menu-name (gnc:report-template-menu-name template))
              (menu-tip (gnc:report-template-menu-tip template))
              (item #f))

          (if (not menu-path)
              (set! menu-path '(""))
              (set! menu-path
                    (append menu-path '(""))))

          (set! menu-path (append (list gnc:menuname-reports) menu-path))

          (if menu-name (set! name menu-name))

          (if (not menu-tip)
              (set! menu-tip
                    (sprintf #f (_ "Display the %s report") (_ name))))

          (set! item
                (gnc:make-menu-item
                 ((menu-namer 'add-name) name)
                 menu-tip
                 menu-path
                 (lambda ()
                   (let ((report (gnc:make-report
                                  (gnc:report-template-name template))))
                     (gnc:main-window-open-report report #f)))))
          (gnc:add-extension item))))

  (gnc:report-templates-for-each add-template-menu-item))

(define (gnc:report-menu-setup)
  ;; since this menu gets added to every child window, we say it 
  ;; comes after the "_Actions" menu. 
  (define menu (gnc:make-menu gnc:menuname-reports (list "_Actions")))
  (define menu-namer (gnc:new-menu-namer))
  (define tax-menu (gnc:make-menu gnc:menuname-taxes
                                  (list gnc:menuname-reports "")))
  (define income-expense-menu
    (gnc:make-menu gnc:menuname-income-expense
                   (list gnc:menuname-reports "")))
  (define asset-liability-menu
    (gnc:make-menu gnc:menuname-asset-liability
                   (list gnc:menuname-reports "")))
  (define utility-menu
    (gnc:make-menu gnc:menuname-utility
                   (list gnc:menuname-reports "")))

  (gnc:add-extension menu)

  ;; (gnc:add-extension tax-menu)
  (gnc:add-extension income-expense-menu)
  (gnc:add-extension asset-liability-menu)
  (gnc:add-extension utility-menu)

  ;; push reports (new items added on top of menu)
  (gnc:add-report-template-menu-items))
