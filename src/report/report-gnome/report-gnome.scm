;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  report-gnome.scm
;;  module definition for the gnome report system code 
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report report-gnome))
(use-modules (gnucash main) (g-wrapped gw-gnc)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))

(use-modules (g-wrapped gw-report-gnome))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/report/report-system" 0)

(export gnc:report-edit-options)

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
