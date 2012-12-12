(define-module (gnucash business-gnome))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/app-utils" 0)
(gnc:module-load "gnucash/gnome-search" 0)

(gnc:module-load "gnucash/report/report-gnome" 0)

(use-modules (gnucash report business-reports))
(use-modules (gnucash main))		;for gnc:debug

(define (business-report-function)
  (gnc-add-scm-extension
   (gnc:make-menu gnc:menuname-business-reports
		  (list gnc:menuname-reports))))

(gnc-hook-add-scm-dangler HOOK-REPORT business-report-function)
