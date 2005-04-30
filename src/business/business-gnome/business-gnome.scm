(define-module (gnucash business-gnome))
(use-modules (g-wrapped gw-business-gnome))
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/gnome-utils" 0)
(gnc:module-load "gnucash/business-core" 0)
(gnc:module-load "gnucash/business-utils" 0)
(gnc:module-load "gnucash/gnome-search" 0)
(gnc:module-load "gnucash/business-core-file" 0)
(gnc:module-load "gnucash/dialog-tax-table" 0)

(gnc:module-load "gnucash/report/report-gnome" 0)

(use-modules (gnucash report business-reports))
(use-modules (gnucash main))		;for gnc:debug

(define main-window gnc:window-name-main)
(define top-level (N_ "_Business"))
(define new-label (N_ "New"))
(define find-label (N_ "Find"))

(define ui-started #f)

(define (remind-bills-due session)
  (define (option-value name)
    (gnc:option-value (gnc:lookup-global-option gnc:*business-label* name)))

  (let ((check-bills? (option-value "Notify Bills Due?")))
    (if (and session check-bills?)
	(let* ((book (gnc:session-get-book session))
	       (days (option-value "Bills Due Days")))
	  (gnc:invoice-show-bills-due book days)))))

(define (business-report-function)
  (gnc:add-extension
   (gnc:make-menu gnc:menuname-business-reports
		  (list "Main" gnc:menuname-reports))))

(define (business-book-opened session)
  (remind-bills-due session))

(define (business-ui-started)
  (set! ui-started #t)
  (remind-bills-due (gnc:get-current-session)))

(gnc:hook-add-dangler gnc:*report-hook* business-report-function)
;(gnc:hook-add-dangler gnc:*ui-post-startup-hook* business-ui-started)
(gnc:hook-add-dangler gnc:*book-opened-hook* business-book-opened)
