(define-module (gnucash business-utils))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/app-utils" 0)
(gnc:module-load "gnucash/business-core" 0)

(define gnc:*business-label* (N_ "Business"))
(define gnc:*company-name* (N_ "Company Name"))
(define gnc:*company-addy* (N_ "Company Address"))
(define gnc:*company-id* (N_ "Company ID"))

(export gnc:*business-label* gnc:*company-name* gnc:*company-addy* gnc:*company-id*)

(load-from-path "business-options.scm")
(load-from-path "business-prefs.scm")
