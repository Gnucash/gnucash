(define-module (gnucash business-utils))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/app-utils" 0)
(gnc:module-load "gnucash/business-core" 0)

(define gnc:*business-label* (N_ "Business"))
(define gnc:*company-name* (N_ "Company Name"))
(define gnc:*company-addy* (N_ "Company Address"))

(export gnc:*business-label* gnc:*company-name* gnc:*company-addy*)

(load-from-path "business-options.scm")
(load-from-path "business-prefs.scm")
