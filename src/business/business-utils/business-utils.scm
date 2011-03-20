(define-module (gnucash business-utils))

(use-modules (gnucash gnc-module))
(gnc:module-load "gnucash/app-utils" 0)
(gnc:module-load "gnucash/business-core" 0)

(define gnc:*business-label* (N_ "Business"))
(define gnc:*company-name* (N_ "Company Name"))
(define gnc:*company-addy* (N_ "Company Address"))
(define gnc:*company-id* (N_ "Company ID"))
(define gnc:*company-phone* (N_ "Company Phone Number"))
(define gnc:*company-fax* (N_ "Company Fax Number"))
(define gnc:*company-url* (N_ "Company Website URL"))
(define gnc:*company-email* (N_ "Company Email Address"))
(define gnc:*company-contact* (N_ "Company Contact Person"))

(export gnc:*business-label* gnc:*company-name* gnc:*company-addy* gnc:*company-id*
            gnc:*company-phone* gnc:*company-fax* gnc:*company-url*
            gnc:*company-email* gnc:*company-contact*)

(define gnc:*option-section-accounts* OPTION-SECTION-ACCOUNTS)
(define gnc:*option-name-trading-accounts* OPTION-NAME-TRADING-ACCOUNTS)

(export gnc:*option-section-accounts* gnc:*option-name-trading-accounts*)

(define gnc:*option-section-budgeting* OPTION-SECTION-BUDGETING)
(define gnc:*option-name-default-budget* OPTION-NAME-DEFAULT-BUDGET)

(export gnc:*option-section-budgeting* gnc:*option-name-default-budget*)

(load-from-path "business-options.scm")
(load-from-path "business-prefs.scm")
