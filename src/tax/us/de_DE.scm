(define-module (gnucash tax de_DE))

(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(export gnc:txf-get-payer-name-source)
(export gnc:txf-get-form)
(export gnc:txf-get-description)
(export gnc:txf-get-format)
(export gnc:txf-get-multiple)
(export gnc:txf-get-category-key)
(export gnc:txf-get-line-data)
(export gnc:txf-get-last-year)
(export gnc:txf-get-help)
(export gnc:txf-get-codes)
(export gnc:txf-get-tax-entity-type)
(export gnc:txf-get-tax-entity-type-description)
(export gnc:txf-get-tax-entity-type-codes)
(export gnc:txf-get-code-info)
(export txf-help-categories)

(export txf-income-categories)
(export txf-expense-categories)
(export txf-asset-categories)
(export txf-liab-eq-categories)

(define gnc:*tax-label* (N_ "Tax"))
(define gnc:*tax-nr-label* (N_ "Tax Number"))

(export gnc:*tax-label* gnc:*tax-nr-label*)

(load-from-path "txf-de_DE")
(load-from-path "txf-help-de_DE")
