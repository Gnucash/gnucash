
;; Index file to load all of the releavant reports.
(gnc:support "report/report-list.scm")

;; Helper functions for reports (which haven't been included
;; elsewhere)
(gnc:depend "options-utilities.scm")

;; reports 
(use-modules (gnucash report account-piecharts))
(use-modules (gnucash report account-summary))
(use-modules (gnucash report average-balance))
(use-modules (gnucash report balance-sheet))
(use-modules (gnucash report category-barchart))
(use-modules (gnucash report hello-world))
(use-modules (gnucash report iframe-url))
(use-modules (gnucash report net-barchart))
(use-modules (gnucash report payables))
(use-modules (gnucash report pnl))
(use-modules (gnucash report portfolio))
(use-modules (gnucash report price-scatter))
(use-modules (gnucash report register))
(use-modules (gnucash report taxtxf))
(use-modules (gnucash report transaction))

;; style sheets 
(gnc:depend "report/stylesheet-plain.scm")
(gnc:depend "report/stylesheet-fancy.scm")

;; view templates 
(gnc:depend "report/view-column.scm")

;; welcome to gnucash
(gnc:depend "report/welcome-to-gnucash.scm")
