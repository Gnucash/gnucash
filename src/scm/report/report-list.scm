;; Index file to load all of the releavant reports.
(gnc:support "report/report-list.scm")

;; Helper functions for reports (which haven't been included
;; elsewhere)
(gnc:depend "options-utilities.scm")

;; reports 
(gnc:depend "report/net-barchart.scm")
(gnc:depend "report/account-summary.scm")
(gnc:depend "report/average-balance.scm")
(gnc:depend "report/balance-sheet.scm")
(gnc:depend "report/account-piecharts.scm")
(gnc:depend "report/category-barchart.scm")
(gnc:depend "report/pnl.scm")
(gnc:depend "report/hello-world.scm")
(gnc:depend "report/portfolio.scm")
(gnc:depend "report/price-scatter.scm")
(gnc:depend "report/register.scm")
(gnc:depend "report/iframe-url.scm") 
(gnc:depend "report/taxtxf.scm")
(gnc:depend "report/transaction-report.scm")

;; style sheets 
(gnc:depend "report/stylesheet-plain.scm")
(gnc:depend "report/stylesheet-fancy.scm")

;; view templates 
(gnc:depend "report/view-column.scm")

;; welcome to gnucash
(gnc:depend "report/welcome-to-gnucash.scm")
