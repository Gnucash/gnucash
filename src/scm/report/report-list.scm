;; Index file to load all of the releavant reports.
(gnc:support "report/report-list.scm")

;; Helper functions for reports (which haven't been included
;; elsewhere)
(gnc:depend "options-utilities.scm")

;; reports 
(gnc:depend "report/account-summary.scm")
(gnc:depend "report/average-balance.scm")
(gnc:depend "report/pnl.scm")
(gnc:depend "report/hello-world.scm")

;; style sheets 
(gnc:depend "report/stylesheet-plain.scm")
(gnc:depend "report/stylesheet-fancy.scm")
