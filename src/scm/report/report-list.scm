;; Index file to load all of the releavant reports.
(gnc:support "report/report-list.scm")

;; Helper functions for reports (which haven't been included
;; elsewhere)
(gnc:depend "options-utilities.scm")

;; reports 
(gnc:depend "report/account-summary.scm")
(gnc:depend "report/average-balance.scm")
(gnc:depend "report/hello-world.scm")
(gnc:depend "report/pnl.scm")
(let ((locale (setlocale LC_MESSAGES)))
  (if (or (equal? locale "C")
          (equal? locale "en")
          (equal? locale "en_US"))
      (gnc:depend "report/taxtxf.scm")))
(gnc:depend "report/transaction-report.scm")

;; style sheets 
(gnc:depend "report/stylesheet-plain.scm")
(gnc:depend "report/stylesheet-fancy.scm")
