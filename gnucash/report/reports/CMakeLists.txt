
add_subdirectory(standard/test)

set (reports_common_SCHEME reports.scm)

set (reports_standard_SCHEME
    standard/account-piecharts.scm
    standard/account-summary.scm
    standard/advanced-portfolio.scm
    standard/average-balance.scm
    standard/balance-sheet.scm
    standard/balance-forecast.scm
    standard/balsheet-pnl.scm
    standard/budget-balance-sheet.scm
    standard/budget-barchart.scm
    standard/budget-flow.scm
    standard/budget-income-statement.scm
    standard/budget.scm
    standard/cash-flow.scm
    standard/cashflow-barchart.scm
    standard/category-barchart.scm
    standard/daily-reports.scm
    standard/equity-statement.scm
    standard/general-journal.scm
    standard/general-ledger.scm
    standard/income-gst-statement.scm
    standard/income-statement.scm
    standard/net-charts.scm
    standard/portfolio.scm
    standard/price-scatter.scm
    standard/reconcile-report.scm
    standard/register.scm
    standard/sx-summary.scm
    standard/transaction.scm
    standard/trial-balance.scm
    )

set(GUILE_DEPENDS
  gncmod-html
  scm-core-utils
  scm-engine
  scm-gettext
  scm-scm
  scm-gnome-utils
  scm-report-system
  )

gnc_add_scheme_targets(scm-reports-common
  "${reports_common_SCHEME}"
  "gnucash/report"
  "scm-reports-standard;${GUILE_DEPENDS}"
  TRUE
)

gnc_add_scheme_targets(scm-reports-standard
  "${reports_standard_SCHEME}"
  "gnucash/report/reports/standard"
  "scm-reports-standard-links;${GUILE_DEPENDS}"
  TRUE
)

add_custom_target(scm-reports ALL DEPENDS scm-reports-standard scm-reports-common)

set_local_dist(reports_DIST_local CMakeLists.txt ${reports_common_SCHEME} ${reports_standard_SCHEME})
set(reports_DIST ${reports_DIST_local} ${test_reports_standard_DIST} PARENT_SCOPE)
