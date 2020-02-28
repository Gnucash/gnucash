
add_subdirectory(test)

set (standard_reports_SCHEME standard-reports.scm)

set (standard_reports_SCHEME_2
    account-piecharts.scm
    account-summary.scm
    advanced-portfolio.scm
    average-balance.scm
    balance-sheet.scm
    balance-forecast.scm
    balsheet-pnl.scm
    budget-balance-sheet.scm
    budget-barchart.scm
    budget-flow.scm
    budget-income-statement.scm
    budget.scm
    cash-flow.scm
    cashflow-barchart.scm
    category-barchart.scm
    daily-reports.scm
    equity-statement.scm
    general-journal.scm
    general-ledger.scm
    income-gst-statement.scm
    income-statement.scm
    net-charts.scm
    portfolio.scm
    price-scatter.scm
    reconcile-report.scm
    register.scm
    transaction.scm
    trial-balance.scm
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

gnc_add_scheme_targets(scm-standard-reports-1
  "${standard_reports_SCHEME}"
  "gnucash/report"
  "scm-standard-reports-2;${GUILE_DEPENDS}"
  TRUE
)

gnc_add_scheme_targets(scm-standard-reports-2
  "${standard_reports_SCHEME_2}"
  "gnucash/report/standard-reports"
  "scm-standard-reports-2-links;${GUILE_DEPENDS}"
  TRUE
)

add_custom_target(scm-standard-reports ALL DEPENDS scm-standard-reports-2 scm-standard-reports-1)

set_local_dist(standard_reports_DIST_local CMakeLists.txt ${standard_reports_SCHEME} ${standard_reports_SCHEME_2})
set(standard_reports_DIST ${standard_reports_DIST_local} ${test_standard_reports_DIST} PARENT_SCOPE)
