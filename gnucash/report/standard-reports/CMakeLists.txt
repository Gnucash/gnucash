
ADD_SUBDIRECTORY(test)

SET (standard_reports_SCHEME standard-reports.scm)

SET (standard_reports_SCHEME_2
    account-piecharts.scm
    account-summary.scm
    advanced-portfolio.scm
    average-balance.scm
    balance-sheet.scm
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
    net-barchart.scm
    net-linechart.scm
    portfolio.scm
    price-scatter.scm
    register.scm
    sx-summary.scm
    transaction.scm
    trial-balance.scm
    )

SET(GUILE_DEPENDS      gncmod-html scm-app-utils scm-gnome-utils scm-report-system scm-report-system-3)

GNC_ADD_SCHEME_TARGETS(scm-standard-reports-1
  "${standard_reports_SCHEME}"
  "gnucash/report"
  "scm-standard-reports-2;${GUILE_DEPENDS}"
  TRUE
)

GNC_ADD_SCHEME_TARGETS(scm-standard-reports-2
  "${standard_reports_SCHEME_2}"
  "gnucash/report/standard-reports"
  "scm-standard-reports-2-links;${GUILE_DEPENDS}"
  TRUE
)

ADD_CUSTOM_TARGET(scm-standard-reports ALL DEPENDS scm-standard-reports-2 scm-standard-reports-1)

# FIXME: This is a hack since core-utils assumes an autotools build environment.

SET(STD_RPT_DIR ${CMAKE_CURRENT_BINARY_DIR}/gnucash/report)
FILE(MAKE_DIRECTORY ${STD_RPT_DIR})
FILE(COPY standard-reports.scm DESTINATION ${STD_RPT_DIR})

FILE(MAKE_DIRECTORY ${STD_RPT_DIR}/standard-reports)
FILE(COPY ${standard_reports_SCHEME_2} DESTINATION ${STD_RPT_DIR}/standard-reports)

SET_LOCAL_DIST(standard_reports_DIST_local CMakeLists.txt ${standard_reports_SCHEME} ${standard_reports_SCHEME_2})
SET(standard_reports_DIST ${standard_reports_DIST_local} ${test_standard_reports_DIST} PARENT_SCOPE)
