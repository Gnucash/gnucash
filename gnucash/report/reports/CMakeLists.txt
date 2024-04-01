
add_subdirectory(standard/test)
add_subdirectory(support)

#These provide some functions used by more than one report.
set (reports_common_SCHEME
  cash-flow-calc.scm
)

# The 'with exposed generator' reports are standard reports that can
# be created with a special generator function, exported by reports.scm
set (reports_standard_with_exposed_generator_SCHEME
    standard/new-aging.scm
    standard/register.scm
    standard/new-owner-report.scm
)

set (reports_standard_SCHEME
    standard/account-piecharts.scm
    standard/account-summary.scm
    standard/advanced-portfolio.scm
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
    standard/dashboard.scm
    standard/equity-statement.scm
    standard/general-journal.scm
    standard/general-ledger.scm
    standard/ifrs-cost-basis.scm
    standard/income-gst-statement.scm
    standard/income-statement.scm
    standard/investment-lots.scm
    standard/lot-viewer.scm
    standard/net-charts.scm
    standard/portfolio.scm
    standard/price-scatter.scm
    standard/reconcile-report.scm
    standard/transaction.scm
    standard/trial-balance.scm
    standard/txn-columns.scm
    standard/view-column.scm
    standard/taxinvoice.scm
    standard/receipt.scm
    standard/invoice.scm
    standard/balsheet-eg.scm
)

# Reports depending on one of the generator functions from
# scm-reports-standard-with-exposed-generator:
set (reports_standard_SCHEME_2
    standard/customer-summary.scm # Depends on gnc:owner-report-create-with-enddate
)

set(reports_example_SCHEME
  example/average-balance.scm
  example/daily-reports.scm
  example/sample-report.scm
  example/sample-graphs.scm
)

set(reports_us_SCHEME
  locale-specific/us/taxtxf.scm
)

set(reports_de_DE_SCHEME
  locale-specific/de_DE/taxtxf.scm
)

set (reports_SCHEME
  reports.scm
)

set(scm_rpts_GUILE_DEPENDS
  scm-gnc-html
  scm-core-utils
  scm-engine
  scm-gnome-utils
  scm-app-utils
  scm-report
  )

set(scm_rpt_example_GUILE_DEPENDS
  ${scm_rpts_GUILE_DEPENDS}
  scm-reports-example-links
  )

set(scm_rpt_locale_specific_GUILE_DEPENDS
  ${scm_rpts_GUILE_DEPENDS}
  scm-test-core
  scm-locale-tax
  )

set(scm_rpt_std_w_exposed_generator_GUILE_DEPENDS
  ${scm_rpts_GUILE_DEPENDS}
  scm-reports-common-links
  scm-reports-standard-links
  )

set(scm_rpt_std_GUILE_DEPENDS
  ${scm_rpt_std_w_exposed_generator_GUILE_DEPENDS}
  scm-rpt-reports
  )

gnc_add_scheme_targets(scm-reports-common
    SOURCES "${reports_common_SCHEME}"
    OUTPUT_DIR "gnucash/reports"
    DEPENDS "scm-reports-common-links;${scm_rpts_GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-reports-standard-with-exposed-generator
    SOURCES "${reports_standard_with_exposed_generator_SCHEME}"
    OUTPUT_DIR "gnucash/reports/standard"
    DEPENDS "scm-reports-common;${scm_rpt_std_w_exposed_generator_GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-rpt-reports
    SOURCES "${reports_SCHEME}"
    OUTPUT_DIR "gnucash"
    DEPENDS "scm-reports-standard-with-exposed-generator;${scm_rpts_GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-reports-standard
    SOURCES "${reports_standard_SCHEME}"
    OUTPUT_DIR "gnucash/reports/standard"
    DEPENDS "scm-reports-common;${scm_rpt_std_GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-reports-example
    SOURCES "${reports_example_SCHEME}"
    OUTPUT_DIR "gnucash/reports/example"
    DEPENDS "${scm_rpt_example_GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-reports-us
    SOURCES "${reports_us_SCHEME}"
    OUTPUT_DIR "gnucash/reports/locale-specific/us"
    DEPENDS "${scm_rpt_locale_specific_GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-reports-de_DE
    SOURCES "${reports_de_DE_SCHEME}"
    OUTPUT_DIR "gnucash/reports/locale-specific/de_DE"
    DEPENDS "${scm_rpt_locale_specific_GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-reports-standard-2
    SOURCES "${reports_standard_SCHEME_2}"
    OUTPUT_DIR "gnucash/reports/standard"
    DEPENDS "scm-rpt-reports;scm-reports-standard"
    MAKE_LINKS)

add_custom_target(scm-reports ALL DEPENDS
    scm-reports-standard
    scm-reports-standard-2
    scm-reports-example
    scm-reports-common
    scm-rpt-reports
    scm-reports-us
    scm-reports-de_DE)


set_local_dist(reports_DIST_local CMakeLists.txt ${reports_SCHEME}
    ${reports_common_SCHEME} ${reports_standard_SCHEME}  ${reports_standard_SCHEME_2}
    ${reports_example_SCHEME} ${reports_us_SCHEME} ${reports_de_DE_SCHEME}
    ${reports_standard_with_exposed_generator_SCHEME})
set(reports_DIST ${reports_DIST_local} ${reports_support_DIST}
    ${test_reports_standard_DIST} PARENT_SCOPE)
