
add_subdirectory(standard/test)
add_subdirectory(support)

#These provide some functions used by more than one report.
set (reports_common_SCHEME
  aging.scm                     #deprecated 4.x to be removed in 5.x
  cash-flow-calc.scm
)

# The 'with exposed generator' reports are standard reports that can
# be created with a special generator function, exported by reports.scm
set (reports_standard_with_exposed_generator_SCHEME
    standard/new-aging.scm
    standard/register.scm
    standard/owner-report.scm   #deprecated 4.x to be removed in 5.x
    standard/new-owner-report.scm
    standard/payables.scm       #deprecated 4.x to be removed in 5.x
    standard/receivables.scm    #deprecated 4.x to be removed in 5.x
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
    standard/income-gst-statement.scm
    standard/income-statement.scm
    standard/lot-viewer.scm
    standard/net-charts.scm
    standard/portfolio.scm
    standard/price-scatter.scm
    standard/reconcile-report.scm
    standard/transaction.scm
    standard/trial-balance.scm
    standard/view-column.scm
    standard/taxinvoice.scm
    standard/receipt.scm
    standard/invoice.scm
    standard/job-report.scm
    standard/balsheet-eg.scm
)

# Reports depending on one of the generator functions from
# scm-reports-standard-with-exposed-generator:
set (reports_standard_SCHEME_2
    standard/customer-summary.scm # Depends on gnc:owner-report-create
)

set(reports_example_SCHEME
  example/average-balance.scm
  example/daily-reports.scm
  example/hello-world.scm
  example/sample-graphs.scm
  example/welcome-to-gnucash.scm
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

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports"
    NEW_MODULE "gnucash reports"
    DEPENDS "scm-rpt-reports")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report aging"
    NEW_MODULE "gnucash reports aging"
    DEPENDS "scm-reports-common")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports average-balance"
    NEW_MODULE "gnucash reports example average-balance"
    DEPENDS "scm-reports-example")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports daily-reports"
    NEW_MODULE "gnucash reports example daily-reports"
    DEPENDS "scm-reports-example")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report hello-world"
    NEW_MODULE "gnucash reports example hello-world"
    DEPENDS "scm-reports-example")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report test-graphing"
    NEW_MODULE "gnucash reports example sample-graphs"
    DEPENDS "scm-reports-example")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report locale-specific de_DE"
    NEW_MODULE "gnucash reports locale-specific de_DE taxtxf"
    DEPENDS "scm-reports-de_DE")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report taxtxf-de_DE"
    NEW_MODULE "gnucash reports locale-specific de_DE taxtxf"
    DEPENDS "scm-reports-de_DE")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report locale-specific us"
    NEW_MODULE "gnucash reports locale-specific us taxtxf"
    DEPENDS "scm-reports-us")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report taxtxf"
    NEW_MODULE "gnucash reports locale-specific us taxtxf"
    DEPENDS "scm-reports-us")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports account-piecharts"
    NEW_MODULE "gnucash reports standard account-piecharts"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports account-summary"
    NEW_MODULE "gnucash reports standard account-summary"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports advanced-portfolio"
    NEW_MODULE "gnucash reports standard advanced-portfolio"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports balance-forecast"
    NEW_MODULE "gnucash reports standard balance-forecast"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports balance-sheet"
    NEW_MODULE "gnucash reports standard balance-sheet"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report balsheet-eg"
    NEW_MODULE "gnucash reports standard balsheet-eg"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports balsheet-pnl"
    NEW_MODULE "gnucash reports standard balsheet-pnl"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports budget-balance-sheet"
    NEW_MODULE "gnucash reports standard budget-balance-sheet"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports budget-barchart"
    NEW_MODULE "gnucash reports standard budget-barchart"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports budget-flow"
    NEW_MODULE "gnucash reports standard budget-flow"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports budget"
    NEW_MODULE "gnucash reports standard budget"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports budget-income-statement"
    NEW_MODULE "gnucash reports standard budget-income-statement"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports cashflow-barchart"
    NEW_MODULE "gnucash reports standard cashflow-barchart"
    DEPENDS "scm-reports-standard-2")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports cash-flow"
    NEW_MODULE "gnucash reports standard cash-flow"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports category-barchart"
    NEW_MODULE "gnucash reports standard category-barchart"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report customer-summary"
    NEW_MODULE "gnucash reports standard customer-summary"
    DEPENDS "scm-reports-standard-2")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report welcome-to-gnucash"
    NEW_MODULE "gnucash reports standard dashboard"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports equity-statement"
    NEW_MODULE "gnucash reports standard equity-statement"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports general-journal"
    NEW_MODULE "gnucash reports standard general-journal"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports general-ledger"
    NEW_MODULE "gnucash reports standard general-ledger"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports income-gst-statement"
    NEW_MODULE "gnucash reports standard income-gst-statement"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports income-statement"
    NEW_MODULE "gnucash reports standard income-statement"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report invoice"
    NEW_MODULE "gnucash reports standard invoice"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report job-report"
    NEW_MODULE "gnucash reports standard job-report"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports net-charts"
    NEW_MODULE "gnucash reports standard net-charts"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report owner-report"
    NEW_MODULE "gnucash reports standard owner-report"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report payables"
    NEW_MODULE "gnucash reports standard payables"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports portfolio"
    NEW_MODULE "gnucash reports standard portfolio"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports price-scatter"
    NEW_MODULE "gnucash reports standard price-scatter"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report receipt"
    NEW_MODULE "gnucash reports standard receipt"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report receivables"
    NEW_MODULE "gnucash reports standard receivables"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports reconcile-report"
    NEW_MODULE "gnucash reports standard reconcile-report"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports register"
    NEW_MODULE "gnucash reports standard register"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports sx-summary"
    NEW_MODULE "gnucash reports standard account-summary"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report taxinvoice"
    NEW_MODULE "gnucash reports standard taxinvoice"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports transaction"
    NEW_MODULE "gnucash reports standard transaction"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report standard-reports trial-balance"
    NEW_MODULE "gnucash reports standard trial-balance"
    DEPENDS "scm-reports-standard")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash report view-column"
    NEW_MODULE "gnucash reports standard view-column"
    DEPENDS "scm-reports-standard")

set_local_dist(reports_DIST_local CMakeLists.txt ${reports_SCHEME}
    ${reports_common_SCHEME} ${reports_standard_SCHEME}  ${reports_standard_SCHEME_2}
    ${reports_example_SCHEME} ${reports_us_SCHEME} ${reports_de_DE_SCHEME}
    ${reports_standard_with_exposed_generator_SCHEME})
set(reports_DIST ${reports_DIST_local} ${reports_support_DIST}
    ${test_reports_standard_DIST} PARENT_SCOPE)
