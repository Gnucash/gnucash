
add_subdirectory(standard/test)
add_subdirectory(support)

set (reports_common_SCHEME
  aging.scm
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
    standard/net-charts.scm
    standard/new-owner-report.scm
    standard/portfolio.scm
    standard/price-scatter.scm
    standard/reconcile-report.scm
    standard/register.scm
    standard/transaction.scm
    standard/trial-balance.scm
    standard/view-column.scm
    standard/taxinvoice.scm
    standard/receipt.scm
    standard/invoice.scm
    standard/job-report.scm
    standard/owner-report.scm
    standard/payables.scm
    standard/receivables.scm
    standard/balsheet-eg.scm
)

set (reports_standard_SCHEME_2
    standard/customer-summary.scm # Depends on owner-report
    standard/new-aging.scm        # ditto
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
  gncmod-html
  scm-core-utils
  scm-engine
  scm-gettext
  scm-gnome-utils
  scm-report
  )

set(scm_rpt_example_GUILE_DEPENDS
  gncmod-html
  scm-app-utils
  scm-core-utils
  scm-gettext
  scm-gnc-module
  scm-engine
  scm-reports-example-links
  scm-gnome-utils
  scm-report
  )

set(scm_rpt_locale_specific_GUILE_DEPENDS
  gncmod-html
  scm-core-utils
  scm-gettext
  scm-gnc-module
  scm-engine
  scm-test-core
  scm-locale-tax
  scm-report
  scm-gnome-utils
  )

set(scm_rpt_std_GUILE_DEPENDS
  ${scm_rpts_GUILE_DEPENDS}
  scm-app-utils
  scm-gnc-module
  scm-rpt-reports
  scm-reports-common-links
  scm-reports-standard-links
  )

gnc_add_scheme_targets(scm-reports-common
  "${reports_common_SCHEME}"
  "gnucash/reports"
  "scm-reports-common-links;${scm_rpt_std_GUILE_DEPENDS}"
  TRUE
)

gnc_add_scheme_targets(scm-rpt-reports
  "${reports_SCHEME}"
  "gnucash"
  "${scm_rpts_GUILE_DEPENDS}"
  TRUE
)

gnc_add_scheme_targets(scm-reports-standard
  "${reports_standard_SCHEME}"
  "gnucash/reports/standard"
  "scm-reports-common;${scm_rpt_std_GUILE_DEPENDS}"
  TRUE
)

gnc_add_scheme_targets(scm-reports-example
  "${reports_example_SCHEME}"
  "gnucash/reports/example"
  "${scm_rpt_example_GUILE_DEPENDS}"
  TRUE
)

gnc_add_scheme_targets(scm-reports-us
  "${reports_us_SCHEME}"
  "gnucash/reports/locale-specific/us"
  "${scm_rpt_locale_specific_GUILE_DEPENDS}"
  TRUE
)

gnc_add_scheme_targets(scm-reports-de_DE
  "${reports_de_DE_SCHEME}"
  "gnucash/reports/locale-specific/de_DE"
  "${scm_rpt_locale_specific_GUILE_DEPENDS}"
  TRUE
)

gnc_add_scheme_targets(scm-reports-standard-2
  "${reports_standard_SCHEME_2}"
  "gnucash/reports/standard"
  "scm-rpt-reports"
  TRUE
)

add_custom_target(scm-reports ALL DEPENDS
    scm-reports-standard
    scm-reports-standard-2
    scm-reports-example
    scm-reports-common
    scm-rpt-reports
    scm-reports-us
    scm-reports-de_DE)

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module ("gnucash report standard-reports" "gnucash reports" "scm-rpt-reports" "")
gnc_add_scheme_deprecated_module ("gnucash report aging" "gnucash reports aging" "scm-reports-common" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports average-balance" "gnucash reports example average-balance" "scm-reports-example" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports daily-reports" "gnucash reports example daily-reports" "scm-reports-example" "")
gnc_add_scheme_deprecated_module ("gnucash report hello-world" "gnucash reports example hello-world" "scm-reports-example" "")
gnc_add_scheme_deprecated_module ("gnucash report test-graphing" "gnucash reports example sample-graphs" "scm-reports-example" "")
gnc_add_scheme_deprecated_module ("gnucash report locale-specific de_DE" "gnucash reports locale-specific de_DE taxtxf" "scm-reports-de_DE" "")
gnc_add_scheme_deprecated_module ("gnucash report taxtxf-de_DE" "gnucash reports locale-specific de_DE taxtxf" "scm-reports-de_DE" "")
gnc_add_scheme_deprecated_module ("gnucash report locale-specific us" "gnucash reports locale-specific us taxtxf" "scm-reports-us" "")
gnc_add_scheme_deprecated_module ("gnucash report taxtxf" "gnucash reports locale-specific us taxtxf" "scm-reports-us" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports account-piecharts" "gnucash reports standard account-piecharts" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports account-summary" "gnucash reports standard account-summary" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports advanced-portfolio" "gnucash reports standard advanced-portfolio" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports balance-forecast" "gnucash reports standard balance-forecast" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports balance-sheet" "gnucash reports standard balance-sheet" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report balsheet-eg" "gnucash reports standard balsheet-eg" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports balsheet-pnl" "gnucash reports standard balsheet-pnl" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports budget-balance-sheet" "gnucash reports standard budget-balance-sheet" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports budget-barchart" "gnucash reports standard budget-barchart" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports budget-flow" "gnucash reports standard budget-flow" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports budget" "gnucash reports standard budget" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports budget-income-statement" "gnucash reports standard budget-income-statement" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports cashflow-barchart" "gnucash reports standard cashflow-barchart" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports cash-flow" "gnucash reports standard cash-flow" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports category-barchart" "gnucash reports standard category-barchart" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report customer-summary" "gnucash reports standard customer-summary" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report welcome-to-gnucash" "gnucash reports standard dashboard" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports equity-statement" "gnucash reports standard equity-statement" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports general-journal" "gnucash reports standard general-journal" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports general-ledger" "gnucash reports standard general-ledger" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports income-gst-statement" "gnucash reports standard income-gst-statement" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports income-statement" "gnucash reports standard income-statement" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report invoice" "gnucash reports standard invoice" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report job-report" "gnucash reports standard job-report" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports net-charts" "gnucash reports standard net-charts" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report owner-report" "gnucash reports standard owner-report" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report payables" "gnucash reports standard payables" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports portfolio" "gnucash reports standard portfolio" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports price-scatter" "gnucash reports standard price-scatter" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report receipt" "gnucash reports standard receipt" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report receivables" "gnucash reports standard receivables" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports reconcile-report" "gnucash reports standard reconcile-report" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports register" "gnucash reports standard register" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports sx-summary" "gnucash reports standard account-summary" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report taxinvoice" "gnucash reports standard taxinvoice" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports transaction" "gnucash reports standard transaction" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report standard-reports trial-balance" "gnucash reports standard trial-balance" "scm-reports-standard" "")
gnc_add_scheme_deprecated_module ("gnucash report view-column" "gnucash reports standard view-column" "scm-reports-standard" "")

set_local_dist(reports_DIST_local CMakeLists.txt ${reports_SCHEME}
    ${reports_common_SCHEME} ${reports_standard_SCHEME}  ${reports_standard_SCHEME_2}
    ${reports_example_SCHEME} ${reports_us_SCHEME} ${reports_de_DE_SCHEME})
set(reports_DIST ${reports_DIST_local} ${reports_support_DIST}
    ${test_reports_standard_DIST} PARENT_SCOPE)
