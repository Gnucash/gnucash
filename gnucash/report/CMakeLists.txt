
add_subdirectory(business-reports)
add_subdirectory(jqplot)
add_subdirectory(locale-specific)
add_subdirectory(report-gnome)
add_subdirectory(report-system)
add_subdirectory(standard-reports)
add_subdirectory(stylesheets)
add_subdirectory(utility-reports)

set_local_dist(report_DIST_local CMakeLists.txt )
set(report_DIST ${report_DIST_local} ${business_reports_DIST} ${jqplot_DIST} ${locale_specific_DIST} ${report_gnome_DIST} ${report_system_DIST}
                ${standard_reports_DIST} ${stylesheets_DIST} ${utility_reports_DIST}  PARENT_SCOPE)