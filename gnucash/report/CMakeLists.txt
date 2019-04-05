
add_subdirectory(jqplot)
add_subdirectory(locale-specific)
add_subdirectory(report-gnome)
add_subdirectory(report-system)
add_subdirectory(reports)
add_subdirectory(stylesheets)

set_local_dist(report_DIST_local CMakeLists.txt )
set(report_DIST ${report_DIST_local} ${jqplot_DIST} ${locale_specific_DIST} ${report_gnome_DIST} ${report_system_DIST}
                ${reports_DIST} ${stylesheets_DIST} PARENT_SCOPE)
