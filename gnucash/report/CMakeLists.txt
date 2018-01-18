
ADD_SUBDIRECTORY(business-reports)
ADD_SUBDIRECTORY(jqplot)
ADD_SUBDIRECTORY(locale-specific)
ADD_SUBDIRECTORY(report-gnome)
ADD_SUBDIRECTORY(report-system)
ADD_SUBDIRECTORY(standard-reports)
ADD_SUBDIRECTORY(stylesheets)
ADD_SUBDIRECTORY(utility-reports)

SET_LOCAL_DIST(report_DIST_local CMakeLists.txt )
SET(report_DIST ${report_DIST_local} ${business_reports_DIST} ${jqplot_DIST} ${locale_specific_DIST} ${report_gnome_DIST} ${report_system_DIST}
                ${standard_reports_DIST} ${stylesheets_DIST} ${utility_reports_DIST}  PARENT_SCOPE)