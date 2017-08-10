# CMakeLists.txt for gnucash/

# The subdirectories
ADD_SUBDIRECTORY (bin)
ADD_SUBDIRECTORY (gnome)
ADD_SUBDIRECTORY (gnome-utils)
ADD_SUBDIRECTORY (gnome-search)
ADD_SUBDIRECTORY (html)
ADD_SUBDIRECTORY (import-export)
ADD_SUBDIRECTORY (plugins)
ADD_SUBDIRECTORY (python)
ADD_SUBDIRECTORY (register)
ADD_SUBDIRECTORY(report)

ADD_DEFINITIONS (-DHAVE_CONFIG_H)

SET_LOCAL_DIST(gnucash_DIST_local CMakeLists.txt Makefile.am ${gnucash_EXTRA_DIST})

SET(gnucash_DIST ${gnucash_DIST_local} ${bin_DIST} ${gnome_DIST} ${gnome_search_DIST}
             ${gnome_utils_DIST} ${html_DIST} ${import_export_DIST} ${plugins_DIST} ${python_DIST}
             ${register_DIST} ${report_DIST} PARENT_SCOPE)
