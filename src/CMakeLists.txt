# CMakeLists.txt for src/

# The subdirectories
ADD_SUBDIRECTORY (core-utils)
ADD_SUBDIRECTORY (debug)
ADD_SUBDIRECTORY (doc)
ADD_SUBDIRECTORY (gnc-module)
ADD_SUBDIRECTORY (engine)
ADD_SUBDIRECTORY (backend)
ADD_SUBDIRECTORY (app-utils)
ADD_SUBDIRECTORY (pixmaps)
ADD_SUBDIRECTORY (quotes)
ADD_SUBDIRECTORY (scm)
ADD_SUBDIRECTORY (tax)
ADD_SUBDIRECTORY (test-core)

IF (WITH_GNUCASH)
  ADD_SUBDIRECTORY (bin)
  ADD_SUBDIRECTORY (cmake_modules)
  ADD_SUBDIRECTORY (gnome)
  ADD_SUBDIRECTORY (gnome-utils)
  ADD_SUBDIRECTORY (gnome-search)
  ADD_SUBDIRECTORY (html)
  ADD_SUBDIRECTORY (import-export)
  ADD_SUBDIRECTORY (optional)
  ADD_SUBDIRECTORY (plugins)
  ADD_SUBDIRECTORY (python)
  ADD_SUBDIRECTORY (register)
  ADD_SUBDIRECTORY(report)
ENDIF (WITH_GNUCASH)

SET(src_EXTRA_DIST
        base-typemaps.i
        config.h.cmake.in
        gnc-test-env.pl
        guile-mappings.h
        platform.h
        README
        README.modules)

SET_LOCAL_DIST(src_DIST_local CMakeLists.txt Makefile.am ${src_EXTRA_DIST})

SET(src_DIST ${src_DIST_local} ${app_utils_DIST} ${backend_DIST} ${bin_DIST} ${cmake_modules_DIST}
             ${core_utils_DIST} ${debug_DIST} ${doc_DIST} ${engine_DIST} ${gnc_module_DIST} ${gnome_DIST} ${gnome_search_DIST}
             ${gnome_utils_DIST} ${html_DIST} ${import_export_DIST} ${optional_DIST} ${plugins_DIST} ${pixmaps_DIST} ${python_DIST}
             ${quotes_DIST} ${register_DIST} ${report_DIST} ${scm_DIST} ${tax_DIST} ${test_core_DIST} PARENT_SCOPE)
