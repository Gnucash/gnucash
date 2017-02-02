
IF (${CMAKE_VERSION} VERSION_LESS 3.3)
    INCLUDE(CMakeParseArguments)
ENDIF()


FUNCTION(SET_LOCAL_DIST output)
    SET(dist_files "")
    FOREACH(file ${ARGN})
        FILE(RELATIVE_PATH relative ${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/${file})
        LIST(APPEND dist_files ${relative})
    ENDFOREACH()
    SET (${output} ${dist_files} PARENT_SCOPE)
ENDFUNCTION()

MACRO(SET_DIST_LIST output)
    SET_LOCAL_DIST(${output}_TMP ${ARGN})
    SET(${output} ${${output}_TMP} PARENT_SCOPE)
ENDMACRO()

FUNCTION(EXECUTE_PROCESS_AND_CHECK_RESULT)
    cmake_parse_arguments(VARS "" "WORKING_DIRECTORY;ERROR_MSG" "COMMAND" ${ARGN})
    EXECUTE_PROCESS(
            COMMAND ${VARS_COMMAND}
            WORKING_DIRECTORY ${VARS_WORKING_DIRECTORY}
            RESULT_VARIABLE RESULT
    )
    IF (NOT "${RESULT}" STREQUAL "0")
        MESSAGE(FATAL_ERROR ${VARS_ERROR_MSG})
    ENDIF()
ENDFUNCTION()


# This is a list of files generated at build time that
# should be copied into the dist tarball. An item in
# this list should be a file, not a directory or glob.
# File in this list become dependenices of the 'dist'
# target.


SET(COPY_FROM_BUILD
  ChangeLog
  src/app-utils/migratable-prefs.xml
  src/app-utils/swig-app-utils-guile.c
  src/app-utils/swig-app-utils-python.c
  src/app-utils/test/test-load-module
  src/backend/xml/test/test-real-data.sh
  src/bin/gnucash.rc
  src/bin/overrides/gnucash-make-guids
  src/bin/test/test-version
  src/core-utils/gnc-vcs-info.h
  src/core-utils/swig-core-utils-guile.c
  src/core-utils/swig-core-utils-python.c
  src/doc/design/gnucash-design.info
  src/engine/iso-4217-currencies.c
  src/engine/swig-engine.c
  src/engine/test/test-create-account
  src/engine/test/test-scm-query-import
  src/gnc-module/swig-gnc-module.c
  src/gnc-module/test/mod-bar/swig-bar.c
  src/gnc-module/test/mod-baz/swig-baz.c
  src/gnc-module/test/mod-foo/swig-foo.c
  src/gnc-module/test/test-gwrapped-c
  src/gnc-module/test/test-load-deps
  src/gnc-module/test/test-load-scm
  src/gnc-module/test/test-scm-dynload
  src/gnc-module/test/test-scm-init
  src/gnc-module/test/test-scm-module
  src/gnc-module/test/test-scm-multi
  src/gnome/gnucash.desktop.in
  src/gnome/swig-gnome.c
  src/gnome-utils/gnc-warnings.c
  src/gnome-utils/swig-gnome-utils.c
  src/gnome-utils/test/test-load-module
  src/html/swig-gnc-html.c
  src/optional/python-bindings/gnucash_core.c
  src/report/report-gnome/swig-report-gnome.c
  src/report/report-gnome/test/test-load-module
  src/report/report-system/swig-report-system.c
  src/report/report-system/test/test-load-module
  src/report/standard-reports/test/test-load-module
  src/report/stylesheets/test/test-load-module
  src/report/utility-reports/test/test-load-module
  src/scm/build-config.scm
  src/swig-runtime.h
  src/test-core/swig-unittest-support-guile.c
  src/test-core/swig-unittest-support-python.c
)

# This list is similiar to the COPY_FROM_BUILD list
# above, except that we don't create an explicit
# dependency on this for the 'dist' target. I need
# to fix the creation of these files so that we
# can add the as dependencies for 'dist'. These
# file are not generated using CONFIGURE_FILE(),
# so CMake does not realize these are generated files.

# Items marked with GNC_CONFIGURE can be
# properly generated when we drop autotools, because
# then the source file can use the @XXX@ convention
# instead of @-XXX-@

SET(COPY_FROM_BUILD_2
        doc/gnucash.1  # Uses GNC_CONFIGURE
        packaging/gnucash.spec # Uses GNC_CONFIGURE
        po/gnucash.pot
        src/doc/design/stamp-vti
        src/doc/design/version.texi
        src/pixmaps/128x128/gnucash-icon.png
        src/pixmaps/16x16/gnucash-icon.png
        src/pixmaps/22x22/gnucash-icon.png
        src/pixmaps/24x24/gnucash-icon.png
        src/pixmaps/256x256/gnucash-icon.png
        src/pixmaps/32x32/gnucash-icon.png
        src/pixmaps/48x48/gnucash-icon.png
        src/pixmaps/64x64/gnucash-icon.png
        src/pixmaps/96x96/gnucash-icon.png
        src/pixmaps/gnucash-icon-16x16.png
        src/pixmaps/gnucash-icon-32x32.png
        src/pixmaps/scalable/gnucash-icon.svg
        src/optional/python-bindings/gnucash_core_c.py
        src/test-core/unittest_support.py

        )