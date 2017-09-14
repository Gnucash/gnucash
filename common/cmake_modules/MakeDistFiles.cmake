
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
  libgnucash/app-utils/migratable-prefs.xml
  libgnucash/app-utils/swig-app-utils-guile.c
  libgnucash/app-utils/swig-app-utils-python.c
  libgnucash/app-utils/test/test-load-module
  libgnucash/backend/xml/test/test-real-data.sh
  gnucash/gnucash.rc
  gnucash/overrides/gnucash-make-guids
  gnucash/test/test-version
  libgnucash/core-utils/gnc-vcs-info.h
  libgnucash/core-utils/swig-core-utils-guile.c
  libgnucash/core-utils/swig-core-utils-python.c
  libgnucash/doc/design/gnucash-design.info
  libgnucash/engine/iso-4217-currencies.c
  libgnucash/engine/swig-engine.c
  libgnucash/engine/test/test-create-account
  libgnucash/engine/test/test-scm-query-import
  libgnucash/gnc-module/swig-gnc-module.c
  libgnucash/gnc-module/test/mod-bar/swig-bar.c
  libgnucash/gnc-module/test/mod-baz/swig-baz.c
  libgnucash/gnc-module/test/mod-foo/swig-foo.c
  libgnucash/gnc-module/test/test-gwrapped-c
  libgnucash/gnc-module/test/test-load-deps
  libgnucash/gnc-module/test/test-load-scm
  libgnucash/gnc-module/test/test-scm-dynload
  libgnucash/gnc-module/test/test-scm-init
  libgnucash/gnc-module/test/test-scm-module
  libgnucash/gnc-module/test/test-scm-multi
  gnucash/gnome/gnucash.desktop.in
  gnucash/gnome/swig-gnome.c
  gnucash/gnome-utils/gnc-warnings.c
  gnucash/gnome-utils/swig-gnome-utils.c
  gnucash/gnome-utils/test/test-load-module
  gnucash/html/swig-gnc-html.c
  bindings/python/gnucash_core.c
  gnucash/report/report-gnome/swig-report-gnome.c
  gnucash/report/report-gnome/test/test-load-module
  gnucash/report/report-system/swig-report-system.c
  gnucash/report/report-system/test/test-load-module
  gnucash/report/stylesheets/test/test-load-module
  libgnucash/scm/build-config.scm
  common/swig-runtime.h
  common/test-core/swig-unittest-support-guile.c
  common/test-core/swig-unittest-support-python.c
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
        po/gnucash.pot
        libgnucash/doc/design/stamp-vti
        libgnucash/doc/design/version.texi
        bindings/python/gnucash_core_c.py
        common/test-core/unittest_support.py

        )
