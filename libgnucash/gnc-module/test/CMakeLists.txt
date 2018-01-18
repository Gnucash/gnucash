
ADD_SUBDIRECTORY(mod-foo)
ADD_SUBDIRECTORY(mod-bar)
ADD_SUBDIRECTORY(mod-baz)
ADD_SUBDIRECTORY(misc-mods)

SET(GNC_MODULE_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/common # for config.h
  ${CMAKE_SOURCE_DIR}/libgnucash/gnc-module
  ${CMAKE_SOURCE_DIR}/common/test-core
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

SET(GNC_MODULE_TEST_LIBS gnc-module test-core)

MACRO(ADD_GNC_MODULE_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS)
ENDMACRO()

FUNCTION(RUN_TEST_WITH_GUILE _TARGET _SOURCE_FILE)
  GET_GUILE_ENV()
  SET(CMAKE_COMMAND_TMP "")
  IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
  SET(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env "${GUILE_ENV};GNC_MODULE_PATH=$${LIBDIR_BUILD}/gnucash/test")
  ENDIF()
  ADD_TEST(NAME ${_TARGET}
    COMMAND ${CMAKE_COMMAND_TMP}
      ${GUILE_EXECUTABLE} -s ${_SOURCE_FILE}
  )

  SET_TESTS_PROPERTIES(${_TARGET} PROPERTIES ENVIRONMENT "${GUILE_ENV};${ARGN}")
ENDFUNCTION()

GNC_ADD_TEST_WITH_GUILE(test-load-c test-load-c.c GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS "GNC_MODULE_PATH=${LIBDIR_BUILD}/gnucash/test")

GNC_ADD_TEST_WITH_GUILE(test-modsysver test-modsysver.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
)
GNC_ADD_TEST_WITH_GUILE(test-incompatdep test-incompatdep.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
)
GNC_ADD_TEST_WITH_GUILE(test-agedver test-agedver.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
  "GNC_MODULE_PATH=${LIBDIR_BUILD}/gnucash/test"
)

SET(_LIBDIR ${LIBDIR_BUILD})
IF (WIN32)
  SET(_LIBDIR ${CMAKE_BINARY_DIR}/bin)
ENDIF()
GNC_ADD_TEST(test-dynload test-dynload.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
  LIBDIR=${_LIBDIR}
)

SET(test_gnc_module_SCHEME
  test-gnc-module-scm-init.scm
  test-gnc-module-load-scm.scm
  test-gnc-module-swigged-c.scm
  test-gnc-module-load-deps.scm
  test-gnc-module-scm-dynload.scm
  test-gnc-module-scm-module.scm
  test-gnc-module-scm-multi.scm
  )

SET(GUILE_DEPENDS gnc-module scm-test-core scm-mod-foo scm-mod-baz)

GNC_ADD_SCHEME_TARGETS(test-gnc-modules-scm
  "${test_gnc_module_SCHEME}"
  "gnucash/gnc-modules/test"
  "${GUILE_DEPENDS}"
  TRUE
  )

GNC_ADD_SCHEME_TESTS("${test_gnc_module_SCHEME}")
IF(NOT WIN32)
  # This little dance is needed because gnc_module_init will assert if
  # it finds libgncmod_futuremod.so outside of a test that expects it.
  GET_GUILE_ENV()
  SET(_GNC_MODULE_PATH "${LIBDIR_BUILD}:${LIBDIR_BUILD}/gnucash:${LIBDIR_BUILD}/gnucash/test")
  FOREACH(test_file ${test_gnc_module_SCHEME})
    GET_FILENAME_COMPONENT(basename ${test_file} NAME_WE)
    SET_TESTS_PROPERTIES(${basename} PROPERTIES ENVIRONMENT "${GUILE_ENV};GNC_MODULE_PATH=${_GNC_MODULE_PATH}")
  ENDFOREACH()
ENDIF()

SET(test_gnc_module_SOURCE_DIST
  test-agedver.c
  test-dynload.c
  test-incompatdep.c
  test-load-c.c
  test-modsysver.c
)

SET(test_gnc_module_EXTRA_DIST
  README
  ${test_gnc_module_SCHEME}
)

SET_LOCAL_DIST(test_gnc_module_DIST_local CMakeLists.txt README ${test_gnc_module_SOURCE_DIST} ${test_gnc_module_EXTRA_DIST})

SET(test_gnc_module_DIST ${test_gnc_module_DIST_local} ${misc_mods_DIST}
                         ${mod_bar_DIST} ${mod_baz_DIST} ${mod_foo_DIST} PARENT_SCOPE)
