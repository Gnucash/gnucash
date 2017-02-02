
ADD_SUBDIRECTORY(mod-foo)
ADD_SUBDIRECTORY(mod-bar)
ADD_SUBDIRECTORY(mod-baz)
ADD_SUBDIRECTORY(misc-mods)

SET(GNC_MODULE_TEST_INCLUDE_DIRS
  ${CMAKE_BINARY_DIR}/src # for config.h
  ${CMAKE_SOURCE_DIR}/src/gnc-module
  ${CMAKE_SOURCE_DIR}/src/test-core
  ${CMAKE_SOURCE_DIR}/src/libqof/qof # for qof.h
  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

SET(GNC_MODULE_TEST_LIBS gnc-module gnc-qof test-core)

MACRO(ADD_GNC_MODULE_TEST _TARGET _SOURCE_FILES)
  GNC_ADD_TEST(${_TARGET} "${_SOURCE_FILES}" GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS)
ENDMACRO()

FUNCTION(RUN_TEST_WITH_GUILE _TARGET _SOURCE_FILE)
  GET_GUILE_ENV()
  SET(CMAKE_COMMAND_TMP "")
  IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
    SET(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env "${GUILE_ENV}")
  ENDIF()
  ADD_TEST(NAME ${_TARGET}
    COMMAND ${CMAKE_COMMAND_TMP}
      ${GUILE_EXECUTABLE} -s ${_SOURCE_FILE}
  )

  SET_TESTS_PROPERTIES(${_TARGET} PROPERTIES ENVIRONMENT "${GUILE_ENV};${ARGN}")
ENDFUNCTION()

GNC_ADD_TEST_WITH_GUILE(test-load-c test-load-c.c GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS)

RUN_TEST_WITH_GUILE(test-load-scm ${CMAKE_CURRENT_SOURCE_DIR}/test-load-scm.in)

RUN_TEST_WITH_GUILE(test-load-deps ${CMAKE_CURRENT_SOURCE_DIR}/test-load-deps.in)

RUN_TEST_WITH_GUILE(test-scm-init ${CMAKE_CURRENT_SOURCE_DIR}/test-scm-init.in)

RUN_TEST_WITH_GUILE(test-scm-multi ${CMAKE_CURRENT_SOURCE_DIR}/test-scm-multi.in)

RUN_TEST_WITH_GUILE(test-scm-dynload ${CMAKE_CURRENT_SOURCE_DIR}/test-scm-dynload.in)

GNC_ADD_TEST_WITH_GUILE(test-modsysver test-modsysver.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
)
GNC_ADD_TEST_WITH_GUILE(test-incompatdep test-incompatdep.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
)
GNC_ADD_TEST_WITH_GUILE(test-agedver test-agedver.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
)

SET(_LIBDIR ${CMAKE_BINARY_DIR}/lib)
IF (WIN32)
  SET(_LIBDIR ${CMAKE_BINARY_DIR}/bin)
ENDIF()
GNC_ADD_TEST(test-dynload test-dynload.c
  GNC_MODULE_TEST_INCLUDE_DIRS GNC_MODULE_TEST_LIBS
  LIBDIR=${_LIBDIR}
)

SET(GUILE ${GUILE_EXECUTABLE})
CONFIGURE_FILE(test-gwrapped-c.in test-gwrapped-c @ONLY)
CONFIGURE_FILE(test-scm-module.in test-scm-module @ONLY)
CONFIGURE_FILE(test-load-deps.in test-load-deps @ONLY)
CONFIGURE_FILE(test-load-scm.in test-load-scm @ONLY)
CONFIGURE_FILE(test-scm-dynload.in test-scm-dynload @ONLY)
CONFIGURE_FILE(test-scm-init.in test-scm-init @ONLY)
CONFIGURE_FILE(test-scm-multi.in test-scm-multi @ONLY)

FUNCTION(RUN_GUILE_SHELL_TEST _TARGET _SOURCE_FILE)
  GET_GUILE_ENV()
  SET(CMAKE_COMMAND_TMP "")
  IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
    SET(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env "${GUILE_ENV}")
  ENDIF()
  ADD_TEST(NAME ${_TARGET}
    COMMAND ${CMAKE_COMMAND_TMP}
      ${SHELL} ${_SOURCE_FILE}
  )
  SET_TESTS_PROPERTIES(${_TARGET} PROPERTIES ENVIRONMENT "${GUILE_ENV}")
ENDFUNCTION()

RUN_GUILE_SHELL_TEST(test-gwrapped-c ${CMAKE_CURRENT_BINARY_DIR}/test-gwrapped-c)

RUN_GUILE_SHELL_TEST(test-scm-module ${CMAKE_CURRENT_BINARY_DIR}/test-scm-module)

SET(test_gnc_module_SOURCE_DIST
  test-agedver.c
  test-dynload.c
  test-incompatdep.c
  test-load-c.c
  test-modsysver.c
)

SET(test_gnc_module_EXTRA_DIST
  README
  test-gwrapped-c.in
  test-load-deps.in
  test-load-scm.in
  test-scm-dynload.in
  test-scm-init.in
  test-scm-module.in
  test-scm-multi.in
)

SET_LOCAL_DIST(test_gnc_module_DIST_local CMakeLists.txt Makefile.am README ${test_gnc_module_SOURCE_DIST} ${test_gnc_module_EXTRA_DIST})

SET(test_gnc_module_DIST ${test_gnc_module_DIST_local} ${misc_mods_DIST}
                         ${mod_bar_DIST} ${mod_baz_DIST} ${mod_foo_DIST} PARENT_SCOPE)