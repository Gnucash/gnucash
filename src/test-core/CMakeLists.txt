
SET(test_core_SOURCES
  test-stuff.c
  unittest-support.c
)

SET(test_core_noinst_HEADERS
  test-stuff.h
  unittest-support.h
)

INCLUDE_DIRECTORIES(
  ${CMAKE_BINARY_DIR}/src
  ${CMAKE_SOURCE_DIR}/src
  ${CMAKE_SOURCE_DIR}/src/libqof/qof
  ${CMAKE_SOURCE_DIR}/src/test-core

  ${GLIB2_INCLUDE_DIRS}
  ${GUILE_INCLUDE_DIRS}
)

ADD_LIBRARY(test-core ${test_core_SOURCES} ${test_core_noinst_HEADERS})
TARGET_LINK_LIBRARIES(test-core gnc-qof ${GLIB2_LDFLAGS})

SET (SWIG_UNITTEST_SUPPORT_GUILE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-unittest-support-guile.c)
GNC_ADD_SWIG_COMMAND (${SWIG_UNITTEST_SUPPORT_GUILE_C}
    ${CMAKE_CURRENT_SOURCE_DIR}/unittest-support.i ${test_core_HEADERS})


SET(test_core_guile_SOURCES ${SWIG_UNITTEST_SUPPORT_GUILE_C})

ADD_LIBRARY(test-core-guile ${test_core_guile_SOURCES})
TARGET_LINK_LIBRARIES(test-core-guile test-core ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS})

SET(test_core_SCHEME unittest-support.scm)

SET(GUILE_OUTPUT_DIR   gnucash)
SET(GUILE_MODULES      "")
SET(GUILE_LOAD_DIRS    "")
SET(GUILE_LIBRARY_DIRS src/test-core src/libqof)
SET(GUILE_DEPENDS      test-core-guile)


GNC_ADD_SCHEME_TARGETS(scm_test_core
  ${test_core_SCHEME}
  ${GUILE_OUTPUT_DIR}
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

INSTALL(FILES unittest-support.h DESTINATION libexec/gnucash/src/libqof/qof/test)