
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

SET_DIST_LIST(test_core_DIST ${test_core_SOURCES} ${test_core_noinst_HEADERS} CMakeLists.txt
        Makefile.am unittest-support.i unittest-support.scm)

ADD_LIBRARY(test-core STATIC ${test_core_SOURCES} ${test_core_noinst_HEADERS})
TARGET_LINK_LIBRARIES(test-core gncmod-engine gnc-qof ${GLIB2_LDFLAGS})
IF (UNIX)
  TARGET_COMPILE_OPTIONS(test-core PRIVATE -fPIC)
ENDIF()

IF (BUILDING_FROM_VCS)
  SET (SWIG_UNITTEST_SUPPORT_GUILE_C ${CMAKE_CURRENT_BINARY_DIR}/swig-unittest-support-guile.c)
  GNC_ADD_SWIG_COMMAND (swig-unittest-support-guile-c ${SWIG_UNITTEST_SUPPORT_GUILE_C}
      ${CMAKE_CURRENT_SOURCE_DIR}/unittest-support.i ${test_core_HEADERS})
  SET (SWIG_UNITTEST_SUPPORT_PYTHON_C ${CMAKE_CURRENT_BINARY_DIR}/swig-unittest-support-python.c)
  GNC_ADD_SWIG_PYTHON_COMMAND (swig-unittest-support-python ${SWIG_UNITTEST_SUPPORT_PYTHON_C} ${CMAKE_CURRENT_SOURCE_DIR}/unittest-support.i)
ELSE()
  SET (SWIG_UNITTEST_SUPPORT_GUILE_C  swig-unittest-support-guile.c)
  SET (SWIG_UNITTEST_SUPPORT_PYTHON_C swig-unittest-support-python.c)
ENDIF()


ADD_LIBRARY(test-core-guile ${SWIG_UNITTEST_SUPPORT_GUILE_C})
TARGET_LINK_LIBRARIES(test-core-guile test-core ${GUILE_LDFLAGS} ${GLIB2_LDFLAGS})

IF (WITH_PYTHON)
  ADD_LIBRARY(unittest_support MODULE ${SWIG_UNITTEST_SUPPORT_PYTHON_C})
  TARGET_LINK_LIBRARIES(unittest_support test-core ${PYTHON_LIBRARIES})
  TARGET_INCLUDE_DIRECTORIES(unittest_support PRIVATE ${PYTHON_INCLUDE_DIRS})
  SET_TARGET_PROPERTIES(unittest_support PROPERTIES PREFIX "_")
ENDIF()

SET(test_core_SCHEME unittest-support.scm)

SET(GUILE_OUTPUT_DIR   gnucash)
SET(GUILE_MODULES      "")
SET(GUILE_LOAD_DIRS    "")
SET(GUILE_LIBRARY_DIRS src/test-core src/libqof)
SET(GUILE_DEPENDS      test-core-guile)


GNC_ADD_SCHEME_TARGETS(scm-test-core
  ${test_core_SCHEME}
  ${GUILE_OUTPUT_DIR}
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)

INSTALL(FILES unittest-support.h DESTINATION libexec/gnucash/src/libqof/qof/test)
