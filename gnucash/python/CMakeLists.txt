ADD_SUBDIRECTORY(pycons)

SET(gncmod_python_SOURCES gncmod-python.c)

IF (WITH_PYTHON)

  ADD_LIBRARY(gncmod-python ${gncmod_python_SOURCES})
  TARGET_LINK_LIBRARIES(gncmod-python gnc-module gnc-core-utils-python gncmod-app-utils-python
   ${PYTHON_LIBRARIES} ${GLIB_LIBS})   # ${PYTHON_EXTRA_LIBS}
  TARGET_INCLUDE_DIRECTORIES(gncmod-python
    PRIVATE ${CMAKE_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/core_utils ${CMAKE_SOURCE_DIR}/gnc-module ${PYTHON_INCLUDE_DIR})
  TARGET_COMPILE_OPTIONS(gncmod-python PRIVATE -DG_LOG_DOMAIN=\"gnc.python\")
IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-python PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()
  INSTALL(TARGETS gncmod-python
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )

  INSTALL(DIRECTORY pycons DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/python
    PATTERN Makefile.* EXCLUDE
    PATTERN CMake* EXCLUDE
    PATTERN CTest* EXCLUDE
    PATTERN cmake* EXCLUDE)
  INSTALL(FILES init.py DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/python)

  FILE(COPY pycons DESTINATION ${CMAKE_BINARY_DIR}/share/gnucash/python
    PATTERN Makefile.* EXCLUDE
    PATTERN CMake* EXCLUDE
    PATTERN CTest* EXCLUDE
    PATTERN cmake* EXCLUDE)
  FILE(COPY init.py DESTINATION ${CMAKE_BINARY_DIR}/share/gnucash/python)
ENDIF(WITH_PYTHON)

SET_LOCAL_DIST(python_DIST_local CMakeLists.txt gncmod-python.c init.py)
SET(python_DIST ${python_DIST_local} ${pycons_DIST} PARENT_SCOPE)

