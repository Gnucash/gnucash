# CMakeLists.txt for libgnucash/backend/dbi

ADD_SUBDIRECTORY(test)

SET (backend_dbi_SOURCES
  gnc-backend-dbi.cpp
  gnc-dbisqlresult.cpp
  gnc-dbisqlconnection.cpp
)
SET (backend_dbi_noinst_HEADERS
  gnc-backend-dbi.h
  gnc-backend-dbi.hpp
  gnc-dbisqlresult.hpp
  gnc-dbisqlconnection.hpp
  gnc-dbiprovider.hpp
  gnc-dbiproviderimpl.hpp
)

SET_LOCAL_DIST(backend_dbi_DIST_local
        ${backend_dbi_SOURCES} ${backend_dbi_noinst_HEADERS} CMakeLists.txt )
SET(backend_dbi_DIST ${backend_dbi_DIST_local} ${test_dbi_backend_DIST} PARENT_SCOPE)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${backend_dbi_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

IF (WITH_SQL)
  ADD_LIBRARY (gncmod-backend-dbi
    ${backend_dbi_SOURCES}
    ${backend_dbi_noinst_HEADERS}
  )

  SET(WINSOCK_LIB "")
  IF(MINGW64)
    SET(WINSOCK_LIB "-lws2_32")
  ENDIF(MINGW64)
  TARGET_LINK_LIBRARIES(gncmod-backend-dbi gnc-backend-sql gncmod-engine ${GTK2_LDFLAGS} ${Boost_REGEX_LIBRARY} ${LIBDBI_LIBRARY} ${WINSOCK_LIB})

  TARGET_COMPILE_DEFINITIONS(gncmod-backend-dbi PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.dbi\")

  TARGET_INCLUDE_DIRECTORIES(gncmod-backend-dbi PRIVATE ${LIBDBI_INCLUDE_PATH})

  IF (APPLE)
    SET_TARGET_PROPERTIES (gncmod-backend-dbi PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
  ENDIF()

  INSTALL(TARGETS gncmod-backend-dbi
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install
ENDIF(WITH_SQL)
