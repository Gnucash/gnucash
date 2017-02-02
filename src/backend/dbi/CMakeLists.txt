# CMakeLists.txt for src/backend/dbi

ADD_SUBDIRECTORY(test)

# Source file gncmod-backend-dbi.c does not appear to be use in Makefile.in, so not included here.

SET (backend_dbi_SOURCES
  gnc-backend-dbi.c
)
SET (backend_dbi_noinst_HEADERS
  gnc-backend-dbi.h gnc-backend-dbi-priv.h
)

SET_LOCAL_DIST(backend_dbi_DIST_local
        ${backend_dbi_SOURCES} ${backend_dbi_noinst_HEADERS} CMakeLists.txt Makefile.am)
SET(backend_dbi_DIST ${backend_dbi_DIST_local} ${test_dbi_backend_DIST} PARENT_SCOPE)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${backend_dbi_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY	(gncmod-backend-dbi
  ${backend_dbi_SOURCES}
  ${backend_dbi_noinst_HEADERS}
)

TARGET_LINK_LIBRARIES(gncmod-backend-dbi gnc-backend-sql gncmod-engine gnc-qof ${GTK2_LDFLAGS} ${LIBDBI_LIBRARY})

TARGET_COMPILE_DEFINITIONS(gncmod-backend-dbi PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.dbi\")

TARGET_INCLUDE_DIRECTORIES(gncmod-backend-dbi PRIVATE ${LIBDBI_INCLUDE_PATH})

INSTALL(TARGETS gncmod-backend-dbi
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install

# FIXME: Below is a hack to create .libs/libgncmod-backend-dbi to silence some complaints
# from guile when compiling. Eventually, fix the guile module loader to not expect items
# to be in .libs. At this time, WIN32 does not use guile-2, so no need for this.

IF (NOT WIN32)
  SET(_LINK_TARGET ${CMAKE_CURRENT_BINARY_DIR}/.libs/libgncmod-backend-dbi${CMAKE_SHARED_LIBRARY_SUFFIX})
  FILE(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/.libs)

  ADD_CUSTOM_COMMAND(
      OUTPUT ${_LINK_TARGET}
      COMMAND ${CMAKE_COMMAND} -E create_symlink $<TARGET_FILE:gncmod-backend-dbi> ${_LINK_TARGET}
      DEPENDS gnc-backend-dbi
  )

  ADD_CUSTOM_TARGET(gncmod-backend-dbi-link ALL DEPENDS gncmod-backend-dbi ${_LINK_TARGET})
ENDIF()
