# CMakeLists.txt for libgnucash/backend/dbi

add_subdirectory(test)

set (backend_dbi_SOURCES
  gnc-backend-dbi.cpp
  gnc-dbisqlresult.cpp
  gnc-dbisqlconnection.cpp
)
set (backend_dbi_noinst_HEADERS
  gnc-backend-dbi.h
  gnc-backend-dbi.hpp
  gnc-dbisqlresult.hpp
  gnc-dbisqlconnection.hpp
  gnc-dbiprovider.hpp
  gnc-dbiproviderimpl.hpp
)

set_local_dist(backend_dbi_DIST_local
        ${backend_dbi_SOURCES} ${backend_dbi_noinst_HEADERS} CMakeLists.txt )
set(backend_dbi_DIST ${backend_dbi_DIST_local} ${test_dbi_backend_DIST} PARENT_SCOPE)

# Add dependency on config.h
set_source_files_properties (${backend_dbi_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

if (WITH_SQL)
  add_library (gncmod-backend-dbi
    ${backend_dbi_SOURCES}
    ${backend_dbi_noinst_HEADERS}
  )

  set(WINSOCK_LIB "")
  if(MINGW64)
    set(WINSOCK_LIB "-lws2_32")
  endif()
  target_link_libraries(gncmod-backend-dbi gnc-backend-sql gncmod-engine ${GTK2_LDFLAGS} ${Boost_REGEX_LIBRARY} ${LIBDBI_LIBRARY} ${WINSOCK_LIB})

  target_compile_definitions(gncmod-backend-dbi PRIVATE -DG_LOG_DOMAIN=\"gnc.backend.dbi\")

  target_include_directories(gncmod-backend-dbi PRIVATE ${LIBDBI_INCLUDE_PATH})

  if (APPLE)
    set_target_properties (gncmod-backend-dbi PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
  endif()

  install(TARGETS gncmod-backend-dbi
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install
endif()
