
#Tests for this directory are not run.
add_subdirectory(test)

set(qif_SOURCES
  qif-context.c
  qif-defaults.c
  qif-file.c
  qif-objects.c
  qif-parse.c
)

# Add dependency on config.h
set_source_files_properties (${qif_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set(qif_noinst_HEADERS
  qif-file.h
  qif-defaults.h
  qif-import-p.h
  qif-import.h
  qif-objects.h
  qif-objects-p.h
  qif-parse.h
)

add_library(gncmod-qif ${qif_noinst_HEADERS} ${qif_SOURCES})

target_link_libraries(gncmod-qif gncmod-generic-import gncmod-engine ${GLIB2_LDFLAGS})

target_compile_definitions(gncmod-qif PRIVATE -DG_LOG_DOMAIN=\"gnc.import.qif\")

if (APPLE)
  set_target_properties (gncmod-qif PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-qif
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

set_local_dist(qif_DIST_local CMakeLists.txt ${qif_SOURCES} ${qif_noinst_HEADERS})
set(qif_DIST ${qif_DIST_local} ${test_qif_DIST} PARENT_SCOPE)
