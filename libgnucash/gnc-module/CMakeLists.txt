# CMakeLists.txt for libgnucash/gnc-module
add_subdirectory(test)
add_subdirectory(example)

# Command to generate the swig-gnc-module.c wrapper file
gnc_add_swig_guile_command (swig-gnc-module-c
    SWIG_GNC_MODULE_C swig-gnc-module.c
    ${CMAKE_CURRENT_SOURCE_DIR}/gnc-module.i
)

set (gnc_module_SOURCES gnc-module.c)

# Add dependency on config.h
set_source_files_properties (${gnc_module_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set (gnc_module_HEADERS
  gnc-module.h
  gnc-module-api.h
)

add_library	(gnc-module
  ${gnc_module_SOURCES}
  ${gnc_module_HEADERS}
  ${SWIG_GNC_MODULE_C}
)

target_link_libraries(gnc-module ${GUILE_LDFLAGS} ${GMODULE_LDFLAGS} ${GLIB2_LDFLAGS})

target_compile_definitions (gnc-module PRIVATE -DG_LOG_DOMAIN=\"gnc.module\")

target_include_directories (gnc-module
    PRIVATE ${GUILE_INCLUDE_DIRS}
    PUBLIC  ${CMAKE_CURRENT_SOURCE_DIR}
            ${CMAKE_SOURCE_DIR}/common
            ${CMAKE_BINARY_DIR}/common # for config.h
            ${GLIB2_INCLUDE_DIRS}
)

install(TARGETS gnc-module
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${gnc_module_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)

# --- Compile Scheme file(s) ---

set (gnc_module_SCHEME gnc-module.scm)

set(GUILE_OUTPUT_DIR   gnucash)
set(GUILE_DEPENDS      gnc-module)

gnc_add_scheme_targets(scm-gnc-module
  "${gnc_module_SCHEME}"
  ${GUILE_OUTPUT_DIR}
  "${GUILE_DEPENDS}"
  FALSE
)

set_local_dist(gnc_module_DIST_local CMakeLists.txt ${gnc_module_SOURCES} ${gnc_module_HEADERS}
        gnc-module.i gnc-module.scm README)
set(gnc_module_DIST ${gnc_module_DIST_local} ${test_gnc_module_DIST} ${example_DIST} PARENT_SCOPE)
