add_subdirectory(test)

set(tax_us_SOURCES gncmod-tax-us.c)

# Add dependency on config.h
set_source_files_properties (${tax_us_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library(gncmod-tax-us ${tax_us_SOURCES})
target_link_libraries(gncmod-tax-us gnc-module ${GLIB2_LDFLAGS} ${GUILE_LDFLAGS})

target_include_directories(gncmod-tax-us
    PRIVATE ${CMAKE_BINARY_DIR}/common ${GUILE_INCLUDE_DIRS})

if (APPLE)
  set_target_properties (gncmod-tax-us PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-tax-us
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

# Scheme

set(gncmod_tax_us_SCHEME_1 txf.scm txf-help.scm txf-de_DE.scm txf-help-de_DE.scm)
set(gncmod_tax_us_SCHEME_2 de_DE.scm us.scm)

set(GUILE_DEPENDS      scm-app-utils scm-gnc-module)

gnc_add_scheme_targets(scm-tax-us-1
  "${gncmod_tax_us_SCHEME_1}"
  ""
  "${GUILE_DEPENDS}"
  FALSE
  )
gnc_add_scheme_targets(scm-tax-us-2
  "${gncmod_tax_us_SCHEME_2}"
  gnucash/tax
  "${GUILE_DEPENDS}"
  FALSE
  )

add_custom_target(scm-tax-us ALL DEPENDS scm-tax-us-1 scm-tax-us-2)

set_local_dist(tax_us_DIST_local CMakeLists.txt ${tax_us_SOURCES} ${gncmod_tax_us_SCHEME_1} ${gncmod_tax_us_SCHEME_2})
set(tax_us_DIST ${tax_us_DIST_local} ${test_tax_us_DIST} PARENT_SCOPE)

