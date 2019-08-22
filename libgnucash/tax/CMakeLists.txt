add_subdirectory(test)

set(locale_tax_SOURCES gncmod-locale-tax.c)

# Add dependency on config.h
set_source_files_properties (${locale_tax_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library(gncmod-locale-tax ${locale_tax_SOURCES})
target_link_libraries(gncmod-locale-tax gnc-module ${GLIB2_LDFLAGS} ${GUILE_LDFLAGS})

target_include_directories(gncmod-locale-tax
    PRIVATE ${CMAKE_BINARY_DIR}/common ${GUILE_INCLUDE_DIRS})

if (APPLE)
  set_target_properties (gncmod-locale-tax PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-locale-tax
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

# Scheme

set(gncmod_tax_us_SCHEME_1 us/txf.scm us/txf-help.scm)
set(gncmod_tax_us_SCHEME_2 us/tax-us.scm)

set(gncmod_tax_de_DE_SCHEME_1 de_DE/txf-de_DE.scm de_DE/txf-help-de_DE.scm)
set(gncmod_tax_de_DE_SCHEME_2 de_DE/tax-de_DE.scm)

set(GUILE_DEPENDS scm-app-utils scm-gnc-module)

gnc_add_scheme_targets(scm-tax-us-1
"${gncmod_tax_us_SCHEME_1}"
"gnucash/tax-us"
"${GUILE_DEPENDS}"
TRUE
)
gnc_add_scheme_targets(scm-tax-us-2
"${gncmod_tax_us_SCHEME_2}"
gnucash
"${GUILE_DEPENDS}"
FALSE
)

gnc_add_scheme_targets(scm-tax-de_DE-1
"${gncmod_tax_de_DE_SCHEME_1}"
"gnucash/tax-de_DE"
"${GUILE_DEPENDS}"
TRUE
)
gnc_add_scheme_targets(scm-tax-de_DE-2
"${gncmod_tax_de_DE_SCHEME_2}"
gnucash
"${GUILE_DEPENDS}"
FALSE
)

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module ("gnucash tax de_DE" "gnucash tax-de_DE" "scm-tax-de_DE-2" "")
gnc_add_scheme_deprecated_module ("gnucash tax us" "gnucash tax-us" "scm-tax-us-2" "")

add_custom_target(scm-locale-tax ALL DEPENDS scm-tax-us-1 scm-tax-us-2 scm-tax-de_DE-1 scm-tax-de_DE-2 )

set(de_DE_tax_EXTRA_DIST
    "de_DE/taxtxf-LIES MICH.txt"
    "de_DE/txf-Ust-Bericht.html"
    "de_DE/txf-Ust-VA Anleitung 2011.pdf")
set_local_dist(tax_DIST_local CMakeLists.txt ${locale_tax_SOURCES}
    ${gncmod_tax_us_SCHEME_1} ${gncmod_tax_us_SCHEME_2}
    ${gncmod_tax_de_DE_SCHEME_1} ${gncmod_tax_de_DE_SCHEME_2}
    $de_DE_tax_EXTRA_DIST)
set(tax_DIST ${tax_DIST_local} ${locale_tax_DIST} ${test_locale_tax_DIST} PARENT_SCOPE)
