add_subdirectory(test)

set(locale_tax_SOURCES gnc-locale-tax.c gnc-locale-tax.h)

# Add dependency on config.h
set_source_files_properties (${locale_tax_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library(gnc-locale-tax ${locale_tax_SOURCES})
target_link_libraries(gnc-locale-tax
    ${GLIB2_LDFLAGS}
    ${GUILE_LDFLAGS})

target_include_directories(gnc-locale-tax
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE
        ${CMAKE_BINARY_DIR}/common
        ${GUILE_INCLUDE_DIRS}
        ${GLIB2_INCLUDE_DIRS})

if (APPLE)
  set_target_properties (gnc-locale-tax PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}")
endif()

install(TARGETS gnc-locale-tax
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

# Scheme

set(gncmod_tax_us_SCHEME_1 us/txf.scm us/txf-help.scm)
set(gncmod_tax_us_SCHEME_2 us/tax.scm)

set(gncmod_tax_de_DE_SCHEME_1 de_DE/txf.scm de_DE/txf-help.scm)
set(gncmod_tax_de_DE_SCHEME_2 de_DE/tax.scm)

set(GUILE_DEPENDS scm-app-utils)

gnc_add_scheme_targets(scm-tax-us-1
    SOURCES "${gncmod_tax_us_SCHEME_1}"
    OUTPUT_DIR "gnucash/locale/us/tax"
    DEPENDS "${GUILE_DEPENDS}"
    MAKE_LINKS)
                       
gnc_add_scheme_targets(scm-tax-us-2
    SOURCES "${gncmod_tax_us_SCHEME_2}"
    OUTPUT_DIR "gnucash/locale/us"
    DEPENDS "scm-tax-us-1;${GUILE_DEPENDS}")

gnc_add_scheme_targets(scm-tax-de_DE-1
    SOURCES "${gncmod_tax_de_DE_SCHEME_1}"
    OUTPUT_DIR "gnucash/locale/de_DE/tax"
    DEPENDS "${GUILE_DEPENDS}"
    MAKE_LINKS)

gnc_add_scheme_targets(scm-tax-de_DE-2
    SOURCES "${gncmod_tax_de_DE_SCHEME_2}"
    OUTPUT_DIR "gnucash/locale/de_DE"
    DEPENDS "scm-tax-de_DE-1;${GUILE_DEPENDS}")

# Module interfaces deprecated in 4.x, will be removed for 5.x
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash tax de_DE"
    NEW_MODULE "gnucash locale de_DE tax"
    DEPENDS "scm-tax-de_DE-2")
gnc_add_scheme_deprecated_module (
    OLD_MODULE "gnucash tax us"
    NEW_MODULE "gnucash locale us tax"
    DEPENDS "scm-tax-us-2")

add_custom_target(scm-locale-tax ALL DEPENDS scm-tax-us-1 scm-tax-us-2 scm-tax-de_DE-1 scm-tax-de_DE-2 )

set(de_DE_tax_EXTRA_DIST
    "de_DE/taxtxf - LIES MICH.txt"
    "de_DE/txf-USt-Bericht.html"
    "de_DE/txf-USt-VA Anleitung 2011.pdf")
set_local_dist(tax_DIST_local CMakeLists.txt ${locale_tax_SOURCES}
    ${gncmod_tax_us_SCHEME_1} ${gncmod_tax_us_SCHEME_2}
    ${gncmod_tax_de_DE_SCHEME_1} ${gncmod_tax_de_DE_SCHEME_2}
    ${de_DE_tax_EXTRA_DIST})
set(tax_DIST ${tax_DIST_local} ${locale_tax_DIST} ${test_locale_tax_DIST} PARENT_SCOPE)
