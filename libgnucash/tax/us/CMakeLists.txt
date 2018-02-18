ADD_SUBDIRECTORY(test)

SET(tax_us_SOURCES gncmod-tax-us.c)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${tax_us_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

ADD_LIBRARY(gncmod-tax-us ${tax_us_SOURCES})
TARGET_LINK_LIBRARIES(gncmod-tax-us gnc-module ${GLIB2_LDFLAGS} ${GUILE_LDFLAGS})

TARGET_INCLUDE_DIRECTORIES(gncmod-tax-us
    PRIVATE ${CMAKE_BINARY_DIR}/common ${GUILE_INCLUDE_DIRS})

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-tax-us PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-tax-us
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install

# Scheme

SET(gncmod_tax_us_SCHEME_1 txf.scm txf-help.scm txf-de_DE.scm txf-help-de_DE.scm)
SET(gncmod_tax_us_SCHEME_2 de_DE.scm us.scm)

SET(GUILE_DEPENDS      scm-app-utils scm-gnc-module)

GNC_ADD_SCHEME_TARGETS(scm-tax-us-1
  "${gncmod_tax_us_SCHEME_1}"
  ""
  "${GUILE_DEPENDS}"
  FALSE
  )
GNC_ADD_SCHEME_TARGETS(scm-tax-us-2
  "${gncmod_tax_us_SCHEME_2}"
  gnucash/tax
  "${GUILE_DEPENDS}"
  FALSE
  )

add_custom_target(scm-tax-us ALL DEPENDS scm-tax-us-1 scm-tax-us-2)

SET_LOCAL_DIST(tax_us_DIST_local CMakeLists.txt ${tax_us_SOURCES} ${gncmod_tax_us_SCHEME_1} ${gncmod_tax_us_SCHEME_2})
SET(tax_us_DIST ${tax_us_DIST_local} ${test_tax_us_DIST} PARENT_SCOPE)

