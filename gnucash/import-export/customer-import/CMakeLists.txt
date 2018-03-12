SET(customer_import_SOURCES
  gnc-plugin-customer-import.c
  gncmod-customer-import.c
  dialog-customer-import-gui.c
  dialog-customer-import.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${customer_import_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})


SET(customer_import_noinst_HEADERS
  gnc-plugin-customer-import.h
  dialog-customer-import-gui.h
  dialog-customer-import.h
)

ADD_LIBRARY(gncmod-customer-import ${customer_import_SOURCES} ${customer_input_noinst_HEADERS})

TARGET_LINK_LIBRARIES(gncmod-customer-import gnc-gnome gncmod-gnome-utils gncmod-app-utils
     gncmod-engine gnc-core-utils gnc-module ${GLIB2_LDFLAGS})

TARGET_INCLUDE_DIRECTORIES(gncmod-customer-import
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${CMAKE_SOURCE_DIR}/gnucash/gnome
)

TARGET_COMPILE_DEFINITIONS(gncmod-customer-import PRIVATE -DG_LOG_DOMAIN=\"gnc.plugin.customer-import\")

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-customer-import PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

INSTALL(TARGETS gncmod-customer-import
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
# No headers to install.

SET_DIST_LIST (customer_import_DIST CMakeLists.txt ${customer_import_SOURCES} ${customer_import_noinst_HEADERS})
