ADD_SUBDIRECTORY(gschemas)
ADD_SUBDIRECTORY(gtkbuilder)
ADD_SUBDIRECTORY(ui)

SET(business_gnome_SOURCES
  business-options-gnome.c
  business-urls.c
  business-gnome-utils.c
  dialog-billterms.c
  dialog-choose-owner.c
  dialog-customer.c
  dialog-date-close.c
  dialog-employee.c
  dialog-invoice.c
  dialog-job.c
  dialog-order.c
  dialog-payment.c
  dialog-vendor.c
  gnc-plugin-business.c
  gnc-plugin-page-invoice.c
  gnc-plugin-page-owner-tree.c
  gncmod-business-gnome.c
  search-owner.c
)

SET(business_gnome_noinst_HEADERS
  business-options-gnome.h
  business-urls.h
  business-gnome-utils.h
  dialog-billterms.h
  dialog-choose-owner.h
  dialog-customer.h
  dialog-date-close.h
  dialog-employee.h
  dialog-invoice.h
  dialog-job.h
  dialog-order.h
  dialog-payment.h
  dialog-vendor.h
  gnc-plugin-business.h
  gnc-plugin-page-invoice.h
  gnc-plugin-page-owner-tree.h
  search-owner.h
)  

SET_SOURCE_FILES_PROPERTIES (${business_gnome_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})
SET_SOURCE_FILES_PROPERTIES (business-options-gnome.c PROPERTIES OBJECT_DEPENDS ${SWIG_RUNTIME_H})
ADD_LIBRARY(gncmod-business-gnome
  ${business_gnome_SOURCES}
  ${business_gnome_HEADERS}
)

TARGET_LINK_LIBRARIES(gncmod-business-gnome gnc-business-ledger gncmod-gnome-search 
   gncmod-report-gnome gncmod-gnome-utils gnc-gnome gncmod-engine gnc-qof ${GTK2_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-business-gnome PRIVATE -DG_LOG_DOMAIN=\"gnc.business.gnome\")

INSTALL(TARGETS gncmod-business-gnome
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install

# Scheme

SET(gncmod_business_gnome_SCHEME business-gnome.scm)

SET(GUILE_OUTPUT_DIR   gnucash)
SET(GUILE_MODULES      src/app-utils src/engine src/gnome-utils src/html src/report/report-system src/report/standard-reports)
SET(GUILE_LOAD_DIRS    src/app-utils src/engine src/core-utils src/gnc-module src/gnome-utils src/report/business-reports src/report/report-system src/report/standard-reports src/scm)
SET(GUILE_LIBRARY_DIRS src/app-utils src/engine src/core-utils src/gnc-module src/gnome-utils src/report/report-system)
SET(GUILE_DEPENDS      scm-business-reports)

GNC_ADD_SCHEME_TARGETS(scm-business-gnome
  "${gncmod_business_gnome_SCHEME}"
  ${GUILE_OUTPUT_DIR} 
  "${GUILE_MODULES}"
  "${GUILE_LOAD_DIRS}"
  "${GUILE_LIBRARY_DIRS}"
  "${GUILE_DEPENDS}"
  FALSE
)
SET_LOCAL_DIST(business_gnome_DIST_local CMakeLists.txt Makefile.am ${business_gnome_SOURCES} ${business_gnome_HEADERS}
               ${business_gnome_noinst_HEADERS} ${gncmod_business_gnome_SCHEME})

SET(business_gnome_DIST ${business_gnome_DIST_local} ${business_gnome_gschema_DIST}
        ${business_gnome_gtkbuilder_DIST} ${business_gnome_ui_DIST} PARENT_SCOPE)