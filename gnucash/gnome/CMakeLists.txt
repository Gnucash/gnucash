
ADD_SUBDIRECTORY(gschemas)
ADD_SUBDIRECTORY(gtkbuilder)
ADD_SUBDIRECTORY(ui)

SET (gnc_gnome_noinst_HEADERS
  assistant-acct-period.h
  assistant-hierarchy.h
  assistant-loan.h
  assistant-stock-split.h
  business-options-gnome.h
  business-urls.h
  business-gnome-utils.h
  dialog-billterms.h
  dialog-choose-owner.h
  dialog-customer.h
  dialog-date-close.h
  dialog-employee.h
  dialog-fincalc.h
  dialog-find-account.h
  dialog-find-transactions.h
  dialog-find-transactions2.h
  dialog-imap-editor.h
  dialog-invoice.h
  dialog-job.h
  dialog-lot-viewer.h
  dialog-new-user.h
  dialog-order.h
  dialog-payment.h
  dialog-print-check.h
  dialog-progress.h
  dialog-sx-editor.h
  dialog-sx-editor2.h
  dialog-sx-from-trans.h
  dialog-sx-since-last-run.h
  dialog-trans-assoc.h
  dialog-vendor.h
  gnc-budget-view.h
  gnc-plugin-account-tree.h
  gnc-plugin-basic-commands.h
  gnc-plugin-budget.h
  gnc-plugin-business.h
  gnc-plugin-register.h
  gnc-plugin-register2.h
  gnc-plugin-page-account-tree.h
  gnc-plugin-page-budget.h
  gnc-plugin-page-invoice.h
  gnc-plugin-page-owner-tree.h
  gnc-plugin-page-sx-list.h
  gnc-plugin-page-register.h
  gnc-plugin-page-register2.h
  gnc-split-reg.h
  gnc-split-reg2.h
  reconcile-view.h
  search-owner.h
  top-level.h
  window-reconcile.h
  window-reconcile2.h
  window-autoclear.h
)

IF (BUILDING_FROM_VCS)
  SET (SWIG_GNOME_C ${CMAKE_CURRENT_BINARY_DIR}/swig-gnome.c)
  GNC_ADD_SWIG_COMMAND (swig-gnome-c ${SWIG_GNOME_C} ${CMAKE_CURRENT_SOURCE_DIR}/gnome.i ${gnc_gnome_HEADERS})
ELSE()
  SET (SWIG_GNOME_C swig-gnome.c)
ENDIF()

SET (gnc_gnome_SOURCES 

  assistant-acct-period.c
  assistant-hierarchy.c
  assistant-loan.c
  assistant-stock-split.c
  business-options-gnome.c
  business-urls.c
  business-gnome-utils.c
  dialog-billterms.c
  dialog-choose-owner.c
  dialog-commodities.c
  dialog-customer.c
  dialog-date-close.c
  dialog-employee.c
  dialog-fincalc.c
  dialog-find-account.c
  dialog-find-transactions.c
  dialog-find-transactions2.c
  dialog-imap-editor.c
  dialog-invoice.c
  dialog-job.c
  dialog-lot-viewer.c
  dialog-new-user.c
  dialog-order.c
  dialog-payment.c
  dialog-price-editor.c
  dialog-price-edit-db.c
  dialog-print-check.c
  dialog-progress.c
  dialog-sx-editor.c
  dialog-sx-editor2.c
  dialog-sx-from-trans.c
  dialog-sx-since-last-run.c
  dialog-tax-info.c
  dialog-trans-assoc.c
  dialog-vendor.c
  gnc-budget-view.c
  gnc-plugin-account-tree.c
  gnc-plugin-basic-commands.c
  gnc-plugin-budget.c
  gnc-plugin-business.c
  gnc-plugin-register.c
  gnc-plugin-register2.c
  gnc-plugin-page-account-tree.c
  gnc-plugin-page-budget.c
  gnc-plugin-page-invoice.c
  gnc-plugin-page-owner-tree.c
  gnc-plugin-page-sx-list.c
  gnc-plugin-page-register.c
  gnc-plugin-page-register2.c
  gnc-split-reg.c
  gnc-split-reg2.c
  reconcile-view.c
  search-owner.c
  top-level.c
  window-reconcile.c
  window-reconcile2.c
  window-autoclear.c
)  

SET_SOURCE_FILES_PROPERTIES (${gnc_gnome_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})
SET_SOURCE_FILES_PROPERTIES (business-options-gnome.c PROPERTIES OBJECT_DEPENDS ${SWIG_RUNTIME_H})

IF (WIN32)
  SET (gnc_gnome_SOURCES ${gnc_gnome_SOURCES} ${CMAKE_SOURCE_DIR}/borrowed/libc/strfmon.c)
ENDIF(WIN32)

ADD_LIBRARY (gnc-gnome ${gnc_gnome_SOURCES} ${gnc_gnome_noinst_HEADERS} ${SWIG_GNOME_C})
TARGET_LINK_LIBRARIES(gnc-gnome gncmod-gnome-search gncmod-ledger-core gncmod-report-gnome gncmod-report-system
    gncmod-register-gnome gncmod-register-core gncmod-gnome-utils gncmod-engine ${GTK3_LDFLAGS} ${GTK_MAC_LDFLAGS})

TARGET_COMPILE_DEFINITIONS (gnc-gnome PRIVATE -DG_LOG_DOMAIN=\"gnc.gui\" ${GTK_MAC_CFLAGS_OTHER})

TARGET_INCLUDE_DIRECTORIES(gnc-gnome
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE
    ${CMAKE_SOURCE_DIR}/libgnucash/app-utils/calculation
    ${CMAKE_BINARY_DIR}/gnucash/gnome-utils # for gnc-warnings.h
    ${GTK_MAC_INCLUDE_DIRS}
)


IF (MAC_INTEGRATION)
  TARGET_COMPILE_OPTIONS(gnc-gnome PRIVATE ${OSX_EXTRA_COMPILE_FLAGS})
  TARGET_LINK_LIBRARIES(gnc-gnome ${OSX_EXTRA_LIBRARIES})
ENDIF(MAC_INTEGRATION)


INSTALL(TARGETS gnc-gnome
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# No headers to install

# FIXME: where does LC_ALL=C come from?

SET(CMAKE_COMMAND_TMP "")
IF (${CMAKE_VERSION} VERSION_GREATER 3.1)
  SET(CMAKE_COMMAND_TMP ${CMAKE_COMMAND} -E env)
ENDIF()

ADD_CUSTOM_COMMAND(
    OUTPUT gnucash.appdata.xml
    COMMAND ${CMAKE_COMMAND_TMP}
      LC_ALL=C
      ${PERL_EXECUTABLE} ${INTLTOOL_MERGE} -x -u -c ${CMAKE_SOURCE_DIR}/po/.intltool-merge-cache ${CMAKE_SOURCE_DIR}/po
        ${CMAKE_CURRENT_SOURCE_DIR}/gnucash.appdata.xml.in gnucash.appdata.xml
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/gnucash.appdata.xml.in
)

ADD_CUSTOM_TARGET(gnucash-appdata ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gnucash.appdata.xml)

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash.appdata.xml DESTINATION  ${CMAKE_INSTALL_DATADIR}/appdata)

#=======

CONFIGURE_FILE(gnucash.desktop.in.in gnucash.desktop.in)


ADD_CUSTOM_COMMAND(
    OUTPUT gnucash.desktop
    COMMAND ${CMAKE_COMMAND_TMP}
      LC_ALL=C
      ${PERL_EXECUTABLE} ${INTLTOOL_MERGE} -d -u -c ${CMAKE_SOURCE_DIR}/po/.intltool-merge-cache ${CMAKE_SOURCE_DIR}/po
        gnucash.desktop.in gnucash.desktop
    MAIN_DEPENDENCY gnucash.desktop.in
)

ADD_CUSTOM_TARGET(gnucash-desktop ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gnucash.desktop)

INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash.desktop DESTINATION  ${CMAKE_INSTALL_DATADIR}/applications)

SET_LOCAL_DIST(gnome_DIST_local
        CMakeLists.txt gnome.i gnucash.appdata.xml.in gnucash.desktop.in.in
        ${gnc_gnome_noinst_HEADERS} ${gnc_gnome_SOURCES})
SET(gnome_DIST ${gnome_DIST_local} ${gnome_ui_DIST} ${gnome_gschema_DIST} ${gnome_gtkbuilder_DIST} PARENT_SCOPE)
