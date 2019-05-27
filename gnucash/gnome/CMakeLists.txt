
set (gnc_gnome_noinst_HEADERS
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

# Command to generate the swig-gnome.c wrapper file
gnc_add_swig_guile_command (swig-gnome-c
    SWIG_GNOME_C swig-gnome.c
    ${CMAKE_CURRENT_SOURCE_DIR}/gnome.i ${gnc_gnome_HEADERS}
)

set (gnc_gnome_SOURCES
  assistant-acct-period.c
  assistant-hierarchy.c
  assistant-loan.cpp
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

set_source_files_properties (${gnc_gnome_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

add_library (gnc-gnome ${gnc_gnome_SOURCES} ${gnc_gnome_noinst_HEADERS} ${SWIG_GNOME_C})
target_link_libraries(gnc-gnome gncmod-gnome-search gncmod-ledger-core gncmod-report-gnome gncmod-report-system
    gncmod-register-gnome gncmod-register-core gncmod-gnome-utils gncmod-engine ${GTK3_LDFLAGS} ${GTK_MAC_LDFLAGS})

target_compile_definitions (gnc-gnome PRIVATE -DG_LOG_DOMAIN=\"gnc.gui\" ${GTK_MAC_CFLAGS_OTHER})
target_compile_options(gnc-gnome PRIVATE -Wno-deprecated-declarations)

target_include_directories(gnc-gnome
  PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE
    ${CMAKE_SOURCE_DIR}/libgnucash/app-utils/calculation
    ${CMAKE_BINARY_DIR}/gnucash/gnome-utils # for gnc-warnings.h
    ${GTK_MAC_INCLUDE_DIRS}
)

add_dependencies (gnc-gnome swig-runtime-h)

if (MAC_INTEGRATION)
  target_compile_options(gnc-gnome PRIVATE ${OSX_EXTRA_COMPILE_FLAGS})
  target_link_libraries(gnc-gnome ${OSX_EXTRA_LIBRARIES})
endif(MAC_INTEGRATION)


install(TARGETS gnc-gnome
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# No headers to install

# FIXME: where does LC_ALL=C come from?

if (${GETTEXT_VERSION_STRING} VERSION_LESS 0.19.6)
    # Gettext is too old to be able to merge an appdata file.
    # Fall back to providing an unmerged (and hence untranslated) appdata file.
    configure_file (${CMAKE_CURRENT_SOURCE_DIR}/gnucash.appdata.xml.in
                    ${CMAKE_CURRENT_BINARY_DIR}/gnucash.appdata.xml COPYONLY)
else()
    add_custom_command (
        OUTPUT gnucash.appdata.xml
        COMMAND ${CMAKE_COMMAND} -E env
            LC_ALL=C
            ${GETTEXT_MSGFMT_EXECUTABLE}
                --xml --template ${CMAKE_CURRENT_SOURCE_DIR}/gnucash.appdata.xml.in
                -d ${CMAKE_SOURCE_DIR}/po
                -o gnucash.appdata.xml
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/gnucash.appdata.xml.in
    )
endif()

add_custom_target(gnucash-appdata ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gnucash.appdata.xml)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash.appdata.xml DESTINATION  ${CMAKE_INSTALL_DATADIR}/metainfo)

#=======

configure_file(gnucash.desktop.in.in gnucash.desktop.in)

if (${GETTEXT_VERSION_STRING} VERSION_LESS 0.19)
    # Gettext is too old to be able to merge a desktop file.
    # Fall back to providing an unmerged (and hence untranslated) desktop file.
    configure_file (${CMAKE_CURRENT_BINARY_DIR}/gnucash.desktop.in
                    ${CMAKE_CURRENT_BINARY_DIR}/gnucash.desktop COPYONLY)
else()
    add_custom_command (
        OUTPUT gnucash.desktop
        COMMAND ${CMAKE_COMMAND} -E env
            LC_ALL=C
            ${GETTEXT_MSGFMT_EXECUTABLE}
                --desktop --template gnucash.desktop.in
                -d ${CMAKE_SOURCE_DIR}/po
                -o gnucash.desktop
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gnucash.desktop.in
    )
endif()

add_custom_target(gnucash-desktop ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gnucash.desktop)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash.desktop DESTINATION  ${CMAKE_INSTALL_DATADIR}/applications)

set_dist_list(gnome_DIST
        CMakeLists.txt gnome.i gnucash.appdata.xml.in gnucash.desktop.in.in
        ${gnc_gnome_noinst_HEADERS} ${gnc_gnome_SOURCES})
