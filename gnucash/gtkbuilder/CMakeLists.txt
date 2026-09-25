set (gtkbuilder_SOURCES
        assistant-acct-period.glade
        assistant-csv-account-import.glade
        assistant-csv-export.glade
        assistant-csv-price-import.glade
        assistant-csv-trans-import.glade
        assistant-hierarchy.glade
        assistant-loan.glade
        assistant-qif-import.glade
        assistant-stock-split.glade
        assistant-stock-transaction.glade
        assistant-xml-encoding.glade
        business-options-gnome.glade
        business-prefs.glade
        dialog-account-picker.glade
        dialog-account.glade
        dialog-doclink.ui
        dialog-bi-import-gui.glade
        dialog-billterms.glade
        dialog-book-close.ui
        dialog-choose-owner.glade
        dialog-commodities.ui
        dialog-commodity.ui
        dialog-custom-report.glade
        dialog-customer-import-gui.glade
        dialog-customer.glade
        dialog-date-close.glade
        dialog-employee.glade
        dialog-file-access.glade
        dialog-fincalc.ui
        dialog-find-account.glade
        dialog-imap-editor.ui
        dialog-import.glade
        dialog-invoice.glade
        dialog-job.glade
        dialog-lot-viewer.glade
        dialog-new-user.glade
        dialog-object-references.glade
        dialog-options.glade
        dialog-order.glade
        dialog-payment.glade
        dialog-preferences.glade
        dialog-price.ui
        dialog-print-check.glade
        dialog-progress.glade
        dialog-query-view.glade
        dialog-report.glade
        dialog-reset-warnings.glade
        dialog-search.glade
        dialog-sx.ui
        dialog-tax-info.glade
        dialog-tax-table.glade
        dialog-totd.glade
        dialog-transfer.glade
        dialog-userpass.glade
        dialog-vendor.glade
        gnc-date-format.glade
        gnc-frequency.ui
        gnc-plugin-page-budget.ui
        gnc-plugin-page-register.glade
        gnc-recurrence.ui
        gnc-tree-view-owner.glade
        window-autoclear.glade
        window-reconcile.glade
)

foreach (gtkbuilder_file ${gtkbuilder_SOURCES})
    configure_file (${gtkbuilder_file} ${DATADIR_BUILD}/gnucash/gtkbuilder/${gtkbuilder_file} COPYONLY)
endforeach()

find_program(GTK4_BUILDER_TOOL NAMES gtk4-builder-tool)
if (GTK4_BUILDER_TOOL)
    string(REPLACE ";" "|" GTKBUILDER_RESOURCES_ARGUMENT "${gtkbuilder_SOURCES}")
    add_test(NAME test-gtkbuilder-layout-parity
             COMMAND ${CMAKE_COMMAND}
                     -DGTK4_BUILDER_TOOL=${GTK4_BUILDER_TOOL}
                     -DGTKBUILDER_DIR=${CMAKE_CURRENT_SOURCE_DIR}
                     -DGNC_SOURCE_DIR=${CMAKE_SOURCE_DIR}/gnucash
                     -DGTKBUILDER_RESOURCES=${GTKBUILDER_RESOURCES_ARGUMENT}
                     -DAQB_ASSISTANT=${CMAKE_SOURCE_DIR}/gnucash/import-export/aqb/assistant-ab-initial.glade
                     -P ${CMAKE_CURRENT_SOURCE_DIR}/test-layout-parity.cmake)
else()
    message(STATUS "gtk4-builder-tool not found; skipping GTK builder layout parity test")
endif()

install (FILES ${gtkbuilder_SOURCES} DESTINATION share/gnucash/gtkbuilder)

set_dist_list(gtkbuilder_DIST CMakeLists.txt test-layout-parity.cmake ${gtkbuilder_SOURCES})
