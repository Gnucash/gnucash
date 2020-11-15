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
        assistant-xml-encoding.glade
        business-options-gnome.glade
        business-prefs.glade
        dialog-account-picker.glade
        dialog-account.glade
        dialog-doclink.glade
        dialog-bi-import-gui.glade
        dialog-billterms.glade
        dialog-book-close.glade
        dialog-choose-owner.glade
        dialog-commodities.glade
        dialog-commodity.glade
        dialog-custom-report.glade
        dialog-customer-import-gui.glade
        dialog-customer.glade
        dialog-date-close.glade
        dialog-employee.glade
        dialog-file-access.glade
        dialog-fincalc.glade
        dialog-find-account.glade
        dialog-imap-editor.glade
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
        dialog-price.glade
        dialog-print-check.glade
        dialog-progress.glade
        dialog-query-view.glade
        dialog-report.glade
        dialog-reset-warnings.glade
        dialog-search.glade
        dialog-stock-editor.glade
        dialog-sx.glade
        dialog-tax-info.glade
        dialog-tax-table.glade
        dialog-totd.glade
        dialog-transfer.glade
        dialog-userpass.glade
        dialog-vendor.glade
        gnc-date-format.glade
        gnc-frequency.glade
        gnc-plugin-page-budget.glade
        gnc-plugin-page-register2.glade
        gnc-plugin-page-register.glade
        gnc-recurrence.glade
        gnc-tree-view-owner.glade
        window-autoclear.glade
        window-reconcile.glade
)

foreach (gtkbuilder_file ${gtkbuilder_SOURCES})
    configure_file (${gtkbuilder_file} ${DATADIR_BUILD}/gnucash/gtkbuilder/${gtkbuilder_file} COPYONLY)
endforeach()

install (FILES ${gtkbuilder_SOURCES} DESTINATION share/gnucash/gtkbuilder)

set_dist_list(gtkbuilder_DIST CMakeLists.txt ${gtkbuilder_SOURCES})
