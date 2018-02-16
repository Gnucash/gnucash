SET(gnome_GLADE
        assistant-acct-period.glade
        assistant-hierarchy.glade
        assistant-loan.glade
        assistant-stock-split.glade
        business-options-gnome.glade
        business-prefs.glade
        dialog-billterms.glade
        dialog-choose-owner.glade
        dialog-commodities.glade
        dialog-customer.glade
        dialog-date-close.glade
        dialog-employee.glade
        dialog-fincalc.glade
        dialog-find-account.glade
        dialog-imap-editor.glade
        dialog-invoice.glade
        dialog-job.glade
        dialog-lot-viewer.glade
        dialog-new-user.glade
        dialog-order.glade
        dialog-payment.glade
        dialog-price.glade
        dialog-print-check.glade
        dialog-progress.glade
        dialog-sx.glade
        dialog-tax-info.glade
        dialog-trans-assoc.glade
        dialog-vendor.glade
        gnc-plugin-page-budget.glade
        gnc-plugin-page-register2.glade
        gnc-plugin-page-register.glade
        window-autoclear.glade
        window-reconcile.glade
)

FILE(COPY ${gnome_GLADE} DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)

INSTALL(FILES ${gnome_GLADE} DESTINATION share/gnucash/gtkbuilder)

SET_DIST_LIST(gnome_gtkbuilder_DIST CMakeLists.txt ${gnome_GLADE})
