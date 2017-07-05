FILE(COPY .
    DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder
    PATTERN Makefile.* EXCLUDE
    PATTERN CMakeLists.txt EXCLUDE)

INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DESTINATION share/gnucash
        PATTERN Makefile.* EXCLUDE
        PATTERN CMakeLists.txt EXCLUDE)

SET(gnome_GLADE
        assistant-acct-period.glade
        assistant-hierarchy.glade
        assistant-loan.glade
        assistant-stock-split.glade
        dialog-commodities.glade
        dialog-fincalc.glade
        dialog-lot-viewer.glade
        dialog-new-user.glade
        dialog-price.glade
        dialog-print-check.glade
        dialog-progress.glade
        dialog-sx.glade
        dialog-tax-info.glade
        gnc-plugin-page-budget.glade
        gnc-plugin-page-register2.glade
        gnc-plugin-page-register.glade
        window-autoclear.glade
        window-reconcile.glade
)

SET_DIST_LIST(gnome_gtkbuilder_DIST CMakeLists.txt Makefile.am ${gnome_GLADE})