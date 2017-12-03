
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} DESTINATION ${CMAKE_INSTALL_DATADIR}/gnucash
        PATTERN Makefile.* EXCLUDE
        PATTERN CMakeLists.txt EXCLUDE
        PATTERN hicolor EXCLUDE
)
FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}
    DESTINATION ${DATADIR_BUILD}/gnucash
    PATTERN Makefile.* EXCLUDE
    PATTERN CMakeLists.txt EXCLUDE
    PATTERN hicolor EXCLUDE
)
INSTALL(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/hicolor DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/icons
    REGEX ".*/apps.*" EXCLUDE
)
FILE(
    COPY ${CMAKE_CURRENT_SOURCE_DIR}/hicolor
    DESTINATION ${DATADIR_BUILD}/gnucash/icons
    REGEX ".*/apps.*" EXCLUDE
)

INSTALL(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/hicolor DESTINATION  ${CMAKE_INSTALL_DATADIR}/icons
    REGEX ".*/actions.*" EXCLUDE
)
FILE(
    COPY ${CMAKE_CURRENT_SOURCE_DIR}/hicolor
    DESTINATION ${DATADIR_BUILD}/icons
    REGEX ".*/actions.*" EXCLUDE
)

SET(gncpixmap_DATA
    hicolor/16x16/actions/gnc-account.png
    hicolor/24x24/actions/gnc-account.png
    hicolor/16x16/actions/gnc-account-delete.png
    hicolor/24x24/actions/gnc-account-delete.png
    hicolor/16x16/actions/gnc-account-edit.png
    hicolor/24x24/actions/gnc-account-edit.png
    hicolor/16x16/actions/gnc-account-new.png
    hicolor/24x24/actions/gnc-account-new.png
    hicolor/16x16/actions/gnc-account-open.png
    hicolor/24x24/actions/gnc-account-open.png
    hicolor/16x16/actions/gnc-account-report.png
    hicolor/24x24/actions/gnc-account-report.png
    hicolor/16x16/actions/gnc-gnome-pdf.png
    hicolor/24x24/actions/gnc-gnome-pdf.png
    hicolor/16x16/actions/gnc-invoice.png
    hicolor/24x24/actions/gnc-invoice.png
    hicolor/16x16/actions/gnc-invoice-duplicate.png
    hicolor/24x24/actions/gnc-invoice-duplicate.png
    hicolor/16x16/actions/gnc-invoice-edit.png
    hicolor/24x24/actions/gnc-invoice-edit.png
    hicolor/16x16/actions/gnc-invoice-new.png
    hicolor/24x24/actions/gnc-invoice-new.png
    hicolor/16x16/actions/gnc-invoice-pay.png
    hicolor/24x24/actions/gnc-invoice-pay.png
    hicolor/16x16/actions/gnc-invoice-post.png
    hicolor/24x24/actions/gnc-invoice-post.png
    hicolor/16x16/actions/gnc-invoice-unpost.png
    hicolor/24x24/actions/gnc-invoice-unpost.png
    hicolor/16x16/actions/gnc-jumpto.png
    hicolor/24x24/actions/gnc-jumpto.png
    hicolor/16x16/actions/gnc-split-trans.png
    hicolor/24x24/actions/gnc-split-trans.png
    hicolor/16x16/actions/gnc-sx-new.png
    hicolor/24x24/actions/gnc-sx-new.png
    hicolor/16x16/actions/gnc-transfer.png
    hicolor/24x24/actions/gnc-transfer.png
    gnucash-icon.ico
    gnucash-icon-48x48.bmp
    gnucash_splash.png
)

SET(gncicon_DATA
    hicolor/16x16/apps/gnucash-icon.png
    hicolor/22x22/apps/gnucash-icon.png
    hicolor/24x24/apps/gnucash-icon.png
    hicolor/32x32/apps/gnucash-icon.png
    hicolor/48x48/apps/gnucash-icon.png
    hicolor/64x64/apps/gnucash-icon.png
    hicolor/96x96/apps/gnucash-icon.png
    hicolor/128x128/apps/gnucash-icon.png
    hicolor/256x256/apps/gnucash-icon.png
)

SET(gncscalableicon_DATA
    hicolor/scalable/apps/gnucash-icon.svg
)

SET_LOCAL_DIST(pixmaps_DIST_local CMakeLists.txt ${gncpixmap_DATA}
    ${gncicon_DATA} ${gncscalableicon_DATA})
SET(pixmaps_DIST ${pixmaps_DIST_local} PARENT_SCOPE)
