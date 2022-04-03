set(gnc_action_icons
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
)

set(gnc_app_icons
    hicolor/16x16/apps/gnucash-icon.png
    hicolor/22x22/apps/gnucash-icon.png
    hicolor/24x24/apps/gnucash-icon.png
    hicolor/32x32/apps/gnucash-icon.png
    hicolor/48x48/apps/gnucash-icon.png
    hicolor/64x64/apps/gnucash-icon.png
    hicolor/96x96/apps/gnucash-icon.png
    hicolor/128x128/apps/gnucash-icon.png
    hicolor/256x256/apps/gnucash-icon.png
    hicolor/scalable/apps/gnucash-icon.svg
)

set(gnc_other_pixmaps
gnucash-icon.ico
gnucash-icon-48x48.bmp
gnucash_splash.png
)

install(FILES ${gnc_other_pixmaps} DESTINATION ${CMAKE_INSTALL_DATADIR}/gnucash/pixmaps)
file(COPY ${gnc_other_pixmaps} DESTINATION ${DATADIR_BUILD}/gnucash/pixmaps)

set(dest_base_dir "gnucash/icons")

function (copy_iconpaths_to_dest iconpaths dest_base_dir)
    foreach(iconpath ${iconpaths})
        get_filename_component(dest_rel_dir ${iconpath} DIRECTORY)
        set(dest_dir "${dest_base_dir}/${dest_rel_dir}")
        install(FILES ${iconpath} DESTINATION "${CMAKE_INSTALL_DATADIR}/${dest_dir}")
        file(COPY ${iconpath} DESTINATION "${DATADIR_BUILD}/${dest_dir}")
    endforeach()
endfunction()

copy_iconpaths_to_dest ("${gnc_action_icons}" "gnucash/icons")
copy_iconpaths_to_dest ("${gnc_app_icons}" "icons")

#install(FILES ${gnc_app_icons} DESTINATION ${CMAKE_INSTALL_DATADIR}/icons)
#file(COPY ${gnc_app_icons} DESTINATION ${DATADIR_BUILD}/icons)

set_local_dist(pixmaps_DIST_local CMakeLists.txt ${gnc_action_icons}
    ${gnc_other_pixmaps} ${gnc_app_icons})
set(pixmaps_DIST ${pixmaps_DIST_local} PARENT_SCOPE)
