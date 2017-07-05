
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} DESTINATION share/gnucash
        PATTERN Makefile.* EXCLUDE
        PATTERN CMakeLists.txt EXCLUDE
)


FILE(COPY .
   DESTINATION ${DATADIR_BUILD}/gnucash/pixmaps
   PATTERN Makefile.* EXCLUDE
   PATTERN CMakeLists.txt EXCLUDE
)


FOREACH(dir 256x256 128x128 96x96 64x64 48x48)
  INSTALL(
      FILES ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-icon-${dir}.png
      RENAME gnucash-icon.png
      DESTINATION share/icons/hicolor/${dir}/apps)

  FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-icon-${dir}.png
       DESTINATION ${DATADIR_BUILD}/icons/hicolor/${dir}/apps/gnucash-icon-${dir}.png)

  IF (BUILDING_FROM_VCS)
    FILE(MAKE_DIRECTORY ${dir})
    FILE(COPY gnucash-icon-${dir}.png DESTINATION ${dir})
    FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/${dir}/gnucash-icon-${dir}.png
                ${CMAKE_CURRENT_BINARY_DIR}/${dir}/gnucash-icon.png)
  ENDIF()
ENDFOREACH(dir)

IF (BUILDING_FROM_VCS)
  FILE(MAKE_DIRECTORY 16x16 22x22 24x24 32x32 scalable)

   # 16x16
  FILE(COPY ${CMAKE_SOURCE_DIR}/art/tango/16x16/gnucash.png DESTINATION 16x16)
  FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/16x16/gnucash.png
              ${CMAKE_CURRENT_BINARY_DIR}/16x16/gnucash-icon.png)

  FILE(COPY ${CMAKE_SOURCE_DIR}/art/tango/16x16/gnucash.png DESTINATION .)
  FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/gnucash.png
              ${CMAKE_CURRENT_BINARY_DIR}/gnucash-icon-16x16.png)
	    
	    
  # 22x22	    
  FILE(COPY ${CMAKE_SOURCE_DIR}/art/tango/22x22/gnucash-22x22.png DESTINATION 22x22)
  FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/22x22/gnucash-22x22.png
              ${CMAKE_CURRENT_BINARY_DIR}/22x22/gnucash-icon.png)

  # 24x24	    
  FILE(COPY ${CMAKE_SOURCE_DIR}/art/tango/22x22/gnucash-24x24.png DESTINATION 24x24)
  FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/24x24/gnucash-24x24.png
              ${CMAKE_CURRENT_BINARY_DIR}/24x24/gnucash-icon.png)

  # 32x32
  FILE(COPY ${CMAKE_SOURCE_DIR}/art/tango/32x32/gnucash.png DESTINATION 32x32)
  FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/32x32/gnucash.png
              ${CMAKE_CURRENT_BINARY_DIR}/32x32/gnucash-icon.png)
  
  FILE(COPY ${CMAKE_SOURCE_DIR}/art/tango/32x32/gnucash.png DESTINATION .)
  FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/gnucash.png
              ${CMAKE_CURRENT_BINARY_DIR}/gnucash-icon-32x32.png)


  # scalable
  FILE(COPY ${CMAKE_SOURCE_DIR}/art/tango/scalable/gnucash.svg DESTINATION scalable)
  FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/scalable/gnucash.svg
              ${CMAKE_CURRENT_BINARY_DIR}/scalable/gnucash-icon.svg)
  
ELSE()
  FILE(MAKE_DIRECTORY 16x16 22x22 24x24 32x32 scalable)
  
  FILE(COPY 16x16/gnucash-icon.png DESTINATION 16x16)

  FILE(COPY 22x22/gnucash-icon.png DESTINATION 22x22)
  FILE(COPY 24x24/gnucash-icon.png DESTINATION 24x24)
  FILE(COPY 32x32/gnucash-icon.png DESTINATION 32x32)
  FILE(COPY 32x32/gnucash-icon.png DESTINATION .)
  FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/gnucash-icon.png ${CMAKE_CURRENT_BINARY_DIR}/gnucash-icon-32x32.png)
  FILE(COPY 16x16/gnucash-icon.png DESTINATION .)
  FILE(RENAME ${CMAKE_CURRENT_BINARY_DIR}/gnucash-icon.png ${CMAKE_CURRENT_BINARY_DIR}/gnucash-icon-16x16.png)
  FILE(COPY scalable/gnucash-icon.svg DESTINATION scalable)
ENDIF()


#-------

FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/32x32/gnucash-icon.png
     DESTINATION ${DATADIR_BUILD}/icons/hicolor/32x32/apps)

FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/22x22/gnucash-icon.png
     DESTINATION ${DATADIR_BUILD}/icons/hicolor/22x22/apps)
   
FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/24x24/gnucash-icon.png
     DESTINATION ${DATADIR_BUILD}/icons/hicolor/24x24/apps)

FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/16x16/gnucash-icon.png
     DESTINATION ${DATADIR_BUILD}/icons/hicolor/16x16/apps)

FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/scalable/gnucash-icon.svg
     DESTINATION ${DATADIR_BUILD}/icons/hicolor/scalable/apps)
  
FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/16x16/gnucash-icon.png
     DESTINATION ${DATADIR_BUILD}/gnucash/pixmaps)

FILE(RENAME ${DATADIR_BUILD}/gnucash/pixmaps/gnucash-icon.png
        ${DATADIR_BUILD}/gnucash/pixmaps/gnucash-icon-16x16.png)

FILE(COPY ${CMAKE_CURRENT_BINARY_DIR}/32x32/gnucash-icon.png
     DESTINATION ${DATADIR_BUILD}/gnucash/pixmaps)

FILE(RENAME ${DATADIR_BUILD}/gnucash/pixmaps/gnucash-icon.png
${DATADIR_BUILD}/gnucash/pixmaps/gnucash-icon-32x32.png)


#------------------------
INSTALL(
        FILES ${CMAKE_CURRENT_BINARY_DIR}/32x32/gnucash-icon.png
        DESTINATION share/icons/hicolor/32x32/apps
)

INSTALL(
        FILES ${CMAKE_CURRENT_BINARY_DIR}/22x22/gnucash-icon.png
        DESTINATION share/icons/hicolor/22x22/apps
)
INSTALL(
        FILES ${CMAKE_CURRENT_BINARY_DIR}/24x24/gnucash-icon.png
        DESTINATION share/icons/hicolor/24x24/apps
)
INSTALL(
        FILES ${CMAKE_CURRENT_BINARY_DIR}/16x16/gnucash-icon.png
        DESTINATION share/icons/hicolor/16x16/apps
)
INSTALL(
        FILES ${CMAKE_CURRENT_BINARY_DIR}/scalable/gnucash-icon.svg
        DESTINATION share/icons/hicolor/scalable/apps
)
INSTALL(
        FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash-icon-16x16.png
        DESTINATION share/gnucash/pixmaps
)
INSTALL(
        FILES ${CMAKE_CURRENT_BINARY_DIR}/gnucash-icon-32x32.png
        DESTINATION share/gnucash/pixmaps
)

SET(gncpixmap_DATA
        gnc-account-16.png
        gnc-account-delete-16.png
        gnc-account-delete.png
        gnc-account-edit-16.png
        gnc-account-edit.png
        gnc-account-new-16.png
        gnc-account-new.png
        gnc-account-open-16.png
        gnc-account-open.png
        gnc-account.png
        gnc-account-report-16.png
        gnc-account-report.png
        gnc-gnome-pdf-16.png
        gnc-gnome-pdf-24.png
        gnc-invoice-16.png
        gnc-invoice-duplicate-16.png
        gnc-invoice-duplicate.png
        gnc-invoice-edit-16.png
        gnc-invoice-edit.png
        gnc-invoice-new-16.png
        gnc-invoice-new.png
        gnc-invoice-pay-16.png
        gnc-invoice-pay.png
        gnc-invoice.png
        gnc-invoice-post-16.png
        gnc-invoice-post.png
        gnc-invoice-unpost-16.png
        gnc-invoice-unpost.png
        gnc-jumpto-16.png
        gnc-jumpto.png
        gnc-split-trans-16.png
        gnc-split-trans.png
        gnc-sx-new-16.png
        gnc-sx-new.png
        gnc-transfer-16.png
        gnc-transfer.png
        gnucash-icon-128x128.png
        gnucash-icon-256x256.png
        gnucash-icon-48x48.png
        gnucash-icon-48x48.bmp
        gnucash-icon-64x64.png
        gnucash-icon-96x96.png
        gnucash-icon.ico
        gnucash_splash.png
        stock_split_title.png
        stock_split_watermark.png
)

SET_LOCAL_DIST(pixmaps_DIST_local CMakeLists.txt Makefile.am ${gncpixmap_DATA})
SET(pixmaps_DIST ${pixmaps_DIST_local} PARENT_SCOPE)

