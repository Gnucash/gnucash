
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} DESTINATION share/gnucash
        PATTERN Makefile.* EXCLUDE
        PATTERN CMakeLists.txt EXCLUDE
        PATTERN hicolor EXCLUDE
)

INSTALL(DIRECTORY ${DATADIR_BUILD}/gnucash/icons DESTINATION share/gnucash
        PATTERN Makefile.* EXCLUDE
        PATTERN CMakeLists.txt EXCLUDE
)

IF (GNC_BUILD_AS_INSTALL)
  FILE(COPY .
     DESTINATION ${DATADIR_BUILD}/gnucash/pixmaps
     PATTERN Makefile.* EXCLUDE
     PATTERN CMakeLists.txt EXCLUDE
     PATTERN hicolor EXCLUDE
  )
ENDIF()

IF (GNC_BUILD_AS_INSTALL)
  FILE(COPY hicolor
     DESTINATION ${DATADIR_BUILD}/gnucash/icons
  )
ENDIF()

FOREACH(dir 256x256 128x128 96x96 64x64 48x48)
  INSTALL(
      FILES ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-icon-${dir}.png
      RENAME gnucash-icon.png
      DESTINATION share/icons/hicolor/${dir}/apps)
  IF(GNC_BUILD_AS_INSTALL)    
    FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/gnucash-icon-${dir}.png
         DESTINATION ${DATADIR_BUILD}/icons/hicolor/${dir}/apps/gnucash-icon-${dir}.png)
  ENDIF()
ENDFOREACH(dir)

INSTALL(
    FILES ${CMAKE_SOURCE_DIR}/art/tango/32x32/gnucash.png
    RENAME gnucash-icon.png
    DESTINATION share/icons/hicolor/32x32/apps
)

INSTALL(
    FILES ${CMAKE_SOURCE_DIR}/art/tango/22x22/gnucash-22x22.png
    RENAME gnucash-icon.png
    DESTINATION share/icons/hicolor/22x22/apps
)
INSTALL(
    FILES ${CMAKE_SOURCE_DIR}/art/tango/22x22/gnucash-24x24.png
    RENAME gnucash-icon.png
    DESTINATION share/icons/hicolor/24x24/apps
)
INSTALL(
    FILES ${CMAKE_SOURCE_DIR}/art/tango/16x16/gnucash.png
    RENAME gnucash-icon.png
    DESTINATION share/icons/hicolor/16x16/apps
)
INSTALL(
    FILES ${CMAKE_SOURCE_DIR}/art/tango/scalable/gnucash.svg
    RENAME gnucash-icon.svg
    DESTINATION share/icons/hicolor/scalable/apps
)
INSTALL(
    FILES ${CMAKE_SOURCE_DIR}/art/tango/16x16/gnucash.png
    RENAME gnucash-icon-16x16.png
    DESTINATION share/gnucash/pixmaps
)
INSTALL(
    FILES ${CMAKE_SOURCE_DIR}/art/tango/32x32/gnucash.png
    RENAME gnucash-icon-32x32.png
    DESTINATION share/gnucash/pixmaps
)

IF (GNC_BUILD_AS_INSTALL)
  FILE(
    COPY ${CMAKE_SOURCE_DIR}/art/tango/32x32/gnucash.png
    DESTINATION ${DATADIR_BUILD}/icons/hicolor/32x32/apps
  )
  FILE(RENAME ${DATADIR_BUILD}/icons/hicolor/32x32/apps/gnucash.png
    ${DATADIR_BUILD}/icons/hicolor/32x32/apps/gnucash-icon.png
    )

  FILE(
    COPY ${CMAKE_SOURCE_DIR}/art/tango/22x22/gnucash-22x22.png
    DESTINATION ${DATADIR_BUILD}/icons/hicolor/22x22/apps
  )
  FILE(RENAME ${DATADIR_BUILD}/icons/hicolor/22x22/apps/gnucash-22x22.png
    ${DATADIR_BUILD}/icons/hicolor/22x22/apps/gnucash-icon.png
    )
  FILE(
    COPY ${CMAKE_SOURCE_DIR}/art/tango/22x22/gnucash-24x24.png
    DESTINATION ${DATADIR_BUILD}/icons/hicolor/24x24/apps
  )
  FILE(RENAME ${DATADIR_BUILD}/icons/hicolor/24x24/apps/gnucash-24x24.png
    ${DATADIR_BUILD}/icons/hicolor/24x24/apps/gnucash-icon.png
  )

  FILE(
    COPY ${CMAKE_SOURCE_DIR}/art/tango/16x16/gnucash.png
    DESTINATION ${DATADIR_BUILD}/icons/hicolor/16x16/apps
  )
  FILE(RENAME ${DATADIR_BUILD}/icons/hicolor/16x16/apps/gnucash.png
    ${DATADIR_BUILD}/icons/hicolor/16x16/apps/gnucash-icon.png
  )

  FILE(
    COPY ${CMAKE_SOURCE_DIR}/art/tango/scalable/gnucash.svg
    DESTINATION ${DATADIR_BUILD}/icons/hicolor/scalable/apps
  )
  FILE(RENAME ${DATADIR_BUILD}/icons/hicolor/scalable/apps/gnucash.svg
    ${DATADIR_BUILD}/icons/hicolor/scalable/apps/gnucash-icon.svg
  )

  FILE(
    COPY ${CMAKE_SOURCE_DIR}/art/tango/16x16/gnucash.png
    DESTINATION ${DATADIR_BUILD}/gnucash/pixmaps
  )
  FILE(RENAME ${DATADIR_BUILD}/gnucash/pixmaps/gnucash.png
              ${DATADIR_BUILD}/gnucash/pixmaps/gnucash-icon-16x16.png
  )

  FILE(
    COPY ${CMAKE_SOURCE_DIR}/art/tango/32x32/gnucash.png
    DESTINATION ${DATADIR_BUILD}/gnucash/pixmaps
  )
  FILE(RENAME ${DATADIR_BUILD}/gnucash/pixmaps/gnucash.png
    ${DATADIR_BUILD}/gnucash/pixmaps/gnucash-icon-32x32.png
  )

ENDIF()
