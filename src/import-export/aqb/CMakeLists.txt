# CMakeLists.txt for src/import-export/aqbanking

ADD_SUBDIRECTORY(gschemas)


SET (aqbanking_SOURCES
  dialog-ab-trans.c
  dialog-ab-daterange.c
  assistant-ab-initial.c
  gnc-ab-getbalance.c
  gnc-ab-gettrans.c
  gnc-ab-kvp.c
  gnc-ab-transfer.c
  gnc-ab-utils.c
  gnc-file-aqb-import.c
  gnc-gwen-gui.c
  gnc-plugin-aqbanking.c
  gncmod-aqbanking.c
)

# Add dependency on config.h
SET_SOURCE_FILES_PROPERTIES (${aqbanking_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

SET (aqbanking_noinst_HEADERS
  dialog-ab-trans.h
  dialog-ab-daterange.h
  assistant-ab-initial.h
  gnc-ab-getbalance.h
  gnc-ab-gettrans.h
  gnc-ab-kvp.h
  gnc-ab-transfer.h
  gnc-ab-utils.h
  gnc-file-aqb-import.h
  gnc-gwen-gui.h
  gnc-plugin-aqbanking.h
)

ADD_LIBRARY (gncmod-aqbanking
  ${aqbanking_SOURCES}
  ${aqbanking_noinst_HEADERS}
  )

SET(AQB_EXPORT_SYMBOLS "")
IF (WIN32)
  SET(AQB_EXPORT_SYMBOLS "-Wl,--export-all-symbols")
ENDIF()

TARGET_LINK_LIBRARIES(gncmod-aqbanking gnc-gnome gncmod-gnome-utils gncmod-generic-import gncmod-register-core
         gncmod-register-gnome gncmod-ledger-core gncmod-engine gnc-qof ${AQB_EXPORT_SYMBOLS} ${AQBANKING_LDFLAGS} ${GWENHYWFAR_LDFLAGS}
         ${GWENGUI_GTK2_LDFLAGS} ${GNOME_LDFLAGS} ${KTOBLZCHECK_LDFLAGS})

TARGET_COMPILE_DEFINITIONS(gncmod-aqbanking PRIVATE -DG_LOG_DOMAIN=\"gnc.import.aqbanking\")

TARGET_INCLUDE_DIRECTORIES(gncmod-aqbanking PRIVATE ${AQBANKING_INCLUDE_DIRS}
       ${GWENHYWFAR_INCLUDE_DIRS} ${KTOBLZCHECK_INCLUDE_DIRS})

INSTALL(TARGETS gncmod-aqbanking
  LIBRARY DESTINATION lib/gnucash
  ARCHIVE DESTINATION lib/gnucash
  RUNTIME DESTINATION bin)
# No headers to install

SET(aqbanking_GLADE assistant-ab-initial.glade dialog-ab.glade)

SET(aqbanking_UI gnc-plugin-aqbanking-ui.xml)

INSTALL(FILES ${aqbanking_GLADE} DESTINATION share/gnucash/gtkbuilder)

INSTALL(FILES ${aqbanking_UI} DESTINATION share/gnucash/ui)

IF (GNC_BUILD_AS_INSTALL)
  FILE(COPY ${aqbanking_UI}
    DESTINATION ${DATADIR_BUILD}/gnucash/ui)
  FILE(COPY ${aqbanking_GLADE}
    DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)
ENDIF()