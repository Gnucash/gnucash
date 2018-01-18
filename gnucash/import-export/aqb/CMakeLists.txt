# CMakeLists.txt for gnucash/import-export/aqbanking

ADD_SUBDIRECTORY(gschemas)
ADD_SUBDIRECTORY(test)

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

SET(aqbanking_GLADE assistant-ab-initial.glade dialog-ab.glade dialog-ab-pref.glade)

SET(aqbanking_UI gnc-plugin-aqbanking-ui.xml)

IF(WITH_AQBANKING)
  ADD_LIBRARY (gncmod-aqbanking
    ${aqbanking_SOURCES}
    ${aqbanking_noinst_HEADERS}
  )

  SET(AQB_EXPORT_SYMBOLS "")
  IF (WIN32)
    SET(AQB_EXPORT_SYMBOLS "-Wl,--export-all-symbols")
  ENDIF()
  SET(AQB_LIBSTDCXX "")
  IF (MINGW)
    SET(AQB_LIBSTDCXX "-lstdc++")
  ENDIF()

  TARGET_LINK_LIBRARIES(gncmod-aqbanking gnc-gnome gncmod-gnome-utils
         gncmod-generic-import gncmod-register-core
         gncmod-register-gnome gncmod-ledger-core gncmod-engine gwengui-gtk3
         ${AQB_EXPORT_SYMBOLS} ${AQBANKING_LDFLAGS}
         ${GWENHYWFAR_LDFLAGS} 
         ${GNOME_LDFLAGS} ${KTOBLZCHECK_LDFLAGS} ${AQB_LIBSTDCXX})

  TARGET_COMPILE_DEFINITIONS(gncmod-aqbanking PRIVATE -DG_LOG_DOMAIN=\"gnc.import.aqbanking\")

  TARGET_INCLUDE_DIRECTORIES(gncmod-aqbanking PRIVATE
    ${CMAKE_SOURCE_DIR}/borrowed/gwengui-gtk3
    ${AQBANKING_INCLUDE_DIRS}
    ${GWENHYWFAR_INCLUDE_DIRS}
    ${KTOBLZCHECK_INCLUDE_DIRS})

IF (APPLE)
  SET_TARGET_PROPERTIES (gncmod-aqbanking PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
ENDIF()

  INSTALL(TARGETS gncmod-aqbanking
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# No headers to install


  INSTALL(FILES ${aqbanking_GLADE} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/gtkbuilder)

  INSTALL(FILES ${aqbanking_UI} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/ui)

  FILE(COPY ${aqbanking_UI}
    DESTINATION ${DATADIR_BUILD}/gnucash/ui)
  FILE(COPY ${aqbanking_GLADE}
    DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)
ENDIF(WITH_AQBANKING)

SET_LOCAL_DIST(aqbanking_DIST_local CMakeLists.txt migratable-prefs-aqbanking.xml
        ${aqbanking_SOURCES} ${aqbanking_noinst_HEADERS} ${aqbanking_EXTRA_DIST}
        ${aqbanking_GLADE} ${aqbanking_UI})

SET(aqbanking_DIST ${aqbanking_DIST_local} ${aqbanking_gschema_DIST} ${test_aqb_DIST} PARENT_SCOPE)
