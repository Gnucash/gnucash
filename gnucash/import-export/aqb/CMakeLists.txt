# CMakeLists.txt for gnucash/import-export/aqbanking

add_subdirectory(gschemas)
add_subdirectory(test)

set (aqbanking_SOURCES
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
set_source_files_properties (${aqbanking_SOURCES} PROPERTIES OBJECT_DEPENDS ${CONFIG_H})

set (aqbanking_noinst_HEADERS
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

set(aqbanking_GLADE assistant-ab-initial.glade dialog-ab.glade dialog-ab-pref.glade)

set(aqbanking_UI gnc-plugin-aqbanking-ui.xml)

if(WITH_AQBANKING)
  add_library (gncmod-aqbanking
    ${aqbanking_SOURCES}
    ${aqbanking_noinst_HEADERS}
  )

  set(AQB_EXPORT_SYMBOLS "")
  if (WIN32)
    set(AQB_EXPORT_SYMBOLS "-Wl,--export-all-symbols")
  endif()
  set(AQB_LIBSTDCXX "")
  if (MINGW)
    set(AQB_LIBSTDCXX "-lstdc++")
  endif()

  target_link_libraries(gncmod-aqbanking gnc-gnome gncmod-gnome-utils
         gncmod-generic-import gncmod-register-core
         gncmod-register-gnome gncmod-ledger-core gncmod-engine gwengui-gtk3
         ${AQB_EXPORT_SYMBOLS} ${AQBANKING_LDFLAGS}
         ${GWENHYWFAR_LDFLAGS} 
         ${GNOME_LDFLAGS} ${AQB_LIBSTDCXX})

  target_compile_definitions(gncmod-aqbanking PRIVATE -DG_LOG_DOMAIN=\"gnc.import.aqbanking\")
  target_compile_options(gncmod-aqbanking PRIVATE -Wno-deprecated-declarations)

  target_include_directories(gncmod-aqbanking PRIVATE
    ${CMAKE_SOURCE_DIR}/borrowed/gwengui-gtk3
    ${AQBANKING_INCLUDE_DIRS}
    ${GWENHYWFAR_INCLUDE_DIRS})

if (APPLE)
  set_target_properties (gncmod-aqbanking PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

  install(TARGETS gncmod-aqbanking
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# No headers to install


  install(FILES ${aqbanking_GLADE} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/gtkbuilder)

  install(FILES ${aqbanking_UI} DESTINATION  ${CMAKE_INSTALL_DATADIR}/gnucash/ui)

  file(COPY ${aqbanking_UI}
    DESTINATION ${DATADIR_BUILD}/gnucash/ui)
  file(COPY ${aqbanking_GLADE}
    DESTINATION ${DATADIR_BUILD}/gnucash/gtkbuilder)
endif()

set_local_dist(aqbanking_DIST_local CMakeLists.txt migratable-prefs-aqbanking.xml
        ${aqbanking_SOURCES} ${aqbanking_noinst_HEADERS} ${aqbanking_EXTRA_DIST}
        ${aqbanking_GLADE} ${aqbanking_UI})

set(aqbanking_DIST ${aqbanking_DIST_local} ${aqbanking_gschema_DIST} ${test_aqb_DIST} PARENT_SCOPE)
