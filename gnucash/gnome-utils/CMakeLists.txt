# Note that gnucash/gnome-utils CANNOT depend on gnucash/gnome!

add_subdirectory(test)

# Command to generate the swig-gnome-utils.c wrapper file
gnc_add_swig_guile_command (swig-gnome-utils-c
    SWIG_GNOME_UTILS_C swig-gnome-utils.c
    ${CMAKE_CURRENT_SOURCE_DIR}/gnome-utils.i
)

set (WARNINGS_SCHEMA ${DATADIR_BUILD}/glib-2.0/schemas/org.gnucash.warnings.gschema.xml)
set (GNC_WARNINGS_C ${CMAKE_CURRENT_BINARY_DIR}/gnc-warnings.c)
set (GNC_WARNINGS_H ${CMAKE_CURRENT_BINARY_DIR}/gnc-warnings.h)

add_custom_command(
  OUTPUT ${GNC_WARNINGS_C}
  DEPENDS ${WARNINGS_SCHEMA} make-gnc-warnings-c.xsl
  COMMAND
    ${LIBXSLT_XSLTPROC_EXECUTABLE} -o ${GNC_WARNINGS_C} ${CMAKE_CURRENT_SOURCE_DIR}/make-gnc-warnings-c.xsl ${WARNINGS_SCHEMA}
)
add_custom_command(
  OUTPUT ${GNC_WARNINGS_H}
  DEPENDS ${WARNINGS_SCHEMA} make-gnc-warnings-h.xsl
  COMMAND
    ${LIBXSLT_XSLTPROC_EXECUTABLE} -o ${GNC_WARNINGS_H} ${CMAKE_CURRENT_SOURCE_DIR}/make-gnc-warnings-h.xsl ${WARNINGS_SCHEMA}
)

#GTK before 3.14 didn't have GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK
if (NOT have_mod_mask)
if (MAC_INTEGRATION)
  add_definitions(-DGDK_MODIFIER_INTENT_DEFAULT_MOD_MASK=GDK_SHIFT_MASK|GDK_CONTROL_MASK|GDK_MOD1_MASK|GDK_MOD2_MASK|GDK_SUPER_MASK|GDK_HYPER_MASK|GDK_META_MASK)
else()
  add_definitions(-DGDK_MODIFIER_INTENT_DEFAULT_MOD_MASK=GDK_SHIFT_MASK|GDK_CONTROL_MASK|GDK_MOD1_MASK|GDK_SUPER_MASK|GDK_HYPER_MASK|GDK_META_MASK)
endif()
endif()

set (gnome_utils_SOURCES
  account-quickfill.c
  assistant-xml-encoding.c
  cursors.c
  dialog-account.c
  dialog-book-close.c
  dialog-commodity.c
  dialog-dup-trans.c
  dialog-file-access.c
  dialog-object-references.c
  dialog-options.c
  dialog-preferences.c
  dialog-query-view.c
  dialog-reset-warnings.c
  dialog-tax-table.c
  dialog-totd.c
  dialog-transfer.c
  dialog-userpass.c
  dialog-utils.c
  gnc-account-sel.c
  gnc-amount-edit.c
  gnc-autosave.c
  gnc-cell-renderer-date.c
  gnc-cell-renderer-popup.c
  gnc-cell-renderer-popup-entry.c
  gnc-combott.c
  gnc-commodity-edit.c
  gnc-currency-edit.c
  gnc-date-delta.c
  gnc-date-edit.c
  gnc-date-format.c 
  gnc-dense-cal.c
  gnc-dense-cal-model.c
  gnc-dense-cal-store.c
  gnc-embedded-window.c
  gnc-file.c
  gnc-frequency.c
  gnc-recurrence.c
  gnc-general-select.c
  gnc-gnome-utils.c
  gnc-gobject-utils.c
  gnc-gtk-utils.c
  gnc-gui-query.c
  gnc-icons.c
  gnc-keyring.c
  gnc-main-window.c
  gnc-menu-extensions.c
  gnc-plugin-file-history.c
  gnc-plugin-manager.c
  gnc-plugin-menu-additions.c
  gnc-plugin-page.c
  gnc-plugin.c
  gnc-period-select.c
  gnc-query-view.c
  gnc-splash.c
  gnc-sx-instance-dense-cal-adapter.c
  gnc-sx-list-tree-model-adapter.c
  gnc-tree-control-split-reg.c
  gnc-tree-model.c
  gnc-tree-model-account-types.c
  gnc-tree-model-account.c
  gnc-tree-model-budget.c
  gnc-tree-model-owner.c
  gnc-tree-model-commodity.c
  gnc-tree-model-price.c
  gnc-tree-model-split-reg.c
  gnc-tree-util-split-reg.c
  gnc-tree-view-account.c
  gnc-tree-view-commodity.c
  gnc-tree-view-owner.c
  gnc-tree-view-price.c
  gnc-tree-view-split-reg.c
  gnc-tree-view-sx-list.c
  gnc-tree-view.c
  gnc-window.c
  gncmod-gnome-utils.c
  misc-gnome-utils.c
  tree-view-utils.c
  search-param.c
  print-session.c
  window-main-summarybar.c
)

set(gnome_utils_noinst_HEADERS
  dialog-tax-table.h
  gnc-autosave.h
  gnc-gobject-utils.h
  gnc-gtk-utils.h
  search-param.h
)

set (gnome_utils_HEADERS
  account-quickfill.h
  assistant-xml-encoding.h
  dialog-account.h
  dialog-book-close.h
  dialog-commodity.h
  dialog-dup-trans.h
  dialog-file-access.h
  dialog-preferences.h
  dialog-object-references.h
  dialog-options.h
  dialog-query-view.h
  dialog-reset-warnings.h
  dialog-totd.h
  dialog-transfer.h
  dialog-utils.h
  gnc-account-sel.h
  gnc-amount-edit.h
  gnc-cell-renderer-date.h
  gnc-cell-renderer-popup.h
  gnc-cell-renderer-popup-entry.h
  gnc-combott.h
  gnc-commodity-edit.h
  gnc-currency-edit.h
  gnc-date-delta.h
  gnc-date-edit.h
  gnc-date-format.h
  gnc-dense-cal.h
  gnc-dense-cal-model.h
  gnc-dense-cal-store.h
  gnc-embedded-window.h
  gnc-file.h
  gnc-frequency.h
  gnc-recurrence.h
  gnc-general-select.h
  gnc-gnome-utils.h
  gnc-gui-query.h
  gnc-icons.h
  gnc-keyring.h
  gnc-main-window.h
  gnc-menu-extensions.h
  gnc-plugin-file-history.h
  gnc-plugin-manager.h
  gnc-plugin-menu-additions.h
  gnc-plugin-page.h
  gnc-plugin.h
  gnc-period-select.h
  gnc-query-view.h
  gnc-splash.h
  gnc-sx-instance-dense-cal-adapter.h
  gnc-sx-list-tree-model-adapter.h
  gnc-tree-control-split-reg.h
  gnc-tree-model.h
  gnc-tree-model-account-types.h
  gnc-tree-model-account.h
  gnc-tree-model-budget.h
  gnc-tree-model-owner.h
  gnc-tree-model-commodity.h
  gnc-tree-model-price.h
  gnc-tree-model-split-reg.h
  gnc-tree-util-split-reg.h
  gnc-tree-view-account.h
  gnc-tree-view-commodity.h
  gnc-tree-view-owner.h
  gnc-tree-view-price.h
  gnc-tree-view-split-reg.h
  gnc-tree-view-sx-list.h
  gnc-tree-view.h
  gnc-ui.h
  gnc-window.h
  misc-gnome-utils.h
  tree-view-utils.h
  print-session.h
  window-main-summarybar.h
)



add_library (gncmod-gnome-utils
  ${gnome_utils_SOURCES}
  ${gnome_utils_HEADERS}
  ${GNC_WARNINGS_C} ${GNC_WARNINGS_H}
  ${SWIG_GNOME_UTILS_C}
  ${gnome_utils_noinst_HEADERS}
)

target_link_libraries(gncmod-gnome-utils gncmod-app-utils gncmod-engine gnc-backend-xml-utils ${GTK3_LDFLAGS}
     ${CMAKE_DL_LIBS} ${LIBSECRET_LDFLAGS} ${GTK_MAC_LDFLAGS})

target_compile_options(gncmod-gnome-utils PRIVATE -Wno-deprecated-declarations)
target_compile_definitions(gncmod-gnome-utils PUBLIC ${GTK_MAC_CFLAGS_OTHER}
  PRIVATE -DG_LOG_DOMAIN=\"gnc.gui\")

if (BUILDING_FROM_VCS)
  target_compile_definitions(gncmod-gnome-utils PRIVATE -DGNC_VCS=\"git\")
endif()

if (MAC_INTEGRATION)
  target_compile_options(gncmod-gnome-utils PRIVATE ${OSX_EXTRA_COMPILE_FLAGS})
  target_link_libraries(gncmod-gnome-utils ${OSX_EXTRA_LIBRARIES})
endif()

target_include_directories(gncmod-gnome-utils
    PUBLIC
      ${GTK3_INCLUDE_DIRS}
      ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE
      ${GTK_MAC_INCLUDE_DIRS}
      ${LIBSECRET_INCLUDE_DIRS}
      ${CMAKE_CURRENT_BINARY_DIR}
)


if (APPLE)
  set_target_properties (gncmod-gnome-utils PROPERTIES INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/gnucash")
endif()

install(TARGETS gncmod-gnome-utils
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/gnucash
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(FILES ${gnome_utils_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gnucash)


# Scheme

set(GUILE_DEPENDS      gncmod-gnome-utils scm-gnc-module scm-scm scm-app-utils)

gnc_add_scheme_targets(scm-gnome-utils-1
  gnome-utils.scm
  gnucash
  "${GUILE_DEPENDS}"
  FALSE
)


gnc_add_scheme_targets(scm-gnome-utils-2
  gnc-menu-extensions.scm
  ""
  "${GUILE_DEPENDS}"
  FALSE
)

add_custom_target(scm-gnome-utils ALL DEPENDS scm-gnome-utils-2 scm-gnome-utils-1)

set_local_dist(gnome_utils_DIST_local CMakeLists.txt ${gnome_utils_SOURCES} ${gnome_utils_HEADERS}
        ${gnome_utils_noinst_HEADERS} gnome-utils.scm gnome-utils.i gnc-menu-extensions.scm
        make-gnc-warnings-c.xsl make-gnc-warnings-h.xsl)
set(gnome_utils_DIST ${gnome_utils_DIST_local} ${test_gnome_utils_DIST} PARENT_SCOPE)
