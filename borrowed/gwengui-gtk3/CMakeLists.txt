set (gwengui_gtk3_noinst_HEADERS
  gtk3_gui.h
  gtk3_gui_p.h
  gtk3_gui_dialog_l.h
  gtk3_gui_dialog_p.h
  )

set (gwengui_gtk3_SOURCES
  gtk3_gui_dialog.c
  gtk3_gui.c
  )

set (gwengui_gtk3_EXTRA_DIST
  w_combobox.c
  w_label.c
  w_dialog.c
  w_gridlayout.c
  w_groupbox.c
  w_image.c
  w_hlayout.c
  w_hline.c
  w_hspacer.c
  w_lineedit.c
  w_vlayout.c
  w_vline.c
  w_vspacer.c
  w_pushbutton.c
  w_textbrowser.c
  w_textedit.c
  w_stack.c
  w_tabbook.c
  w_progressbar.c
  w_listbox.c
  w_checkbox.c
  w_scrollarea.c
  w_radiobutton.c
  w_spinbox.c
  )

set_dist_list(gwengui_gtk3_DIST CMakeLists.txt
  ${gwengui_gtk3_SOURCES}
  ${gwengui_gtk3_noinst_HEADERS}
  ${gwengui_gtk3_EXTRA_DIST}
  )

if (WITH_AQBANKING AND NOT HAVE_GWEN_GTK3)
    add_library(gwengui-gtk3 ${gwengui_gtk3_SOURCES})

    target_compile_definitions(gwengui-gtk3 PRIVATE BUILDING_GTK3_GUI=True)
    target_include_directories(gwengui-gtk3 PUBLIC
        ${GTK3_INCLUDE_DIRS}
        ${GWENHYWFAR_INCLUDE_DIRS}
    )
    target_link_libraries(gwengui-gtk3 ${GWENHYWFAR_LDFLAGS} ${GTK3_LDFLAGS})

    install(TARGETS gwengui-gtk3
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    )

endif()
