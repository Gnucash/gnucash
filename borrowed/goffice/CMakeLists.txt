set(goffice_noinst_HEADERS go-optionmenu.h go-charmap-sel.h go-glib-extras.h)
set(goffice_SOURCES go-optionmenu.c go-charmap-sel.c go-glib-extras.c)

#add_library(gnc-goffice ${goffice_noinst_HEADERS} ${goffice_SOURCES})

#target_link_libraries(gnc-goffice ${GTK_LIBS} ${LIBXML2_LIBS})
#target_include_directories(gnc-goffice PUBLIC ${GTK_CFLAGS} ${LIBXML2_CFLAGS})

#install(TARGETS gnc-goffice
#  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
#  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
#  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

set_dist_list(goffice_DIST CMakeLists.txt
  README ${goffice_noinst_HEADERS} ${goffice_SOURCES})
