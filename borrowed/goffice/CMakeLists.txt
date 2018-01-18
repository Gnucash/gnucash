set(goffice_noinst_HEADERS go-optionmenu.h go-charmap-sel.h go-glib-extras.h)
set(goffice_SOURCES go-optionmenu.c go-charmap-sel.c go-glib-extras.c)

#ADD_LIBRARY(gnc-goffice ${goffice_noinst_HEADERS} ${goffice_SOURCES})

#TARGET_LINK_LIBRARIES(gnc-goffice ${GTK_LIBS} ${LIBXML2_LIBS})
#TARGET_INCLUDE_DIRECTORIES(gnc-goffice PUBLIC ${GTK_CFLAGS} ${LIBXML2_CFLAGS})

#INSTALL(TARGETS gnc-goffice
#  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
#  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
#  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

SET_DIST_LIST(goffice_DIST CMakeLists.txt 
  README ${goffice_noinst_HEADERS} ${goffice_SOURCES})
