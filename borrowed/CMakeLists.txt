ADD_SUBDIRECTORY(libc)
ADD_SUBDIRECTORY(goffice)
ADD_SUBDIRECTORY(gwengui-gtk3)

SET_LOCAL_DIST(borrowed_DIST_local CMakeLists.txt README)
SET(borrowed_DIST ${borrowed_DIST_local} ${libc_DIST} ${goffice_DIST} ${gwengui_gtk3_DIST} PARENT_SCOPE)
