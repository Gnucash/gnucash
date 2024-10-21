set(cglib_noinst_HEADERS cglib.h cglib-other.h)
set(cglib_SOURCES cglib.c color.c doughnut.c graph.c hbar.c pie.c vbar.c)

set_dist_list(cglib_DIST CMakeLists.txt
  README ${cglib_noinst_HEADERS} ${cglib_SOURCES})
