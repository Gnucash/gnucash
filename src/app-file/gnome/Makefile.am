lib_LTLIBRARIES = libgnc-app-file-gnome.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/engine \
  ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GLIB_CFLAGS} \
  ${GNOME_CFLAGS}

libgnc_app_file_gnome_la_SOURCES = \
  gnc-file-dialog.c \
  gnc-file-history.c 

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-file-history-gnome.h

libgnc_app_file_gnome_la_LIBADD = \
  ${GUILE_LIBS} \
  ${GNOME_LIBS} \
  ${GLIB_LIBS}
