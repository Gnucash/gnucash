SUBDIRS = . #test

pkglib_LTLIBRARIES = libgncmod-app-file-gnome.la

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} ${G_WRAP_COMPILE_ARGS}

INCLUDES = -I${top_srcdir}/src/gnc-module \
           -I${top_srcdir}/src \
           -I${top_srcdir}/src/engine \
           -I${top_srcdir}/src/app-utils \
           -I${top_srcdir}/src/app-file \
           ${GNOME_INCLUDEDIR} \
           ${GUILE_INCS}

libgncmod_app_file_gnome_la_SOURCES = \
  gnc-file-dialog.c \
  gnc-file-history.c \
  gncmod-app-file-gnome.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-file-history-gnome.h

libgncmod_app_file_gnome_la_LDFLAGS = -module

libgncmod_app_file_gnome_la_LIBADD = \
  -L${top_srcdir}/src/gnc-module \
  -L${top_srcdir}/src/gnc-module/.libs -lgncmodule \
  -L${top_srcdir}/src/app-file \
  -L${top_srcdir}/src/app-file/.libs -lgncmod-app-file \
  ${GNOMEUI_LIBS} \
  ${GNOME_LIBDIR} \
  ${GLIB_LIBS}
