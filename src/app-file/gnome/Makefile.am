SUBDIRS = . #test

pkglib_LTLIBRARIES = libgncmod-app-file-gnome.la libgw-app-file-gnome.la

AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GLIB_CFLAGS} \
  ${GNOME_INCLUDEDIR}

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

libgw_app_file_gnome_la_SOURCES = gw-app-file-gnome.c
libgw_app_file_gnome_la_LDFLAGS = -module

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-app-file-gnome-spec.scm gw-app-file-gnome.scm

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gwmod_DATA}

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links

gw-app-file-gnome.scm gw-app-file-gnome.c gw-app-file-gnome.h: .scm-links gw-app-file-gnome-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_srcdir}/src/engine\" %load-path)) \
	  (primitive-load \"./gw-app-file-gnome-spec.scm\") \
	  (gw:generate-module \"gw-app-file-gnome\")"

BUILT_SOURCES = gw-app-file-gnome.scm gw-app-file-gnome.c gw-app-file-gnome.h
CLEANFILES = gw-app-file-gnome.scm gw-app-file-gnome.c gw-app-file-gnome.h \
             gw-app-file-gnome.html gnucash g-wrapped .scm-links
