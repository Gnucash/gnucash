SUBDIRS = . gnome

pkglib_LTLIBRARIES = libgncmod-app-file.la libgw-app-file.la

CFLAGS = \
  @CFLAGS@ \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS}

libgncmod_app_file_la_SOURCES = \
  gnc-file.c \
  gncmod-app-file.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-file.h \
  gnc-file-dialog.h \
  gnc-file-history.h

noinst_HEADERS = gnc-file-p.h

libgncmod_app_file_la_LDFLAGS = -module

libgncmod_app_file_la_LIBADD = \
  -L../gnc-module -L../gnc-module/.libs -lgncmodule \
  -L../engine -L../engine/.libs -lgncmod-engine \
  -L../calculation -L../calculation/.libs -lgncmod-calculation \
  -L../app-utils -L../app-utils/.libs -lgncmod-app-utils \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

libgw_app_file_la_SOURCES = gw-app-file.c
libgw_app_file_la_LDFLAGS = -module

#gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
#gncmod_DATA = app-file.scm 

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-app-file-spec.scm gw-app-file.scm

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gwmod_DATA}

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links

gw-app-file.scm gw-app-file.c gw-app-file.h: .scm-links gw-app-file-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_srcdir}/src/engine\" %load-path)) \
	  (primitive-load \"./gw-app-file-spec.scm\") \
	  (gw:generate-module \"gw-app-file\")"

BUILT_SOURCES = gw-app-file.scm gw-app-file.c gw-app-file.h
CLEANFILES = gw-app-file.scm gw-app-file.c gw-app-file.h gw-app-file.html \
              gnucash g-wrapped .scm-links
