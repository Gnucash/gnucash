SUBDIRS = . test
DIST_SUBDIRS = test

pkglib_LTLIBRARIES = libgncmod-binary-import.la

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS}

#FIXME remove the following deps
#  -I${top_srcdir}/src \
#  -I${top_srcdir}/src/gnome \

INCLUDES = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnome \
  ${GNOME_INCLUDEDIR} \
  ${GUILE_INCS}

libgncmod_binary_import_la_SOURCES = \
  druid-commodity.c \
  gncmod-binary-import.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  druid-commodity.h

libgncmod_binary_import_la_LDFLAGS = -module

libgncmod_binary_import_la_LIBADD = \
  -L${top_srcdir}/src/gnc-module \
  -L${top_srcdir}/src/gnc-module/.libs \
  -lgncmodule \
  -L${top_srcdir}/src/engine \
  -L${top_srcdir}/src/engine/.libs \
  -lgncmod-engine \
  -L${top_srcdir}/src/calculation \
  -L${top_srcdir}/src/calculation/.libs \
  -lgncmod-calculation \
  -L${top_srcdir}/src/app-utils \
  -L${top_srcdir}/src/app-utils/.libs \
  -lgncmod-app-utils \
  -L${top_srcdir}/src/app-file \
  -L${top_srcdir}/src/app-file/.libs \
  -lgncmod-app-file \
  -L${top_srcdir}/src/gnome-utils \
  -L${top_srcdir}/src/gnome-utils/.libs \
  -lgncmod-gnome-utils \
  ${GNOMEUI_LIBS} \
  ${GNOME_LIBDIR} \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

libgw_binary_import_la_SOURCES = gw-binary-import.c
libgw_binary_import_la_LDFLAGS = -module

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/import-export
gncmod_DATA = binary-import.scm 

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_LTLIBRARIES = libgw-binary-import.la
gwmod_DATA = gw-binary-import-spec.scm

noinst_DATA = .scm-links

gladedir = $(GNC_GLADE_DIR)
glade_DATA = binary-import.glade

EXTRA_DIST = \
  .cvsignore \
  ${gncmod_DATA} \
  ${gwmod_DATA} \
  ${glade_DATA}

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links

gw-binary-import.c gw-binary-import.h: .scm-links gw-binary-import-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (primitive-load \"./gw-binary-import-spec.scm\") \
	  (gw:generate-module \"gw-binary-import\")"

BUILT_SOURCES = gw-binary-import.c gw-binary-import.h
CLEANFILES = gw-binary-import.c gw-binary-import.h gw-binary-import.html \
              gnucash g-wrapped .scm-links
