SUBDIRS = . test
DIST_SUBDIRS = test

pkglib_LTLIBRARIES = libgncmod-binary-import.la libgw-binary-import.la

AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/app-file \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnome \
  ${GUILE_INCS} \
  ${GLADE_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GLIB_CFLAGS} \
  ${GNOME_INCLUDEDIR}

#FIXME remove the following deps
#  -I${top_srcdir}/src \
#  -I${top_srcdir}/src/gnome \

libgncmod_binary_import_la_SOURCES = \
  druid-commodity.c \
  gncmod-binary-import.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  druid-commodity.h

libgncmod_binary_import_la_LDFLAGS = -module

libgncmod_binary_import_la_LIBADD = \
  ${top_srcdir}/src/gnc-module/libgncmodule.la \
  ${top_srcdir}/src/engine/libgncmod-engine.la \
  ${top_srcdir}/src/calculation/libgncmod-calculation.la \
  ${top_srcdir}/src/app-utils/libgncmod-app-utils.la \
  ${top_srcdir}/src/app-file/libgncmod-app-file.la \
  ${top_srcdir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${GUILE_LIBS} \
  ${GLADE_LIBS} \
  ${GNOMEUI_LIBS} \
  ${GNOME_LIBDIR} \
  ${GLIB_LIBS}

libgw_binary_import_la_SOURCES = gw-binary-import.c
libgw_binary_import_la_LDFLAGS = -module

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/import-export
gncmod_DATA = binary-import.scm 

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-binary-import-spec.scm gw-binary-import.scm

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

gw-binary-import.scm gw-binary-import.c gw-binary-import.h: .scm-links gw-binary-import-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (primitive-load \"./gw-binary-import-spec.scm\") \
	  (gw:generate-module \"gw-binary-import\")"

BUILT_SOURCES = gw-binary-import.scm gw-binary-import.c gw-binary-import.h
CLEANFILES = gw-binary-import.scm gw-binary-import.c gw-binary-import.h gw-binary-import.html \
              gnucash g-wrapped .scm-links
