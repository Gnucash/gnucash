
pkglib_LTLIBRARIES = libgncmod-gnome-search.la libgw-gnome-search.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/business/business-core \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${GTKHTML_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GLIB_CFLAGS} 

libgncmod_gnome_search_la_SOURCES = \
  gncmod-gnome-search.c \
  dialog-search.c \
  search-boolean.c \
  search-core-type.c \
  search-date.c \
  search-debcred.c \
  search-double.c \
  search-int64.c \
  search-numeric.c \
  search-param.c \
  search-string.c

noinst_HEADERS = \
  search-boolean.h \
  search-core-type.h \
  search-date.h \
  search-debcred.h \
  search-double.h \
  search-int64.h \
  search-numeric.h \
  search-param.h \
  search-string.h

libgncmod_gnome_search_la_LDFLAGS = -module

libgncmod_gnome_search_la_LIBADD = \
  ${top_srcdir}/src/gnc-module/libgncmodule.la \
  ${GUILE_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOMEUI_LIBS} \
  ${GTKHTML_LIBS} \
  ${GLADE_LIBS} \
  ${GLIB_LIBS}

libgw_gnome_search_la_SOURCES = gw-gnome-search.c
libgw_gnome_search_la_LDFLAGS = -module ${G_WRAP_LINK_ARGS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = gnome-search.scm

gladedir = $(GNC_GLADE_DIR)
glade_DATA = \
  search.glade

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = \
  gw-gnome-search.scm \
  gw-gnome-search-spec.scm

EXTRA_DIST = \
  .cvsignore \
  ${glade_DATA} \
  ${gncmod_DATA} \
  ${gwmod_DATA}

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links

gncmod-gnome-search.c: gw-gnome-search.h

gw-gnome-search.scm gw-gnome-search.c gw-gnome-search.h: .scm-links gw-gnome-search-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (set! %load-path (cons \"../engine\" %load-path)) \
	 (set! %load-path (cons \"../app-utils\" %load-path)) \
	 (set! %load-path (cons \"../gnome-utils\" %load-path)) \
	 (primitive-load \"./gw-gnome-search-spec.scm\") \
	 (gw:generate-wrapset \"gw-gnome-search\")"

BUILT_SOURCES = gw-gnome-search.scm gw-gnome-search.c gw-gnome-search.h
CLEANFILES = gw-gnome-search.scm gw-gnome-search.c gw-gnome-search.h gw-gnome-search.html
