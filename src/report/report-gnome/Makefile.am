pkglib_LTLIBRARIES = libgncmod-report-gnome.la libgw-report-gnome.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/app-file \
  ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GLADE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GNOME_INCLUDEDIR} \
  ${GNOME_PRINT_CFLAGS}

libgncmod_report_gnome_la_SOURCES = \
  dialog-column-view.c \
  dialog-style-sheet.c \
  gncmod-report-gnome.c \
  window-report.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  dialog-column-view.h \
  dialog-style-sheet.h \
  window-report.h

libgncmod_report_gnome_la_LDFLAGS = -module

libgncmod_report_gnome_la_LIBADD = \
  ${top_srcdir}/src/gnc-module/libgncmodule.la \
  ${GLADE_LIBS} \
  ${GUILE_LIBS} \
  ${GNOME_LIBDIR} \
  ${GNOMEUI_LIBS} \
  ${GNOME_PRINT_LIBS} \
  ${GLIB_LIBS}

libgw_report_gnome_la_SOURCES = gw-report-gnome.c
libgw_report_gnome_la_LDFLAGS = -module

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-report-gnome.scm gw-report-gnome-spec.scm

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report
gncmod_DATA = report-gnome.scm

noinst_DATA = .scm-links

gladedir = $(GNC_GLADE_DIR)
glade_DATA = report.glade

EXTRA_DIST = \
  .cvsignore \
  ${glade_DATA} \
  ${gwmod_DATA} \
  ${gncmod_DATA}

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	ln -sf . report 
	touch .scm-links

gw-report-gnome.scm gw-report-gnome.c gw-report-gnome.h: .scm-links gw-report-gnome-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_srcdir}/src/engine\" %load-path)) \
	  (set! %load-path (cons \"${top_srcdir}/src/gnome-utils\" %load-path)) \
	  (primitive-load \"./gw-report-gnome-spec.scm\") \
	  (gw:generate-module \"gw-report-gnome\")"

BUILT_SOURCES = gw-report-gnome.scm gw-report-gnome.c gw-report-gnome.h
CLEANFILES = gw-report-gnome.scm gw-report-gnome.c gw-report-gnome.h \
             gw-report-gnome.html gnucash g-wrapped report .scm-links
