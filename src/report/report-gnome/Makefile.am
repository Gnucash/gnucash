SUBDIRS = .
# test -- removed 'test' from SUBDIRS by jsled 2004.05.02 .. I don't know how
# to get the test-link-mumble to link correctly; plz assist.
PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgncmod-report-gnome.la libgw-report-gnome.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/report/report-system \
  -I${top_srcdir}/lib/glib26 \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${GTKHTML_CFLAGS} \
  ${GNOME_PRINT_CFLAGS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GNOME_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS}

libgncmod_report_gnome_la_SOURCES = \
  dialog-column-view.c \
  dialog-style-sheet.c \
  gnc-plugin-page-report.c \
  gncmod-report-gnome.c \
  window-report.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  dialog-column-view.h \
  dialog-style-sheet.h \
  gnc-plugin-page-report.h \
  window-report.h

libgncmod_report_gnome_la_LIBADD = \
  ${top_builddir}/src/core-utils/libcore-utils.la \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/report/report-system/libgncmod-report-system.la \
  ${top_builddir}/lib/glib26/libgncglib.la \
  ${GLADE_LIBS} \
  ${G_WRAP_LINK_ARGS} \
  ${GUILE_LIBS} \
  ${GNOME_PRINT_LIBS} \
  ${GNOME_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

libgw_report_gnome_la_SOURCES = gw-report-gnome.c
libgw_report_gnome_la_LIBADD = \
  libgncmod-report-gnome.la \
  ${G_WRAP_LINK_ARGS}

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = gw-report-gnome.scm gw-report-gnome-spec.scm

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report
gncmod_DATA = report-gnome.scm

noinst_DATA = .scm-links

gladedir = $(GNC_GLADE_DIR)
glade_DATA = report.glade

uidir = $(GNC_UI_DIR)

ui_DATA = \
	gnc-plugin-page-report-ui.xml

noinst_HEADERS = \
 gw-report-gnome.h

EXTRA_DIST = \
  ${glade_DATA} \
  ${gwmod_DATA} \
  ${gncmod_DATA} \
  ${ui_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
#  for compiling
SCM_FILE_LINKS = gw-report-gnome-spec.scm
# for running
SCM_FILE_LINKS += ${gncmod_DATA}
endif

.scm-links: 
	rm -f gnucash g-wrapped
	$(LN_S) -f . gnucash 
	$(LN_S) -f . g-wrapped 
	$(LN_S) -f ${srcdir} report 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

gw-report-gnome.scm gw-report-gnome.c gw-report-gnome.h: \
  .scm-links gw-report-gnome-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	  (set! %load-path (cons \"${PWD}\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/engine\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/app-utils\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/gnome-utils\" %load-path)) \
	  (set! %load-path (cons \"${top_builddir}/src/gnome\" %load-path)) \
	  (primitive-load \"./gw-report-gnome-spec.scm\") \
	  (gw:generate-wrapset \"gw-report-gnome\")"

BUILT_SOURCES = gw-report-gnome.scm gw-report-gnome.c gw-report-gnome.h
CLEANFILES = gnucash g-wrapped report .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS} gw-report-gnome.html
