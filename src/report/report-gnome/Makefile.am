SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-report-gnome.la

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnome \
  -I${top_srcdir}/src/html \
  -I${top_srcdir}/src/report/report-system \
  ${QOF_CFLAGS} \
  ${GLADE_CFLAGS} \
  ${GUILE_INCS} \
  ${GTKHTML_CFLAGS} \
  ${GNOME_CFLAGS} \
  ${GLIB_CFLAGS}

libgncmod_report_gnome_la_SOURCES = \
  swig-report-gnome.c \
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

libgncmod_report_gnome_la_LDFLAGS = -avoid-version

libgncmod_report_gnome_la_LIBADD = \
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/html/libgncmod-html.la \
  ${top_builddir}/src/report/report-system/libgncmod-report-system.la \
  ${GLADE_LIBS} \
  ${GUILE_LIBS} \
  ${GNOME_LIBS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

if BUILDING_FROM_SVN
swig-report-gnome.c: report-gnome.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report
gncmod_DATA = report-gnome.scm

noinst_DATA = .scm-links

gladedir = $(GNC_GLADE_DIR)
glade_DATA = report.glade

uidir = $(GNC_UI_DIR)

ui_DATA = \
	gnc-plugin-page-report-ui.xml

EXTRA_DIST = \
  report-gnome.i \
  ${glade_DATA} \
  ${gncmod_DATA} \
  ${ui_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
# for running
SCM_FILE_LINKS = ${gncmod_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash
	mkdir -p  gnucash/report
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/report; for A in $(gncmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
	touch .scm-links

CLEANFILES = gnucash report .scm-links ${SCM_FILE_LINKS}
MAINTAINERCLEANFILES = swig-report-gnome.c

INCLUDES = -DG_LOG_DOMAIN=\"gnc.report.gui\"
