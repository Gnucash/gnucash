SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-report-gnome.la

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_builddir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/gnucash/gnome \
  -I${top_srcdir}/gnucash/html \
  -I${top_srcdir}/gnucash/report/report-system \
  ${GUILE_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GLIB_CFLAGS}

libgncmod_report_gnome_la_SOURCES = \
  swig-report-gnome.c \
  dialog-report-column-view.c \
  dialog-custom-report.c \
  dialog-report-style-sheet.c \
  gnc-plugin-page-report.c \
  gncmod-report-gnome.c \
  window-report.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  dialog-report-column-view.h \
  dialog-custom-report.h \
  dialog-report-style-sheet.h \
  gnc-plugin-page-report.h \
  window-report.h

libgncmod_report_gnome_la_LDFLAGS = -avoid-version

libgncmod_report_gnome_la_LIBADD = \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/gnucash/html/libgncmod-html.la \
  ${top_builddir}/gnucash/report/report-system/libgncmod-report-system.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS}

if BUILDING_FROM_VCS
swig-report-gnome.c: report-gnome.i ${top_srcdir}/common/base-typemaps.i
	$(SWIG) -guile -Linkage module \
	-I${top_srcdir}/common -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/common/swig-utf8.patch; \
	fi
endif
endif
endif

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash/report
gncscmmod_DATA = report-gnome.scm

noinst_DATA = .scm-links

gtkbuilderdir = $(GNC_GTKBUILDER_DIR)
gtkbuilder_DATA = \
  dialog-report.glade \
  dialog-custom-report.glade

uidir = $(GNC_UI_DIR)

ui_DATA = \
	gnc-plugin-page-report-ui.xml

EXTRA_DIST = \
  report-gnome.i \
  ${gtkbuilder_DATA} \
  ${gncscmmod_DATA} \
  ${ui_DATA} \
  CMakeLists.txt

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
# for running
SCM_FILE_LINKS = ${gncscmmod_DATA}
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
	( cd gnucash/report; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/gnucash/html \
  --gnc-module-dir ${top_builddir}/gnucash/report/report-system \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/gnucash/gnome-utils \
  --guile-load-dir ${top_builddir}/gnucash/report/report-system \
  --guile-load-dir ${top_builddir}/gnucash/report/utility-reports \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/gnucash/gnome-utils \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql \
  --library-dir    ${top_builddir}/gnucash/html \
  --library-dir    ${top_builddir}/gnucash/report/report-system \
  --library-dir    ${top_builddir}/gnucash/report/report-gnome

%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/report
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)
endif

clean-local:
	rm -rf gnucash

CLEANFILES = report .scm-links ${SCM_FILE_LINKS} ${gncscmmodcache_DATA}
MAINTAINERCLEANFILES = swig-report-gnome.c

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.report.gui\"
