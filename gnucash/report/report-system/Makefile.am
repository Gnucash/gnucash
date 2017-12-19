SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-report-system.la

libgncmod_report_system_la_SOURCES = \
  swig-report-system.c \
  gncmod-report-system.c \
  gnc-report.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  gnc-report.h

libgncmod_report_system_la_LDFLAGS = -avoid-version

libgncmod_report_system_la_LIBADD = \
  ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${GTK_LIBS}

if BUILDING_FROM_VCS
swig-report-system.c: report-system.i ${top_srcdir}/common/base-typemaps.i
	$(SWIG) -guile -Linkage module \
	-I${top_srcdir}/common -o $@ $<
endif

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/libgnucash/engine \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GUILE_CFLAGS}

gncscmdir = ${GNC_SCM_INSTALL_DIR}
gncscm_DATA = \
     commodity-utilities.scm  \
     html-acct-table.scm \
     html-barchart.scm \
     html-document.scm \
     html-fonts.scm \
     html-piechart.scm \
     html-scatter.scm \
     html-linechart.scm \
     html-style-info.scm \
     html-style-sheet.scm \
     html-table.scm \
     html-text.scm \
     html-utilities.scm \
     html-jqplot.scm \
     options-utilities.scm \
     report-utilities.scm \
     report.scm

gncmodscmdir = ${GNC_SCM_INSTALL_DIR}/gnucash/report/report-system
gncmodscm_DATA = \
     collectors.scm \
     list-extras.scm \
     report-collectors.scm

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash/report/
gncscmmod_DATA = \
    report-system.scm \
	eguile-gnc.scm \
	eguile-utilities.scm \
	eguile-html-utilities.scm

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
# for running
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA} \
  ${gncmodscm_DATA}
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
	mkdir -p  gnucash/report
	mkdir -p  gnucash/report/report-system
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/report; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
	( cd gnucash/report/report-system; for A in $(gncmodscm_DATA) ; do $(LN_S) -f ../../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

GUILE_COMPILE_ENV = \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/gnucash/html \
  --gnc-module-dir ${top_builddir}/gnucash/gnome-utils \
  --gnc-module-dir ${top_builddir}/gnucash/report/report-system \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/gnucash/gnome-utils \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --guile-load-dir ${top_builddir}/gnucash/report/report-system \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/gnucash/gnome-utils \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql \
  --library-dir    ${top_builddir}/gnucash/html \
  --library-dir    ${top_builddir}/gnucash/report/report-system

%.go : %.scm .scm-links $(pkglib_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/report
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)

gncmodscmcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash/report/report-system
gncmodscmcache_DATA = $(gncmodscm_DATA:.scm=.go)

gncscmcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@
gncscmcache_DATA = $(gncscm_DATA:.scm=.go)

noinst_DATA = .scm-links

EXTRA_DIST = \
	report-system.i \
	${gncscmmod_DATA} \
	${gncscm_DATA} \
	${gncmodscm_DATA} \
	CMakeLists.txt

clean-local:
	rm -rf gnucash

CLEANFILES = \
	report \
	.scm-links \
	${SCM_FILE_LINKS} \
	${gncscmmodcache_DATA} \
	${gncmodscmcache_DATA} \
	${gncscmcache_DATA}

MAINTAINERCLEANFILES = swig-report-system.c

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.report.core\"
