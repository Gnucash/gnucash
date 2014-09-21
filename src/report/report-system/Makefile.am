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
  ${top_builddir}/src/core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${GTK_LIBS}

if BUILDING_FROM_SCM
swig-report-system.c: report-system.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/src/swig-utf8.patch; \
	fi
endif
endif
endif

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/libqof/qof \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GUILE_CFLAGS}

gncscmdir = ${GNC_SHAREDIR}/scm
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

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report-system

gncmodscmdir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/report-system

gncmodscm_DATA = \
     collectors.scm \
     list-extras.scm \
     report-collectors.scm


gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
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

noinst_DATA = .scm-links

EXTRA_DIST = report-system.i ${gncscmmod_DATA} ${gncscm_DATA} ${gncmodscm_DATA}

clean-local:
	rm -rf gnucash

CLEANFILES = report .scm-links ${SCM_FILE_LINKS}
MAINTAINERCLEANFILES = swig-report-system.c

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.report.core\"
