SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-utility-reports.la

libgncmod_utility_reports_la_SOURCES = \
  gncmod-utility-reports.c 

libgncmod_utility_reports_la_LDFLAGS = -module

INCLUDES = -I${top_srcdir}/src/gnc-module ${GLIB_CFLAGS}

.scm-links:
	rm -f gnucash report
	ln -sf . gnucash
	ln -sf . report 
	touch .scm-links

noinst_DATA = .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA = \
  hello-world.scm \
  iframe-url.scm \
  utility-reports.scm \
  view-column.scm \
  welcome-to-gnucash.scm 

EXTRA_DIST = ${gncscmmod_DATA}

CLEANFILES = gnucash report .scm-links
