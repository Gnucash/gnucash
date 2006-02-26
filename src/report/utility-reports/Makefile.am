SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-utility-reports.la

libgncmod_utility_reports_la_SOURCES = \
  gncmod-utility-reports.c 

libgncmod_utility_reports_la_LDFLAGS = -module

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} ${GLIB_CFLAGS}

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmmod_DATA = \
  hello-world.scm \
  utility-reports.scm \
  view-column.scm \
  welcome-to-gnucash.scm \
  test-graphing.scm

if GNUCASH_SEPARATE_BUILDDIR
#For separate build directory
SCM_FILE_LINKS = \
  ${gncscmmod_DATA}
endif

.scm-links:
	rm -f gnucash report
	ln -sf . gnucash
	ln -sf ${srcdir} report 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  ln -sf ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

noinst_DATA = .scm-links

EXTRA_DIST = ${gncscmmod_DATA}

CLEANFILES = gnucash report .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}
