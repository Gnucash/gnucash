SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-locale-reports-us.la

libgncmod_locale_reports_us_la_SOURCES = \
  gncmod-locale-reports-us.c 

libgncmod_locale_reports_us_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} 


AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} ${GLIB_CFLAGS}

## This is unused and therefore no longer installed and/or loaded
gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/locale-specific
gncscmmod_DATA = us.scm de_DE.scm

gncscmothermoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmothermod_DATA = taxtxf.scm taxtxf-de_DE.scm

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscmothermod_DATA}
endif

.scm-links:
	rm -f gnucash report locale-specific us 
	ln -sf . gnucash
	ln -sf . report 
	ln -sf ${srcdir} locale-specific 
	ln -sf . us 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  ln -sf ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncscmothermod_DATA} \
  ${gncscmmod_DATA} 

CLEANFILES = gnucash report locale-specific us .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}
