SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-locale-reports-us.la

libgncmod_locale_reports_us_la_SOURCES = \
  gncmod-locale-reports-us.c 

libgncmod_locale_reports_us_la_LDFLAGS = -module
libgncmod_locale_reports_us_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgncmodule.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} 


AM_CFLAGS = -I${top_srcdir}/src/gnc-module ${GUILE_INCS} ${GLIB_CFLAGS}

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = taxtxf.scm
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

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/locale-specific
gncscmmod_DATA = us.scm 

gncscmothermoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/report/
gncscmothermod_DATA = taxtxf.scm 

EXTRA_DIST = \
  ${gncscmmod_DATA} \
  ${gncscmothermod_DATA}

CLEANFILES = ${SCM_FILE_LINKS} gnucash report locale-specific us .scm-links
