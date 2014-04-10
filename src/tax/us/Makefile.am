SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-tax-us.la

libgncmod_tax_us_la_SOURCES = \
  gncmod-tax-us.c 

libgncmod_tax_us_la_LDFLAGS = -module
libgncmod_tax_us_la_LIBADD = \
 ${GUILE_LIBS} \
 ${GLIB_LIBS}

AM_CFLAGS = -I${top_srcdir}/src/gnc-module ${GLIB_CFLAGS} ${GUILE_INCS}

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = us.scm
endif

.scm-links:
	rm -f gnucash tax
	ln -sf . gnucash
	ln -sf . tax 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  ln -sf ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

noinst_DATA = .scm-links

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/tax
gncscmmod_DATA = us.scm

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = txf.scm txf-help.scm 

EXTRA_DIST = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}

CLEANFILES = ${SCM_FILE_LINKS} gnucash tax .scm-links
