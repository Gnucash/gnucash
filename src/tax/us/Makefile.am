SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-tax-us.la

libgncmod_tax_us_la_SOURCES = \
  gncmod-tax-us.c 

libgncmod_tax_us_la_LDFLAGS = -avoid-version

libgncmod_tax_us_la_LIBADD = \
 ${GUILE_LIBS} \
 ${GLIB_LIBS}

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GLIB_CFLAGS} ${GUILE_INCS}

gncscmmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash/tax
gncscmmod_DATA = us.scm de_DE.scm

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = txf.scm txf-help.scm txf-de_DE.scm txf-help-de_DE.scm 

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links:
	$(RM) -rf gnucash
	mkdir -p gnucash
	mkdir -p gnucash/tax
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash/tax; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links

EXTRA_DIST = \
  ${gncscmmod_DATA} \
  ${gncscm_DATA}

CLEANFILES = .scm-links
DISTCLEANFILES = ${SCM_FILE_LINKS}
