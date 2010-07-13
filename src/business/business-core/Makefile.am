SUBDIRS = . sql test

pkglib_LTLIBRARIES = libgncmod-business-core.la

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_builddir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

libgncmod_business_core_la_SOURCES = \
  gncmod-business-core.c

noinst_HEADERS =

libgncmod_business_core_la_LDFLAGS = -avoid-version

libgncmod_business_core_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${top_builddir}/src/engine/libgncmod-engine.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${EFENCE_LIBS}

# business-core-helpers.c

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = business-core.scm

EXTRA_DIST = ${gncmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For executing test cases
SCM_FILE_LINKS = ${gncmod_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

noinst_DATA = .scm-links
CLEANFILES = gnucash .scm-links ${SCM_FILE_LINKS}
MAINTAINERCLEANFILES =

INCLUDES = -DG_LOG_DOMAIN=\"gnc.business.core\"
