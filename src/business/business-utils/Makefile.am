pkglib_LTLIBRARIES = libgncmod-business-utils.la

AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/business/business-core \
  ${G_WRAP_COMPILE_ARGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GNOME_INCLUDEDIR}

libgncmod_business_utils_la_SOURCES = \
  gncmod-business-utils.c \
  business-options.c

noinst_HEADERS = \
  business-options.h

libgncmod_business_utils_la_LDFLAGS = -module
libgncmod_business_utils_la_LIBADD = \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${EFENCE_LIBS}

# Guile Modules
gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = business-utils.scm

# For regular scheme definitions
gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
  business-options.scm \
  business-prefs.scm

EXTRA_DIST = \
  .cvsignore \
  ${gncscm_DATA} \
  ${gncmod_DATA}

noinst_DATA = .scm-links

.scm-links: 
	rm -f gnucash
	ln -sf ${srcdir} gnucash 
	touch .scm-links

DISTCLEANFILES = gnucash .scm-links
