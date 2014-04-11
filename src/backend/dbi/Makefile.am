SUBDIRS = . test

# Now a shared library AND a GModule
pkglib_LTLIBRARIES = libgncmod-backend-dbi.la


AM_CFLAGS = \
  -I.. -I../.. \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/backend/sql \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils\
  -I${top_srcdir}/lib/libc\
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${WARN_CFLAGS}

libgncmod_backend_dbi_la_SOURCES = \
  gnc-backend-dbi.c

noinst_HEADERS = \
  gnc-backend-dbi.h

libgncmod_backend_dbi_la_LDFLAGS = -module -avoid-version
libgncmod_backend_dbi_la_LIBADD = \
   ${GLIB_LIBS} ${GCONF_LIBS} \
   ${top_builddir}/src/backend/sql/libgnc-backend-sql.la \
   ${top_builddir}/src/engine/libgncmod-engine.la \
   ${LIBDBI_LIBS} \
   ${QOF_LIBS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.dbi\"
