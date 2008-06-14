SUBDIRS = . test

# Now a shared library AND a GModule
pkglib_LTLIBRARIES = libgncmod-backend-gda.la


AM_CFLAGS = \
  -I.. -I../.. \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils\
  -I${top_srcdir}/lib/libc\
  -I${top_srcdir}/src/backend/sql \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${LIBGDA_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${WARN_CFLAGS}

libgncmod_backend_gda_la_SOURCES = \
  gnc-backend-gda.c

noinst_HEADERS = \
  gnc-backend-gda.h

libgncmod_backend_gda_la_LDFLAGS = -module -avoid-version
libgncmod_backend_gda_la_LIBADD = \
   ${GLIB_LIBS} ${GCONF_LIBS} \
   ${top_builddir}/src/engine/libgnc-engine.la \
   ${top_builddir}/src/backend/sql/libgncmod-backend-sql.la \
   ${LIBGDA_LIBS} \
   ${QOF_LIBS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.gda\"
