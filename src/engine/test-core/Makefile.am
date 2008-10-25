check_LTLIBRARIES=libgncmod-test-engine.la

LDADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ../libgncmod-engine.la \
  ${GLIB_LIBS} \
  ${QOF_LIBS}

libgncmod_test_engine_la_SOURCES = gncmod-test-engine.c test-engine-stuff.c
libgncmod_test_engine_la_LDFLAGS = -module
libgncmod_test_engine_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ../libgncmod-engine.la \
  ${QOF_LIBS} \
  ${GLIB_LIBS}

noinst_HEADERS=test-engine-stuff.h test-engine-strings.h

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}
