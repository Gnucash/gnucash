noinst_LTLIBRARIES=libgncmod-test-engine.la

LDADD = \
  ${top_srcdir}/src/gnc-module/libgncmodule.la \
  ../libgncmod-engine.la \
  ${GLIB_LIBS} \
  -lltdl

libgncmod_test_engine_la_SOURCES = gncmod-test-engine.c test-engine-stuff.c
libgncmod_test_engine_la_LDFLAGS = -module
libgncmod_test_engine_la_LIBADD = \
  -L${top_srcdir}/src/gnc-module -L${top_srcdir}/src/gnc-module/.libs -lgncmodule \
  ${GLIB_LIBS}

noinst_HEADERS=test-engine-stuff.h

AM_CFLAGS = \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  ${GLIB_CFLAGS}
