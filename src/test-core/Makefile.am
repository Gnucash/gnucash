noinst_LTLIBRARIES=libgncmod-test.la

libgncmod_test_la_SOURCES = gncmod-test.c test-stuff.c
libgncmod_test_la_LDFLAGS = -module
libgncmod_test_la_LIBADD = \
  -L${top_srcdir}/src/gnc-module -L${top_srcdir}/src/gnc-module/.libs -lgncmodule \
  ${GLIB_LIBS}

noinst_HEADERS=test-stuff.h

INCLUDES=-I${top_srcdir}/src/gnc-module ${GLIB_CFLAGS}
