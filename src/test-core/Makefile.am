noinst_LTLIBRARIES=libgncmod-test.la

libgncmod_test_la_SOURCES = gncmod-test.c test-stuff.c
libgncmod_test_la_LDFLAGS = -module
libgncmod_test_la_LIBADD = \
  ${top_srcdir}/src/gnc-module/libgncmodule.la \
  ${GLIB_LIBS}

noinst_HEADERS=test-stuff.h

AM_CFLAGS = -I${top_srcdir}/src/gnc-module ${GLIB_CFLAGS}
