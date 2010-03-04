noinst_LTLIBRARIES=libtest-core.la

libtest_core_la_SOURCES = test-stuff.c
libtest_core_la_LDFLAGS = -module
libtest_core_la_LIBADD = \
  ${GLIB_LIBS}

noinst_HEADERS=test-stuff.h

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  ${GLIB_CFLAGS}
