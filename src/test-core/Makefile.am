noinst_LTLIBRARIES=libgncmod-test.la

libgncmod_test_la_SOURCES = gncmod-test.c test-stuff.c
libgncmod_test_la_LDFLAGS = -module
libgncmod_test_la_LIBADD = \
  ${top_builddir}/src/gnc-module/libgnc-module.la \
  ${GLIB_LIBS}

noinst_HEADERS=test-stuff.h

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}
