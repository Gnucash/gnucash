noinst_LTLIBRARIES=libgncmod-test-engine.la

LDADD = \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ../libgncmod-engine.la \
  ${GLIB_LIBS}

libgncmod_test_engine_la_SOURCES = gncmod-test-engine.c test-engine-stuff.cpp
libgncmod_test_engine_la_LDFLAGS = -module
libgncmod_test_engine_la_LIBADD = \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ../libgncmod-engine.la \
  ${GLIB_LIBS}

noinst_HEADERS=test-engine-stuff.h test-engine-strings.h

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/engine \
  ${GLIB_CFLAGS} \
  ${GUILE_CFLAGS} \
  ${BOOST_CPPFLAGS}

EXTRA_DIST = CMakeLists.txt
