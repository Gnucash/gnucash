SUBDIRS = test-files

TESTS = \
  test-date-converting \
  test-dom-converters1 \
  test-kvp-frames \
  test-load-example-account \
  test-load-module \
  test-load-xml2 \
  test-real-data.sh \
  test-string-converters \
  test-xml-account \
  test-xml-commodity \
  test-xml-pricedb \
  test-xml-transaction \
  test-xml2-is-file

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/backend/file \
  --guile-load-dir ${G_WRAP_MODULE_DIR}

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_LTLIBRARIES = libgnc-test-file-stuff.la

libgnc_test_file_stuff_la_SOURCES = test-file-stuff.c

noinst_PROGRAMS = \
  test-date-converting \
  test-dom-converters1 \
  test-kvp-frames \
  test-load-example-account \
  test-load-xml2 \
  test-save-in-lang \
  test-string-converters \
  test-xml-account \
  test-xml-commodity \
  test-xml-pricedb \
  test-xml-transaction \
  test-xml2-is-file

noinst_HEADERS = test-file-stuff.h

LDADD = ${top_srcdir}/src/test-core/libgncmod-test.la \
        ${top_srcdir}/src/gnc-module/libgncmodule.la \
        ${top_srcdir}/src/engine/libgncmod-engine.la \
        ${top_srcdir}/src/engine/libgw-engine.la \
        ${top_srcdir}/src/engine/libgw-kvp.la \
        ${top_srcdir}/src/engine/test-core/libgncmod-test-engine.la \
        ../libgncmod-backend-file.la \
        ./libgnc-test-file-stuff.la 

AM_CFLAGS = \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I.. \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}

EXTRA_DIST = \
  grab-types.pl \
  test-load-module \
  test-real-data.sh
