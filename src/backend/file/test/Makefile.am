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

TESTS_ENVIRONMENT = \
  GNC_MODULE_PATH="${top_srcdir}/src/engine:${top_srcdir}/src/gnc-module:${top_srcdir}/src/backend/file" \
  GUILE_LOAD_PATH="${G_WRAP_MODULE_DIR}:..:${top_srcdir}/src/gnc-module:${top_srcdir}/src/engine" \
  LTDL_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/engine:${top_srcdir}/src/backend/file \
  LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/engine:${top_srcdir}/src/backend/file:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine/.libs:${top_srcdir}/src/backend/file/.libs

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

LDADD = -L${top_srcdir}/src/gnc-module -L${top_srcdir}/src/gnc-module/.libs \
        -L${top_srcdir}/src/engine -L${top_srcdir}/src/engine/.libs \
        ${top_srcdir}/src/test-core/libgncmod-test.la \
        ${top_srcdir}/src/gnc-module/libgncmodule.la \
        ${top_srcdir}/src/engine/libgncmod-engine.la \
        ${top_srcdir}/src/engine/test-core/libgncmod-test-engine.la \
        ../libgncmod-backend-file.la \
        ./libgnc-test-file-stuff.la

CFLAGS = ${GLIB_CFLAGS}

INCLUDES = \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I..

EXTRA_DIST = \
  grab-types.pl \
  test-load-module \
  test-real-data.sh
