TESTS = test-load-module
#TESTS = test-load-module test-dom-converters1 test-kvp-frames	\
#  test-string-converters test-xml-account test-xml2-is-file	\
#  test-load-xml2 test-load-example-account			\
#  test-xml-commodity test-xml-transaction test-real-data.sh

TESTS_ENVIRONMENT = \
  GNC_MODULE_PATH="${top_srcdir}/src/engine:${top_srcdir}/src/gnc-module:${top_srcdir}/src/backend/file" \
  GUILE_LOAD_PATH="${G_WRAP_MODULE_DIR}:..:${top_srcdir}/src/gnc-module" \
  LTDL_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/engine:${top_srcdir}/src/backend/file

noinst_PROGRAMS = test-dom-converters1 test-kvp-frames		\
  test-string-converters test-xml-account test-xml2-is-file	\
  test-load-xml2 test-load-example-account			\
  test-xml-commodity test-xml-transaction test-save-in-lang 

LDADD = ../../../engine/libgncmod-engine.la \
        ../../../gnc-module/libgncmodule.la \
        ../libgncmod-backend-file.la

CFLAGS=${GLIB_CFLAGS}
INCLUDES=-I ../../../engine -I ../


test_kvp_frames_SOURCES = test-kvp-frames.c test-stuff.c gnc-test-stuff.c

test_load_example_account_SOURCES = test-load-example-account.c test-stuff.c gnc-test-stuff.c

test_dom_converters1_SOURCES = test-dom-converters1.c test-stuff.c gnc-test-stuff.c

test_string_converters_SOURCES = test-string-converters.c test-stuff.c gnc-test-stuff.c

test_xml_account_SOURCES = test-xml-account.c test-stuff.c gnc-test-stuff.c

test_xml_commodity_SOURCES = test-xml-commodity.c test-stuff.c gnc-test-stuff.c

test_xml_transaction_SOURCES = test-xml-transaction.c test-stuff.c gnc-test-stuff.c

test_xml2_is_file_SOURCES = test-xml2-is-file.c test-stuff.c gnc-test-stuff.c

test_load_xml2_SOURCES = test-load-xml2.c test-stuff.c gnc-test-stuff.c

test_save_in_lang_SOURCES = test-save-in-lang.c test-stuff.c gnc-test-stuff.c

