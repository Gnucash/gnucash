LDADD=${top_srcdir}/src/gnc-module/libgncmodule.la ${GLIB_LIBS} -lltdl
CFLAGS=@CFLAGS@ ${GLIB_CFLAGS} 
INCLUDES = -I${top_srcdir}/src/gnc-module ${GUILE_INCS} 

TESTS=test-load-module test-parser test-reader test-file-formats \
test-import-phase-1

TESTS_ENVIRONMENT = \
  GNC_MODULE_PATH="${top_srcdir}/src/modules/qif-io-core:${top_srcdir}/src/modules/engine" GUILE_LOAD_PATH="../:../../engine/:../../../gnc-module/:${G_WRAP_MODULE_DIR}"

