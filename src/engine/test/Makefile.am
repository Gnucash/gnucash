INCLUDES=-I${top_srcdir}/src/gnc-module ${GUILE_INCS} 
CFLAGS=@CFLAGS@ ${GLIB_CFLAGS} 
LDADD=${top_srcdir}/src/gnc-module/libgncmodule.la ${GLIB_LIBS} -lltdl

TESTS = test-load-engine test-create-account
TESTS_ENVIRONMENT = \
  GNC_MODULE_PATH="${top_srcdir}/src/engine" \
  GUILE_LOAD_PATH="${G_WRAP_MODULE_DIR}:..:../../gnc-module" \
  LTDL_LIBRARY_PATH=../../gnc-module \
  LD_LIBRARY_PATH=../../gnc-module:../../gnc-module/.libs

bin_PROGRAMS = test-load-engine
test_load_engine_SOURCES=test-load-engine.c

