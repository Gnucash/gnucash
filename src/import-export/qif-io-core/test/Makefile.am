LDADD=${top_builddir}/src/gnc-module/libgncmodule.la ${GLIB_LIBS} -lltdl

AM_CFLAGS = -I${top_srcdir}/src/gnc-module ${GUILE_INCS} ${GLIB_CFLAGS} 

TESTS=test-load-module test-parser test-reader test-file-formats \
test-import-phase-1

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/import-export/qif-io-core \
  --guile-load-dir ${top_srcdir}/src/import-export/qif-io-core/test \
  --guile-load-dir ${top_srcdir}/lib \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/gnome \
  --guile-load-dir ${top_builddir}/src/scm \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_srcdir}/src/gnome \
  --library-dir    ${G_WRAP_LIB_DIR}

TESTS_ENVIRONMENT := \
  SRCDIR=${srcdir} \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

#TESTS_ENVIRONMENT = \
#  GNC_MODULE_PATH="${top_srcdir}/src/modules/qif-io-core:${top_srcdir}/src/modules/engine" \
#  GUILE_LOAD_PATH="../:../../engine/:../../../gnc-module/:${G_WRAP_MODULE_DIR}" \
#  LD_LIBRARY_PATH=${top_srcdir}/src/gnc-module:${top_srcdir}/src/gnc-module/.libs:${top_srcdir}/src/engine:${top_srcdir}/src/engine/.libs:${top_srcdir}/src/app-utils:${top_srcdir}/src/app-utils/.libs

