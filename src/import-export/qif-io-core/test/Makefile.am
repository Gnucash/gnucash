LDADD=${top_builddir}/src/gnc-module/libgnc-module.la ${GLIB_LIBS}

AM_CPPFLAGS = -I${top_srcdir}/src/gnc-module ${GUILE_INCS} ${GLIB_CFLAGS}

TESTS=test-load-module test-parser test-reader test-file-formats \
test-import-phase-1

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/backend/xml \
  --gnc-module-dir ${top_builddir}/src/import-export/qif-io-core \
  --guile-load-dir ${top_srcdir}/src/import-export/qif-io-core/test \
  --guile-load-dir ${top_srcdir}/lib \
  --guile-load-dir ${top_builddir}/src/gnome-utils \
  --guile-load-dir ${top_builddir}/src/gnome \
  --guile-load-dir ${top_builddir}/src/scm \
  --library-dir    ${top_builddir}/src/gnome-utils \
  --library-dir    ${top_srcdir}/src/gnome

TESTS_ENVIRONMENT = \
  SRCDIR=${srcdir} \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

print_test_env:
	echo ${GNC_TEST_DEPS}
	echo ${TESTS_ENVIRONMENT}
