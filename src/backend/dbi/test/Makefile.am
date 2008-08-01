SUBDIRS = .

TESTS = \
  test-load-backend \
  test-dbi-account

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/lib/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/gda

TESTS_ENVIRONMENT := \
  GNC_ACCOUNT_PATH=${top_srcdir}/accounts/C \
  SRCDIR=${srcdir} \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-load-backend \
  test-dbi-account

#noinst_HEADERS = test-file-stuff.h

LDADD = ${top_builddir}/src/test-core/libgncmod-test.la \
        ${top_builddir}/src/gnc-module/libgnc-module.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
        ${top_builddir}/src/core-utils/libgnc-core-utils.la \
        ${QOF_LIBS} \
        ${top_builddir}/lib/libc/libc-missing.la

AM_CFLAGS = \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/backend/gda \
  -I${top_srcdir}/src/backend/qsf \
  ${GLIB_CFLAGS} \
  ${QOF_CFLAGS} \
  ${GUILE_INCS} \
  ${GCONF_CFLAGS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.dbi\"
