include $(top_srcdir)/test-templates/Makefile.decl

check_PROGRAMS = \
  test-link-module \
  test-exp-parser \
  test-scm-query-string \
  test-print-parse-amount \
  test-sx \
  test-app-utils

TESTS =  \
  test-load-module \
  ${check_PROGRAMS}

test_exp_parser_SOURCES = \
  test-exp-parser.c

test_print_parse_amount_SOURCES = \
  test-print-parse-amount.c

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --guile-load-dir ${top_builddir}/src/core-utils \
  --guile-load-dir ${top_builddir}/src/gnc-module \
  --guile-load-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_builddir}/src/app-utils \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/backend/xml \
  --library-dir    ${top_builddir}/src/backend/sql \
  --library-dir    ${top_builddir}/src/app-utils

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  SRCDIR=${srcdir} \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

LDADD = \
   ${top_builddir}/src/libqof/qof/libgnc-qof.la \
   ${top_builddir}/src/core-utils/libgnc-core-utils.la \
   ${top_builddir}/src/engine/libgncmod-engine.la \
   ${top_builddir}/src/gnc-module/libgnc-module.la \
   ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
   ${top_builddir}/src/test-core/libtest-core.la \
   ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
   ${GUILE_LIBS}

EXTRA_DIST += \
  test-load-module

AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/app-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/libqof/qof \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

test_app_utils_SOURCES = \
	test-app-utils.c \
	test-option-util.c

test_app_utils_CFLAGS = \
	${DEFAULT_INCLUDES} \
	-I${top_srcdir}/${MODULEPATH}/ \
	-DTESTPROG=test_app_utils \
	${GLIB_CFLAGS}
