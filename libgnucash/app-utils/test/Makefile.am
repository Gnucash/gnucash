include $(top_srcdir)/test-templates/Makefile.decl

check_PROGRAMS = \
  test-link-module \
  test-exp-parser \
  test-scm-query-string \
  test-print-parse-amount \
  test-sx \
  test-app-utils

TESTS =  \
  ${check_PROGRAMS} \
  ${SCM_TESTS}

test_scm_query_string_SOURCES = test-scm-query-string.cpp
test_sx_SOURCES = test-sx.cpp
test_print_parse_amount_SOURCES = test-print-parse-amount.cpp

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/engine/test \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/engine/test \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/backend/xml \
  --library-dir    ${top_builddir}/libgnucash/backend/sql \
  --library-dir    ${top_builddir}/libgnucash/app-utils

TESTS_ENVIRONMENT = \
  GUILE_WARN_DEPRECATED=no \
  GUILE="${GUILE}" \
  SRCDIR=${srcdir} \
  GNC_BUILDDIR="${abs_top_builddir}" \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

LDADD = \
   ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
   ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
   ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
   ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
   ${top_builddir}/common/test-core/libtest-core.la \
   ${top_builddir}/libgnucash/engine/test-core/libgncmod-test-engine.la \
   ${GUILE_LIBS}

EXTRA_DIST += \
  test-print-queries.cpp \
  ${SCM_TEST_SRCS} \
  CMakeLists.txt

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/engine/test-core \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/core-utils \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${BOOST_CPPFLAGS}

test_app_utils_SOURCES = \
	test-app-utils.c \
	test-option-util.cpp \
	test-gnc-ui-util.c

test_app_utils_CXXFLAGS = \
	${DEFAULT_INCLUDES} \
	-I${top_srcdir}/${MODULEPATH}/ \
	-DTESTPROG=test_app_utils \
	${GLIB_CFLAGS}

SCM_TESTS =   test-load-app-utils-module test-c-interface
SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

$(SCM_TESTS): %: $(srcdir)/%.scm Makefile
	echo 'export GNC_BUILDDIR="${abs_top_builddir}";' > $@
	echo 'export GNC_UNINSTALLED=yes;' >> $@
	echo '${GUILE} --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' >> $@
	chmod a+x $@
