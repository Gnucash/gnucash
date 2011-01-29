SUBDIRS = .

check_LTLIBRARIES=libgncmod-test-dbi.la

libgncmod_test_dbi_la_SOURCES = \
    test-dbi-stuff.c
libgncmod_test_dbi_la_LDFLAGS = -module

test_dbi_business_SOURCES = \
  test-dbi-business.c \
  test-dbi-business-stuff.c

test_dbi_basic_SOURCES = \
  test-dbi-basic.c

test_dbi_SOURCES = \
  test-dbi.c

TESTS = \
  test-dbi-basic \
  test-dbi \
  test-dbi-business \
  test-load-backend

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/src/engine \
  --guile-load-dir ${top_builddir}/src/engine \
  --library-dir    ${top_builddir}/src/libqof/qof \
  --library-dir    ${top_builddir}/src/core-utils \
  --library-dir    ${top_builddir}/src/gnc-module \
  --library-dir    ${top_builddir}/src/engine

if CUSTOM_GNC_DBD_DIR
gnc_dbd_dir_override = GNC_DBD_DIR="@GNC_DBD_DIR@"
endif

TESTS_ENVIRONMENT = \
  GNC_ACCOUNT_PATH=${top_srcdir}/accounts/C \
  SRCDIR=${srcdir} \
  ${gnc_dbd_dir_override} \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-dbi-basic \
  test-dbi \
  test-dbi-business \
  test-load-backend

EXTRA_DIST = \
    test-dbi-stuff.h \
    test-dbi-business-stuff.h

LDADD = ${top_builddir}/src/test-core/libtest-core.la \
        ${top_builddir}/src/gnc-module/libgnc-module.la \
        ${top_builddir}/src/engine/libgncmod-engine.la \
        ${top_builddir}/src/engine/test-core/libgncmod-test-engine.la \
        ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
        ${top_builddir}/src/core-utils/libgnc-core-utils.la \
        ${top_builddir}/src/libqof/qof/libgnc-qof.la \
        ${top_builddir}/src/backend/dbi/test/libgncmod-test-dbi.la \
        ${top_builddir}/src/backend/sql/libgnc-backend-sql.la \
        ${top_builddir}/lib/libc/libc-missing.la

AM_CFLAGS = \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/core-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/test-core \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/engine/test-core \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src/backend/sql \
  -DTEST_MYSQL_URL=\"${TEST_MYSQL_URL}\" \
  -DTEST_PGSQL_URL=\"${TEST_PGSQL_URL}\" \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS} \
  ${GCONF_CFLAGS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.dbi\"

# If you let make run parallel builds it complains that there is no rule to make
# libgncmod-test-dbi.la.  This is wrong, but I can't figure out how to fix it
# so I'll turn off parallel builds
.NOTPARALLEL:
