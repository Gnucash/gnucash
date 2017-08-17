# Makefile.am for src/backend/dbi/test

include $(top_srcdir)/test-templates/Makefile.decl

AM_CPPFLAGS = \
-I${top_srcdir}/borrowed/libc \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/engine/test-core \
  -I${top_srcdir}/libgnucash/backend/sql \
  -DTEST_MYSQL_URL=\"${TEST_MYSQL_URL}\" \
  -DTEST_PGSQL_URL=\"${TEST_PGSQL_URL}\" \
  ${GLIB_CFLAGS} \
  ${GUILE_CFLAGS} \
  ${BOOST_CPPFLAGS}

LDADD = ${top_builddir}/common/test-core/libtest-core.la \
        ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
        ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
        ${top_builddir}/libgnucash/engine/test-core/libgncmod-test-engine.la \
        ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
        ${top_builddir}/libgnucash/backend/sql/libgnc-backend-sql.la \
        ${top_builddir}/libgnucash/backend/xml/libgnc-backend-xml-utils.la \
        ${top_builddir}/borrowed/libc/libc-missing.la

GNC_TEST_DEPS = \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/core-utils \
  --library-dir    ${top_builddir}/libgnucash/gnc-module \
  --library-dir    ${top_builddir}/libgnucash/engine \
  --library-dir    ${top_builddir}/libgnucash/backend/xml

TESTS_ENVIRONMENT = \
  GNC_BUILDDIR=${abs_top_builddir} \
  GNC_ACCOUNT_PATH=${top_srcdir}/accounts/C \
  SRCDIR=${srcdir} \
  ${gnc_dbd_dir_override} \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

EXTRA_DIST += \
    test-dbi-stuff.h \
    test-dbi-business-stuff.h \
    test-dbi.xml \
    CMakeLists.txt

check_PROGRAMS = test-backend-dbi

TESTS = ${check_PROGRAMS}

if CUSTOM_GNC_DBD_DIR
gnc_dbd_dir_override = GNC_DBD_DIR="@GNC_DBD_DIR@"
endif

test_backend_dbi_SOURCES = \
    test-backend-dbi.cpp \
    test-backend-dbi-basic.cpp \
    test-dbi-stuff.cpp \
    test-dbi-business-stuff.cpp \
    ../gnc-dbisqlconnection.cpp \
    ../gnc-backend-dbi.cpp \
    ../gnc-dbisqlresult.cpp

test_backend_dbi_CPPFLAGS = \
	-DDBI_TEST_XML_FILENAME=\"${srcdir}/test-dbi.xml\" \
	${AM_CPPFLAGS}

test_backend_dbi_LDADD = \
    ${BOOST_LDFLAGS} -lboost_regex \
    ${LIBDBI_LIBS} \
    ${LDADD}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.backend.dbi\"

# If you let make run parallel builds it complains that there is no rule to make
# libgncmod-test-dbi.la.  This is wrong, but I can't figure out how to fix it
# so I'll turn off parallel builds
#.NOTPARALLEL:
