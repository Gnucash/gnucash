AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/common/test-core \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/gnucash/import-export \
  -I${top_srcdir}/gnucash/import-export/qif \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

LDADD = \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la \
  ${top_builddir}/common/test-core/libtest-core.la \
  ../../libgncmod-generic-import.la \
  ../libgncmod-qif.la \
  ${GLIB_LIBS}

TESTS = \
  test-link \
  test-qif

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/libgnucash/core-utils \
  --gnc-module-dir ${top_builddir}/libgnucash/gnc-module \
  --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --gnc-module-dir ${top_builddir}/gnucash/import-export \
  --gnc-module-dir ${top_builddir}/gnucash/import-export/qif \
  --gnc-module-dir ${top_builddir}/gnucash/gnome-utils \
  --gnc-module-dir ${top_srcdir}/libgnucash/gnc-module \
  --gnc-module-dir ${top_srcdir}/libgnucash/engine \
  --gnc-module-dir ${top_srcdir}/libgnucash/app-utils \
  --gnc-module-dir ${top_srcdir}/gnucash/gnome-utils \
  --gnc-module-dir ${top_builddir}/gnucash/gnome-utils \
  --gnc-module-dir ${top_builddir}/gnucash/gnome \
  --guile-load-dir ${top_srcdir}/libgnucash/scm \
  --guile-load-dir ${top_srcdir}/gnucash/import-export \
  --guile-load-dir ${top_srcdir}/gnucash/import-export/qif

TESTS_ENVIRONMENT = \
  GNC_TEST_FILES=${srcdir}/test-files \
  $(shell ${abs_top_srcdir}/common/gnc-test-env.pl --noexports ${GNC_TEST_DEPS})

check_PROGRAMS = \
  test-link \
  test-qif

EXTRA_DIST = \
  test-files/test-1-bank-txn.qif CMakeLists.txt
