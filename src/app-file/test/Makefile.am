TESTS = test-link test-load-module 

GNC_TEST_DEPS := \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/app-file \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/scm \
  --gnc-module-dir ${top_srcdir}/src/engine \
  --guile-load-dir ${G_WRAP_MODULE_DIR} \
  --guile-load-dir ${top_srcdir}/lib \
  --guile-load-dir ${top_srcdir}/src/scm \
  --guile-load-dir ${top_srcdir}/src/gnome \
  --guile-load-dir ${top_builddir}/src/gnome \
  --guile-load-dir ${top_srcdir}/src/gnome-utils \
  --guile-load-dir ${top_srcdir}/src/app-utils \
  --library-dir    ${top_srcdir}/src/gnome \
  --library-dir    ${top_srcdir}/src/gnome-utils

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = test-link

test_link_SOURCES = test-link.c
test_link_LDADD = ../libgncmod-app-file.la 

EXTRA_DIST = \
  test-load-module

testit: 
	${TESTS_ENVIRONMENT} gdb guile
