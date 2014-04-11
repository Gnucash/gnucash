TESTS =  \
  test-link-module test-load-module

# The following tests are nice, but have absolutely no place in an
# automated testing system.
#
TESTS_GUI = test-gnc-recurrence test-gnc-dialog

##lib_LTLIBRARIES = libgncgnome.la

GNC_TEST_DEPS := @GNC_TEST_SRFI_LOAD_CMD@ \
  --gnc-module-dir ${top_builddir}/src/core-utils \
  --gnc-module-dir ${top_builddir}/src/gnc-module \
  --gnc-module-dir ${top_builddir}/src/network-utils \
  --gnc-module-dir ${top_builddir}/src/engine \
  --gnc-module-dir ${top_builddir}/src/calculation \
  --gnc-module-dir ${top_builddir}/src/app-utils \
  --gnc-module-dir ${top_builddir}/src/gnome-utils \
  --gnc-module-dir ${top_builddir}/src/gnome \
  --gnc-module-dir ${top_builddir}/src/scm \
  --guile-load-dir ${top_srcdir}/src/scm \
  --guile-load-dir ${top_srcdir}/src/gnome \
  --guile-load-dir ${top_srcdir}/src/engine \
  --guile-load-dir ${top_srcdir}/src/app-utils \
  --guile-load-dir ${top_srcdir}/src/gnome-utils \
  --library-dir    ${top_srcdir}/src/gnome

TESTS_ENVIRONMENT := \
  $(shell ${top_srcdir}/src/gnc-test-env --no-exports ${GNC_TEST_DEPS})

noinst_PROGRAMS = \
  test-link-module test-gnc-recurrence test-gnc-dialog

INCLUDES= \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/gnome-utils \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/app-utils \
  ${GLIB_CFLAGS} ${GUILE_INCS} ${GNOME_CFLAGS} ${GLADE_CFLAGS} ${QOF_CFLAGS}

test_gnc_recurrence_SOURCES=test-gnc-recurrence.c
test_gnc_recurrence_LDADD = ${GNOME_LIBS} \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la

test_gnc_dialog_LDADD = ${GNOME_LIBS} \
  ${top_builddir}/src/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/src/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/src/engine/libgncmod-engine.la

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD= \
  ${GUILE_LIBS} \
  ${top_builddir}/src/gnc-module/libgncmodule.la 

EXTRA_DIST = \
  test-load-module

testit:
	libtool --mode execute gdb test-link-module


