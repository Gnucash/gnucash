TESTS =  \
  test-link-module ${SCM_TESTS}

# The following tests are nice, but have absolutely no place in an
# automated testing system.
#
TESTS_GUI = test-gnc-recurrence

##lib_LTLIBRARIES = libgncgnome.la

GNC_TEST_DEPS = --gnc-module-dir ${top_builddir}/libgnucash/engine \
  --gnc-module-dir ${top_builddir}/libgnucash/app-utils \
  --gnc-module-dir ${top_builddir}/gnucash/gnome-utils \
  --guile-load-dir ${top_builddir}/libgnucash/gnc-module \
  --guile-load-dir ${top_builddir}/libgnucash/scm \
  --guile-load-dir ${top_builddir}/libgnucash/engine \
  --guile-load-dir ${top_builddir}/libgnucash/core-utils \
  --guile-load-dir ${top_builddir}/libgnucash/app-utils \
  --guile-load-dir ${top_builddir}/gnucash/gnome-utils \
  --library-dir    ${top_builddir}/gnucash/gnome-utils \
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

check_PROGRAMS = \
  test-link-module test-gnc-recurrence

AM_CPPFLAGS = \
  -I${top_srcdir}/common \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/engine/test-core \
  -I${top_srcdir}/gnucash/gnome-utils \
  -I${top_srcdir}/libgnucash/gnc-module \
  -I${top_srcdir}/libgnucash/app-utils \
  -I${top_srcdir}/common/test-core \
  ${GLIB_CFLAGS} \
  ${GTK_CFLAGS} \
  ${GUILE_CFLAGS}

LDADD = \
  ${GNOME_LIBS} \
  ${top_builddir}/libgnucash/app-utils/libgncmod-app-utils.la \
  ${top_builddir}/gnucash/gnome-utils/libgncmod-gnome-utils.la \
  ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
  ${top_builddir}/libgnucash/engine/test-core/libgncmod-test-engine.la \
  ${top_builddir}/common/test-core/libtest-core.la

test_gnc_recurrence_SOURCES=test-gnc-recurrence.c
test_gnc_recurrence_LDADD = \
  ${GTK_LIBS} \
  ${LDADD}

test_link_module_SOURCES=test-link-module.c
test_link_module_LDADD = \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${top_builddir}/libgnucash/gnc-module/libgnc-module.la

SCM_TESTS =   test-load-gnome-utils-module
SCM_TEST_SRCS = $(SCM_TESTS:%=%.scm)

$(SCM_TESTS): %: $(srcdir)/%.scm Makefile
	echo 'export GNC_BUILDDIR="${abs_top_builddir}";' > $@
	echo 'export GNC_UNINSTALLED=yes;' >> $@
	echo '${GUILE} --debug -l $(srcdir)/$*.scm -c "(exit (run-test))"' >> $@
	chmod a+x $@

EXTRA_DIST = \
  ${SCM_TEST_SRCS} \
  CMakeLists.txt

testit:
	libtool --mode execute gdb test-link-module
