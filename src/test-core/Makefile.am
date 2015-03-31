noinst_LTLIBRARIES = libtest-core.la

SWIG_FILES = \
	unittest-support.i

libtest_core_la_SOURCES = \
	test-stuff.c \
	unittest-support.c

libtest_core_la_LIBADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GLIB_LIBS}

noinst_HEADERS = \
	test-stuff.h \
	unittest-support.h

libtest_core_la_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/libqof/qof \
  ${GLIB_CFLAGS}

libtest_core_la_LDFLAGS = \
  -rpath ${exec-prefix}/lib

if BUILDING_FROM_VCS
swig-unittest-support-guile.c: unittest-support.i $(top_srcdir)/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src \
	${AM_CPPFLAGS} -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/src/swig-utf8.patch; \
	fi
endif
endif

swig-unittest-support-python.c: unittest-support.i $(top_srcdir)/src/base-typemaps.i
	$(SWIG) -python  -Wall -Werror $(SWIG_ARGS) \
	-I${top_srcdir}/src \
	${AM_CPPFLAGS} -o $@ $<

unittest-support.py: swig-unittest-support-python.c ${SWIG_FILES}
endif
noinst_LTLIBRARIES += libtest-core-guile.la
libtest_core_guile_la_SOURCES = swig-unittest-support-guile.c
libtest_core_guile_la_LIBADD = \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  libtest-core.la

libtest_core_guile_la_CFLAGS = \
	${libtest_core_la_CPPFLAGS} \
	${GUILE_CFLAGS} \
	${AM_CFLAGS}

libtest_core_guile_la_LDFLAGS = \
  -rpath ${exec-prefix}/lib

if WITH_PYTHON

noinst_LTLIBRARIES += _unittest_support.la

_unittest_support_la_SOURCES = \
  swig-unittest-support-python.c
_unittest_support_la_CFLAGS = \
  ${libtest_core_la_CPPFLAGS} \
  ${PYTHON_CPPFLAGS}
_unittest_support_la_LDFLAGS = \
  ${PYTHON_LDFLAGS} \
  -module \
  -rpath ${exec-prefix}/lib

_unittest_support_la_LIBADD = \
  ${PYTHON_LIBS} \
  ${PYTHON_EXTRA_LIBS} \
  libtest-core.la

endif

if WITH_GOOGLE_TEST
if GOOGLE_TEST_LIBS
noinst_LIBRARIES = libgmock.a
else
noinst_LIBRARIES = \
        libgtest.a \
        libgmock.a
nodist_libgtest_a_SOURCES = ${GTEST_SRC}/src/gtest-all.cc
libgtest_a_CPPFLAGS = ${AM_CPPFLAGS} -I${GTEST_HEADERS} -I${GTEST_SRC}
endif
nodist_libgmock_a_SOURCES = ${GMOCK_SRC_PATH}/gmock-all.cc
libgmock_a_CPPFLAGS = ${libgtest_a_CPPFLAGS} -I${GMOCK_HEADERS} -I${GMOCK_SRC}
endif

gncscmmoddir = ${GNC_SCM_INSTALL_DIR}/gnucash
gncscmmod_DATA = unittest-support.scm

SCM_FILES = $(gncscmmod_DATA)

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILES} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncscmmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

if GNC_HAVE_GUILE_2
GUILE_COMPILE_ENV = \
  --library-dir    ${top_builddir}/src/test-core \
  --library-dir    ${top_builddir}/src/libqof/qof

%.go : %.scm .scm-links $(noinst_LTLIBRARIES)
	GNC_UNINSTALLED=yes \
	GNC_BUILDDIR=${top_builddir} \
	$(shell ${abs_top_srcdir}/src/gnc-test-env.pl --noexports ${GUILE_COMPILE_ENV}) \
	$(GUILD) compile -o $@ $<

gncscmmodcachedir = ${pkglibdir}/scm/ccache/@GUILE_EFFECTIVE_VERSION@/gnucash
gncscmmodcache_DATA = $(gncscmmod_DATA:.scm=.go)
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links
CLEANFILES = .scm-links ${gncscmmodcache_DATA}

EXTRA_DIST = \
  $(SCM_FILES) \
  swig-unittest-support-python.c \
  swig-unittest-support-guile.c \
  unittest-support.i

MAINTAINERCLEANFILES = \
	swig-unittest-support-guile.c \
	swig-unittest-support-python.c
