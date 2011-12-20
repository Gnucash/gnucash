noinst_LTLIBRARIES = libtest-core.la

SWIG_FILES = test-stuff.i

libtest_core_la_SOURCES = \
	test-stuff.c

libtest_core_la_LIBADD = \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GLIB_LIBS}

noinst_HEADERS=test-stuff.h

libtest_core_la_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/libqof/qof \
  ${GLIB_CFLAGS}

libtest_core_la_LDFLAGS = \
  -rpath ${exec-prefix}/lib

if BUILDING_FROM_SVN
swig-test-stuff-guile.c: test-stuff.i $(top_srcdir)/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src \
	${AM_CPPFLAGS} -o $@ $<

swig-test-stuff-python.c: test-stuff.i $(top_srcdir)/src/base-typemaps.i
	$(SWIG) -python  -Wall -Werror $(SWIG_ARGS) \
	-I${top_srcdir}/src \
	${AM_CPPFLAGS} -o $@ $<

test-stuff.py: swig-test-stuff-python.c ${SWIG_FILES}
endif
noinst_LTLIBRARIES += libtest-core-guile.la
libtest_core_guile_la_SOURCES = swig-test-stuff-guile.c
libtest_core_guile_la_LIBADD = \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  libtest-core.la

libtest_core_guile_la_CFLAGS = \
	${libtest_core_la_CPPFLAGS} \
	${GUILE_CFLAGS}

libtest_core_guile_la_LDFLAGS = \
  -rpath ${exec-prefix}/lib

if WITH_PYTHON

noinst_LTLIBRARIES += _test_stuff.la

_test_stuff_la_SOURCES = \
  swig-test-stuff-python.c
_test_stuff_la_CFLAGS = \
  ${libtest_core_la_CPPFLAGS} \
  ${PYTHON_CPPFLAGS}
_test_stuff_la_LDFLAGS = \
  ${PYTHON_LDFLAGS} \
  -module \
  -rpath ${exec-prefix}/lib

_test_stuff_la_LIBADD = \
  ${PYTHON_LIBS} \
  ${PYTHON_EXTRA_LIBS} \
  libtest-core.la

endif
SCM_FILES = test-stuff.scm

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = $(SCM_FILES)

.scm-links:
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILES} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(SCM_FILES) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

clean-local:
	$(RM) -rf gnucash

noinst_DATA = .scm-links
CLEANFILES = .scm-links

EXTRA_DIST = \
  $(SCM_FILES) \
  swig-test-stuff-python.c \
  swig-test-stuff-guile.c \
  test-stuff.i

MAINTAINERCLEANFILES = swig-test-stuff-guile.c swig-test-stuff-python.c