AM_CPPFLAGS=\
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

PWD := $(shell pwd)
check_LTLIBRARIES=libfoo.la libgncmodfoo.la

libfoo_la_SOURCES = foo.c
libfoo_la_LDFLAGS = -rpath ${PWD}

# Make swig-foo.c for dist. It is an automatic dependency
# of gnc-mod-foo.c
noinst_HEADERS = \
  foo.h \
  swig-foo.c

libgncmodfoo_la_SOURCES = gnc-mod-foo.c
libgncmodfoo_la_LDFLAGS = -rpath ${PWD} -avoid-version
libgncmodfoo_la_LIBADD = \
  libfoo.la \
  $(GUILE_LIBS) \
  $(GLIB_LIBS)

EXTRA_DIST = \
  foo.i \
  gnucash/foo.scm

if BUILDING_FROM_VCS
swig-foo.c: foo.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/src/swig-utf8.patch; \
	fi
endif
endif
endif

BUILT_SOURCES = swig-foo.c
MAINTAINERCLEANFILES = swig-foo.c
