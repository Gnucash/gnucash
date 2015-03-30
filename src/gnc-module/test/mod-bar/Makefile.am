AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_CFLAGS} \
  ${GLIB_CFLAGS}

PWD := $(shell pwd)
check_LTLIBRARIES = libbar.la libgncmodbar.la

libbar_la_SOURCES = bar.c
libbar_la_LDFLAGS = -rpath ${PWD}

# Make swig-bar.c for dist. It is an automatic dependency
# of gnc-mod-bar.c
noinst_HEADERS = \
  bar.h \
  swig-bar.c

libgncmodbar_la_SOURCES=gnc-mod-bar.c
libgncmodbar_la_LDFLAGS=-rpath ${PWD} -avoid-version
libgncmodbar_la_LIBADD=libbar.la \
  $(GUILE_LIBS) \
  $(GLIB_LIBS)

if BUILDING_FROM_VCS
swig-bar.c: bar.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module -o $@ $<
if ! OS_WIN32
if ! SWIG_DIST_FAIL
	if ! `grep "define scm_from_utf8_string" $@ > /dev/null 2>&1`; then \
	  patch $@ $(top_srcdir)/src/swig-utf8.patch; \
	fi
endif
endif
endif

EXTRA_DIST = \
  bar.i \
  gnucash/bar.scm

BUILT_SOURCES = swig-bar.c
MAINTAINERCLEANFILES = swig-bar.c
