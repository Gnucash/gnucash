AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnc-module/test/mod-foo \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}

PWD := $(shell pwd)
check_LTLIBRARIES = libbaz.la libgncmodbaz.la

libbaz_la_SOURCES = baz.c
libbaz_la_LDFLAGS = -rpath ${PWD}
libbaz_la_LIBADD = ../mod-foo/libfoo.la

# Make swig-baz.c for dist. It is an automatic dependency
# of gnc-mod-baz.c
noinst_HEADERS = \
  baz.h \
  swig-baz.c

libgncmodbaz_la_SOURCES = gnc-mod-baz.c
libgncmodbaz_la_LDFLAGS = -rpath ${PWD} -avoid-version
libgncmodbaz_la_LIBADD = \
  libbaz.la \
  ../../libgnc-module.la \
  $(GUILE_LIBS) \
  $(GLIB_LIBS)

if BUILDING_FROM_SVN
swig-baz.c: baz.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module -o $@ $<
endif

EXTRA_DIST = \
  baz.i \
  gnucash/baz.scm

BUILT_SOURCES = swig-baz.c
MAINTAINERCLEANFILES = swig-baz.c
