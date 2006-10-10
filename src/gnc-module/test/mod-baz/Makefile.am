AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  -I${top_srcdir}/src/gnc-module/test/mod-foo \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}

PWD := $(shell pwd)
checklibdir = ${PWD}
checklib_LTLIBRARIES = libbaz.la libgncmodbaz.la

libbaz_la_SOURCES = baz.c
libbaz_la_LIBADD = ../mod-foo/libfoo.la

noinst_HEADERS = \
  baz.h

libgncmodbaz_la_SOURCES = gnc-mod-baz.c

libgncmodbaz_la_LIBADD = \
  libbaz.la \
  ../../libgncmodule.la \
  $(GLIB_LIBS)

swig-baz.c: baz.i
	swig -guile $(SWIG_ARGS) -Linkage module -o $@ $<

EXTRA_DIST = \
  baz.i \
  gnucash/baz.scm

BUILT_SOURCES = swig-baz.c
CLEANFILES = $(BUILT_SOURCES)

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
