AM_CFLAGS=\
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

PWD := $(shell pwd)
checklibdir=${PWD}
checklib_LTLIBRARIES=libfoo.la libgncmodfoo.la

libfoo_la_SOURCES = foo.c

noinst_HEADERS = \
  foo.h

libgncmodfoo_la_SOURCES = gnc-mod-foo.c

libgncmodfoo_la_LIBADD = \
  libfoo.la \
  $(GLIB_LIBS)

EXTRA_DIST = \
  foo.i \
  gnucash/foo.scm

swig-foo.c: foo.i
	swig -guile $(SWIG_ARGS) -Linkage module -o $@ $<

BUILT_SOURCES = swig-foo.c
CLEANFILES = $(BUILT_SOURCES)

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
