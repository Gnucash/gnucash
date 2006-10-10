AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

PWD := $(shell pwd)
checklibdir=${PWD}
checklib_LTLIBRARIES = libbar.la libgncmodbar.la

libbar_la_SOURCES = bar.c

noinst_HEADERS = \
  bar.h

libgncmodbar_la_SOURCES=gnc-mod-bar.c

libgncmodbar_la_LIBADD=libbar.la \
  $(GLIB_LIBS)

swig-bar.c: bar.i
	swig -guile $(SWIG_ARGS) -Linkage module -o $@ $<

EXTRA_DIST = \
  bar.i \
  gnucash/bar.scm

BUILT_SOURCES = swig-bar.c
CLEANFILES = $(BUILT_SOURCES)

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
