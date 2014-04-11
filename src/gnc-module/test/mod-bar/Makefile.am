AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

PWD := $(shell pwd)
checklibdir=${PWD}
checklib_LTLIBRARIES = libbar.la libgncmodbar.la

libbar_la_SOURCES = bar.c

# Make swig-bar.c for dist. It is an automatic dependency
# of gnc-mod-bar.c
noinst_HEADERS = \
  bar.h \
  swig-bar.c

libgncmodbar_la_SOURCES=gnc-mod-bar.c

libgncmodbar_la_LIBADD=libbar.la \
  $(GLIB_LIBS)

if BUILDING_FROM_SVN
swig-bar.c: bar.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module -o $@ $<
endif

EXTRA_DIST = \
  bar.i \
  gnucash/bar.scm

BUILT_SOURCES = swig-bar.c
MAINTAINERCLEANFILES = swig-bar.c

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
