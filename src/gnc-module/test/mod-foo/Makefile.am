AM_CFLAGS=\
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}

PWD := $(shell pwd)
checklibdir=${PWD}
checklib_LTLIBRARIES=libfoo.la libgncmodfoo.la

libfoo_la_SOURCES = foo.c

# Make swig-foo.c for dist. It is an automatic dependency
# of gnc-mod-foo.c
noinst_HEADERS = \
  foo.h \
  swig-foo.c

libgncmodfoo_la_SOURCES = gnc-mod-foo.c

libgncmodfoo_la_LIBADD = \
  libfoo.la \
  $(GLIB_LIBS)

EXTRA_DIST = \
  foo.i \
  gnucash/foo.scm

if BUILDING_FROM_SVN
swig-foo.c: foo.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module -o $@ $<
endif

BUILT_SOURCES = swig-foo.c
MAINTAINERCLEANFILES = swig-foo.c

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
