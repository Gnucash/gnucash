AM_CFLAGS=\
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GUILE_INCS} \
  ${G_WRAP_COMPILE_ARGS} \
  ${GLIB_CFLAGS}

PWD := $(shell pwd)
checklibdir=${PWD}
checklib_LTLIBRARIES=libfoo.la libgncmodfoo.la

libfoo_la_SOURCES = foo.c

noinst_HEADERS = \
  foo.h

libgncmodfoo_la_SOURCES = gnc-mod-foo.c
nodist_libgncmodfoo_la_SOURCES = foo-gwrap.c

libgncmodfoo_la_LIBADD = \
  libfoo.la \
  $(G_WRAP_LINK_ARGS) \
  $(GLIB_LIBS)

EXTRA_DIST = \
  foo-gwrap.scm \
  gnucash/foo.scm

foo-gwrap.c foo-gwrap.h: foo-gwrap.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	  "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
           (primitive-load \"${srcdir}/foo-gwrap.scm\") \
           (gw:generate-wrapset \"foo-gwrap\")"

BUILT_SOURCES = foo-gwrap.c foo-gwrap.h
CLEANFILES = $(BUILT_SOURCES) foo-gwrap.html

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
