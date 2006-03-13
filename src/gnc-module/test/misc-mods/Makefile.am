AM_CFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}

LDADD = -L../.. -L../../.libs

PWD := $(shell pwd)
checklibdir=${PWD}
checklib_LTLIBRARIES=libfuturemodsys.la libincompatdep.la libagedver.la

libfuturemodsys_la_SOURCES=futuremodsys.c
libfuturemodsys_la_LIBADD = \
  $(GLIB_LIBS)

libincompatdep_la_SOURCES=incompatdep.c
libincompatdep_la_LDFLAGS=-module
libincompatdep_la_LIBADD = \
  ../../libgncmodule.la \
  $(GLIB_LIBS)

libagedver_la_SOURCES=agedver.c
libagedver_la_LIBADD = \
  $(GLIB_LIBS)

CLEANFILES = *.so* *.a

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
