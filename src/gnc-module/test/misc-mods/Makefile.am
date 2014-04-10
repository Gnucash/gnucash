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

libincompatdep_la_SOURCES=incompatdep.c
libincompatdep_la_LDFLAGS=-module

libagedver_la_SOURCES=agedver.c

CLEANFILES = *.so* *.a

# Keep automake from generating install targets for test libs
install-checklibLTLIBRARIES:
uninstall-checklibLTLIBRARIES:
