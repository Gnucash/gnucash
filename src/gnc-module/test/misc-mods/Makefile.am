AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}

LDADD = -L../.. -L../../.libs

PWD := $(shell pwd)
check_LTLIBRARIES=libfuturemodsys.la libincompatdep.la libagedver.la

libfuturemodsys_la_SOURCES=futuremodsys.c
libfuturemodsys_la_LDFLAGS=-rpath ${PWD} -avoid-version
libfuturemodsys_la_LIBADD = \
  $(GLIB_LIBS)

libincompatdep_la_SOURCES=incompatdep.c
libincompatdep_la_LDFLAGS=--rpath ${PWD} -avoid-version
libincompatdep_la_LIBADD = \
  ../../libgnc-module.la \
  $(GLIB_LIBS)

libagedver_la_SOURCES=agedver.c
libagedver_la_LDFLAGS=-rpath ${PWD} -avoid-version
libagedver_la_LIBADD = \
  $(GLIB_LIBS)

CLEANFILES = *.so* *.a
