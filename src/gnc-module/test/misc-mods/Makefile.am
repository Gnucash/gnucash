AM_CPPFLAGS = \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${GLIB_CFLAGS} \
  ${GUILE_INCS}

LDADD = -L../.. -L../../.libs

PWD := $(shell pwd)
check_LTLIBRARIES=libgncmod_futuremodsys.la libgncmod_incompatdep.la libgncmod_agedver.la

libgncmod_futuremodsys_la_SOURCES=futuremodsys.c
libgncmod_futuremodsys_la_LDFLAGS=-rpath ${PWD} -avoid-version
libgncmod_futuremodsys_la_LIBADD = \
  $(GLIB_LIBS)

libgncmod_incompatdep_la_SOURCES=incompatdep.c
libgncmod_incompatdep_la_LDFLAGS=--rpath ${PWD} -avoid-version
libgncmod_incompatdep_la_LIBADD = \
  ../../libgnc-module.la \
  $(GLIB_LIBS)

libgncmod_agedver_la_SOURCES=agedver.c
libgncmod_agedver_la_LDFLAGS=-rpath ${PWD} -avoid-version
libgncmod_agedver_la_LIBADD = \
  $(GLIB_LIBS)

CLEANFILES = *.so* *.a
