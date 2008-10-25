SUBDIRS = . test

pkglib_LTLIBRARIES = libgncmod-calculation.la

libgncmod_calculation_la_LDFLAGS = -avoid-version

libgncmod_calculation_la_LIBADD = \
  ${GLIB_LIBS} ${GUILE_LIBS} \
  ${QOF_LIBS}

libgncmod_calculation_la_SOURCES = \
  expression_parser.c \
  fin.c \
  gncmod-calculation.c

noinst_HEADERS = \
  finvar.h \
  finproto.h \
  fin_spl_protos.h \
  fin_static_proto.h

EXTRA_DIST = \
  README \
  amort_opt.c \
  amort_prt.c \
  fin-interactive.c \
  fin-main.c \
  numeric_ops.c \
  numeric_ops.h

AM_CPPFLAGS = \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/src \
  -I${top_srcdir}/src/gnc-module \
  ${QOF_CFLAGS} \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS}
