SUBDIRS = . test

pkglib_LTLIBRARIES = libgnc-calculation.la

libgnc_calculation_la_LDFLAGS = -avoid-version

libgnc_calculation_la_LIBADD = \
  ${GLIB_LIBS} ${GUILE_LIBS} \
  ${QOF_LIBS}

libgnc_calculation_la_SOURCES = \
  expression_parser.c \
  fin.c

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

AM_CFLAGS = \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/src \
  ${GUILE_INCS} \
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS}
