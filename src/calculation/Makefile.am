
noinst_LIBRARIES = libgnccalc.a

libgnccalc_a_SOURCES = \
  expression_parser.c \
  fin.c

noinst_HEADERS = \
  finvar.h \
  finproto.h \
  fin_spl_protos.h \
  fin_static_proto.h

EXTRA_DIST = \
  .cvsignore \
  README \
  amort_opt.c \
  amort_prt.c \
  fin-interactive.c \
  fin-main.c \
  numeric_ops.c \
  numeric_ops.h

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS}
