#SUBDIRS = objects . test backend

AM_CFLAGS = \
  -I.. -I../..  \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/src/backend/file \
  -I${top_srcdir}/src/business/business-core \
  -I${top_srcdir}/src/engine \
  ${GOBJECT_CFLAGS} \
  ${GLIB_CFLAGS}  \
  ${QOF_CFLAGS}

bin_PROGRAMS= \
  cashutil

cashutil_SOURCES = \
  qof-main.c \
  qofundo.c \
  cashutil.c 

EXTRA_DIST = \
  qof-main.h \
  qofundo.h \
  gncla-dir.h.in

noinst_HEADERS = qofundo-p.h

cashutil_LDADD =   \
  ${top_builddir}/src/objects/libcashobjects.la  \
  ${top_builddir}/src/objects/libcashbusobjects.la  \
  -lltdl  \
  ${QOF_LIBS}  \
  ${GLIB_LIBS}  \
  ${GOBJECT_LIBS} \
  ${GMODULE_LIBS} \
  ${RL_LIBS} \
  -lltdl  \
  -lpopt
