
AM_CFLAGS = \
  -I.. -I../..  \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I../../src/backend/file \
  -I../../src/business/business-core \
  -I../../src/business/business-core/file \
  -I../../src/engine \
  ${GOBJECT_CFLAGS} \
  ${GLIB_CFLAGS}  \
  ${QOF_CFLAGS}

bin_PROGRAMS= \
  cashutil

cashutil_SOURCES = \
  backend-bus.c \
  qof-main.c \
  qofundo.c \
  cashutil.c 

EXTRA_DIST = \
  qof-main.h \
  qofundo.h

noinst_HEADERS = qofundo-p.h

cashutil_LDADD =   \
  ../../src/engine/libcashobjects.la  \
  ../../src/business/business-core/libcashbusobjects.la  \
  -lltdl  \
  ${QOF_LIBS}  \
  ${GLIB_LIBS}  \
  ${GOBJECT_LIBS} \
  ${GMODULE_LIBS} \
  ${RL_LIBS} \
  -lltdl  \
  -lpopt
