SUBDIRS = . test

# Now a shared library AND a GModule
lib_LTLIBRARIES = libgnc-backend-gda-utils.la
pkglib_LTLIBRARIES = libgncmod-backend-gda.la


AM_CFLAGS = \
  -I.. -I../.. \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/src/backend \
  -I${top_srcdir}/src/engine \
  -I${top_srcdir}/src/core-utils\
  -I${top_srcdir}/lib/libc\
  ${QOF_CFLAGS} \
  ${GLIB_CFLAGS} \
  ${LIBGDA_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${WARN_CFLAGS}

libgnc_backend_gda_utils_la_SOURCES = \
  gnc-backend-util-gda.c \
  gnc-account-gda.c \
  gnc-book-gda.c \
  gnc-budget-gda.c \
  gnc-commodity-gda.c \
  gnc-lots-gda.c \
  gnc-price-gda.c \
  gnc-recurrence-gda.c \
  gnc-schedxaction-gda.c \
  gnc-slots-gda.c \
  gnc-transaction-gda.c

libgncmod_backend_gda_la_SOURCES = \
  gnc-backend-gda.c

noinst_HEADERS = \
  gnc-account-gda.h \
  gnc-backend-gda.h \
  gnc-backend-util-gda.h \
  gnc-book-gda.h \
  gnc-budget-gda.h \
  gnc-commodity-gda.h \
  gnc-lots-gda.h \
  gnc-price-gda.h \
  gnc-recurrence-gda.h \
  gnc-schedxaction-gda.h \
  gnc-slots-gda.h \
  gnc-transaction-gda.h

libgnc_backend_gda_utils_la_LIBADD = \
   ${GLIB_LIBS} ${GCONF_LIBS} \
   ${top_builddir}/src/engine/libgncmod-engine.la \
   ${LIBGDA_LIBS} \
   ${QOF_LIBS}

libgncmod_backend_gda_la_LDFLAGS = -module -avoid-version
libgncmod_backend_gda_la_LIBADD = \
   ${GLIB_LIBS} ${GCONF_LIBS} \
   ${top_builddir}/src/engine/libgncmod-engine.la \
   libgnc-backend-gda-utils.la \
   ${LIBGDA_LIBS} \
   ${QOF_LIBS}

INCLUDES = -DG_LOG_DOMAIN=\"gnc.backend.gda\"
