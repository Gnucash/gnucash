SUBDIRS = . test

# Now a shared library AND a GModule
pkglib_LTLIBRARIES = libgncmod-backend-dbi.la


# "${top_srcdir}/common/debug" below is for the splint header
AM_CPPFLAGS = \
  -I.. -I../.. \
  -DLOCALE_DIR=\""$(datadir)/locale"\" \
  -I${top_srcdir}/libgnucash/backend \
  -I${top_srcdir}/libgnucash/backend/sql \
  -I${top_srcdir}/common/debug \
  -I${top_srcdir}/libgnucash/engine \
  -I${top_srcdir}/libgnucash/core-utils \
  -I${top_srcdir}/lib/libc \
  -I${top_srcdir}/common \
  ${GLIB_CFLAGS} \
  ${BOOST_CPPFLAGS} \
  ${WARN_CFLAGS}

libgncmod_backend_dbi_la_SOURCES = \
  gnc-backend-dbi.cpp \
  gnc-dbisqlconnection.cpp \
  gnc-dbisqlresult.cpp

noinst_HEADERS = \
  gnc-backend-dbi.h \
  gnc-backend-dbi.hpp \
  gnc-dbisqlconnection.hpp \
  gnc-dbisqlresult.hpp \
  gnc-dbiprovider.hpp \
  gnc-dbiproviderimpl.hpp

libgncmod_backend_dbi_la_LDFLAGS = -shared -avoid-version
libgncmod_backend_dbi_la_LIBADD = \
   ${GLIB_LIBS} \
   ${top_builddir}/libgnucash/backend/sql/libgnc-backend-sql.la \
   ${top_builddir}/libgnucash/engine/libgncmod-engine.la \
   ${top_builddir}/libgnucash/core-utils/libgnc-core-utils.la \
   ${BOOST_LDFLAGS} -lboost_regex \
   ${LIBDBI_LIBS}

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"gnc.backend.dbi\"

EXTRA_DIST = CMakeLists.txt
