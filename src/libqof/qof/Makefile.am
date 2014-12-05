include $(top_srcdir)/test-templates/Makefile.decl
SUBDIRS = . test
TEST_SUBDIRS = test

lib_LTLIBRARIES = libgnc-qof.la

libgnc_qof_la_LDFLAGS= \
	-version-info \
	$(LIBQOF_LIBRARY_VERSION) \
	$(BOOST_LDFLAGS)

libgnc_qof_common_libs =  \
  $(GLIB_LIBS) \
  $(REGEX_LIBS) \
  $(top_builddir)/lib/libc/libc-missing.la

libgnc_qof_la_LIBADD = $(libgnc_qof_common_libs)

AM_CPPFLAGS = \
  -I$(top_srcdir)/lib/libc \
  -I$(top_srcdir)/src \
  $(GLIB_CFLAGS) \
  $(BOOST_CPPFLAGS)

libgnc_qof_la_SOURCES =  \
   gnc-date.cpp        \
   gnc-int128.cpp      \
   gnc-numeric.cpp     \
   gnc-rational.cpp    \
   guid.cpp            \
   kvp-util.cpp        \
   kvp_frame.cpp       \
   kvp-value.cpp       \
   qofbackend.cpp      \
   qofbook.cpp         \
   qofchoice.cpp       \
   qofclass.cpp        \
   qofevent.cpp        \
   qofid.cpp           \
   qofinstance.cpp     \
   qoflog.cpp          \
   qofobject.cpp       \
   qofquery.cpp        \
   qofquerycore.cpp    \
   qofsession.cpp      \
   qof-string-cache.cpp  \
   qofutil.cpp

qofincludedir = ${pkgincludedir}

qofinclude_HEADERS = \
   gnc-date-p.h      \
   gnc-date.h        \
   gnc-numeric.h     \
   gnc-rational.hpp  \
   guid.h            \
   kvp-util-p.h      \
   kvp-util.h        \
   kvp_frame.h       \
   qof.h             \
   qofbackend-p.h    \
   qofbackend.h      \
   qofbook.h         \
   qofbookslots.h    \
   qofchoice.h       \
   qofclass.h        \
   qofevent.h        \
   qofid-p.h         \
   qofid.h           \
   qofinstance-p.h   \
   qofinstance.h     \
   qoflog.h          \
   qofobject.h       \
   qofquery.h        \
   qofquerycore.h    \
   qofsession.h      \
   qof-string-cache.h  \
   qofutil.h         \
   qof-gobject.h

noinst_HEADERS = \
   qofbook-p.h  \
   qofclass-p.h  \
   qofevent-p.h \
   gnc-int128.hpp  \
   qofobject-p.h  \
   qofquery-p.h  \
   qofquerycore-p.h \
   qofsession-p.h

EXTRA_DIST += \
  qofmath128.cpp

if OS_WIN32
libgnc_qof_la_SOURCES += qof-win32.cpp
else
EXTRA_DIST += qof-win32.cpp
endif

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"qof\"
