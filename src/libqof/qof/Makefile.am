include $(top_srcdir)/test-templates/Makefile.decl
SUBDIRS = . test
TEST_SUBDIRS = test

lib_LTLIBRARIES = libgnc-qof.la

libgnc_qof_la_LDFLAGS= -version-info $(LIBQOF_LIBRARY_VERSION)
libgnc_qof_common_libs =  \
  $(GLIB_LIBS) \
  $(REGEX_LIBS) \
  $(top_builddir)/lib/libc/libc-missing.la
libgnc_qof_la_LIBADD = $(libgnc_qof_common_libs)

AM_CPPFLAGS = \
  -I$(top_srcdir)/lib/libc \
  -I$(top_srcdir)/src \
  $(GLIB_CFLAGS)

libgnc_qof_la_SOURCES =  \
   gnc-date.c        \
   gnc-numeric.c     \
   guid.c            \
   kvp-util.c        \
   kvp_frame.c       \
   qofbackend.c      \
   qofbook.c         \
   qofchoice.c       \
   qofclass.c        \
   qofevent.c        \
   qofid.c           \
   qofinstance.c     \
   qoflog.c          \
   qofobject.c       \
   qofquery.c        \
   qofquerycore.c    \
   qofreference.c    \
   qofsession.c      \
   qof-string-cache.c  \
   qofutil.c

qofincludedir = ${pkgincludedir}

qofinclude_HEADERS = \
   gnc-date-p.h      \
   gnc-date.h        \
   gnc-numeric.h     \
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
   qofreference.h    \
   qofsession.h      \
   qof-string-cache.h  \
   qofutil.h         \
   qof-gobject.h

noinst_HEADERS = \
   md5.h  \
   qofbook-p.h  \
   qofclass-p.h  \
   qofevent-p.h \
   qofmath128-p.h  \
   qofobject-p.h  \
   qofquery-p.h  \
   qofquerycore-p.h \
   qofsession-p.h
   
# Must compile md5.c without strict aliasing, otherwise function md5_finish_ctx
# gets "dereferencing type-punned pointer will break strict-aliasing rules"
noinst_LTLIBRARIES = libmd5.la
libmd5_la_SOURCES = md5.c
libmd5_la_CFLAGS=-fno-strict-aliasing
libgnc_qof_la_LIBADD += libmd5.la

EXTRA_DIST += \
  qofmath128.c \
  CMakeLists.txt

if OS_WIN32
libgnc_qof_la_SOURCES += qof-win32.c
else
EXTRA_DIST += qof-win32.c
endif

## For testing the qofmath128 routines
# run "make check" (to build the test program) and then run test-qofmath
check_PROGRAMS = test-qofmath
test_qofmath_SOURCES=gnc-numeric.c
test_qofmath_CPPFLAGS=$(AM_CPPFLAGS) -DTEST_128_BIT_MULT
test_qofmath_LDFLAGS=$(libgnc_qof_la_LDFLAGS)
test_qofmath_LDADD=$(libgnc_qof_common_libs)

AM_CPPFLAGS += -DG_LOG_DOMAIN=\"qof\"
