lib_LTLIBRARIES = libgncqof.la

libgncqof_la_LDFLAGS= -version-info $(LIBQOF_LIBRARY_VERSION)
libgncqof_la_LIBADD= \
  $(GLIB_LIBS) \
  $(REGEX_LIBS) \
  $(top_builddir)/lib/libc/libc-missing.la

AM_CFLAGS = \
  $(GLIB_CFLAGS) \
  -I$(top_srcdir)/lib/libc

libgncqof_la_SOURCES =  \
   deprecated.c      \
   gnc-date.c        \
   gnc-numeric.c     \
   guid.c            \
   kvp_frame.c       \
   kvp-util.c        \
   md5.c             \
   qofbackend.c      \
   qofclass.c        \
   qofchoice.c       \
   qofid.c           \
   qofinstance.c     \
   qofquery.c        \
   qofbook.c         \
   qofevent.c        \
   qoflog.c          \
   qofobject.c       \
   qofquerycore.c    \
   qofreference.c    \
   qofutil.c         \
   qofsession.c      \
   qofbookmerge.c

qofincludedir = ${pkgincludedir}

qofinclude_HEADERS = \
   deprecated.h      \
   gnc-date.h        \
   gnc-numeric.h     \
   guid.h            \
   kvp_frame.h       \
   kvp-util.h        \
   kvp-util-p.h      \
   qof.h             \
   qofbackend.h      \
   qofbackend-p.h    \
   qofclass.h        \
   qofchoice.h       \
   qofevent.h        \
   qofgobj.h         \
   qofid.h           \
   qofid-p.h         \
   qofinstance-p.h   \
   qofinstance.h     \
   qofquery.h        \
   qofbook.h         \
   qoflog.h          \
   qofobject.h       \
   qofquerycore.h    \
   qofreference.h    \
   qofsession.h      \
   qofsql.h          \
   qofutil.h         \
   qofbookmerge.h

nodist_qofinclude_HEADERS = \
   qofla-dir.h     

noinst_HEADERS = \
   md5.h  \
   qofclass-p.h  \
   qofmath128.h  \
   qofquery-p.h  \
   qofquery-deserial.h \
   qofquery-serialize.h \
   qofbook-p.h  \
   qofevent-p.h \
   qofobject-p.h  \
   qofquerycore-p.h \
   qofsession-p.h

EXTRA_DIST = \
  qofla-dir.h.in \
  qofmath128.c

qofla-dir.h: $(srcdir)/qofla-dir.h.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
                -e 's#@-libdir-@#${QOF_LIB_DIR}#g'
	 mv $@.tmp $@

BUILT_SOURCES = qofla-dir.h
CLEANFILES = $(BUILT_SOURCES)
