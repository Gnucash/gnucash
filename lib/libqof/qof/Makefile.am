lib_LTLIBRARIES = libqof.la

libqof_la_LDFLAGS= -version-info $(LIBQOF_LIBRARY_VERSION)

AM_CFLAGS = \
  -I. \
  ${GLIB_CFLAGS}

libqof_la_SOURCES =  \
   deprecated.c      \
   gnc-date.c        \
   gnc-engine-util.c \
   gnc-numeric.c     \
   gnc-event.c       \
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
   qoflog.c          \
   qofobject.c       \
   qofquerycore.c    \
   qofsession.c      \
   qof_book_merge.c

qofincludedir = ${pkgincludedir}

qofinclude_HEADERS = \
   deprecated.h      \
   gnc-date.h        \
   gnc-engine-util.h \
   gnc-numeric.h     \
   gnc-event.h       \
   guid.h            \
   kvp_frame.h       \
   kvp-util.h        \
   kvp-util-p.h      \
   qof.h             \
   qof-be-utils.h    \
   qofbackend.h      \
   qofbackend-p.h    \
   qofclass.h        \
   qofchoice.h       \
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
   qofsession.h      \
   qofsql.h          \
   qofla-dir.h       \
   qof_book_merge.h

noinst_HEADERS = \
   gnc-event-p.h \
   md5.h  \
   qofclass-p.h  \
   qofmath128.h  \
   qofquery-p.h  \
   qofquery-deserial.h \
   qofquery-serialize.h \
   qofbook-p.h  \
   qofobject-p.h  \
   qofquerycore-p.h \
   qofsession-p.h

QOFLIBdir = $(QOF_LIB_DIR)

EXTRA_DIST = \
  qofla-dir.h.in \
  qofmath128.c

qofla-dir.h: qofla-dir.h.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
                -e 's:@-libdir-@:${QOFLIBdir}:g'
	 mv $@.tmp $@

BUILT_SOURCES = qofla-dir.h


