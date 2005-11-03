SUBDIRS = . test-core test 
PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgw-engine.la libgw-kvp.la libgncmod-engine.la

AM_CFLAGS = \
	-I${top_srcdir}/lib/libc \
	-I${top_srcdir}/src/core-utils \
	-I${top_srcdir}/src \
	-I${top_srcdir}/src/gnc-module \
	-I${top_srcdir}/src/business/business-core/ \
	${QOF_CFLAGS} \
	${GNUCASH_ENGINE_CFLAGS}

qof_library = \
   gnc-date.c        \
   gnc-engine-util.c \
   gnc-numeric.c     \
   gnc-event.c       \
   gnc-trace.c       \
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
   qofobject.c       \
   qofquerycore.c    \
   qofsession.c      \
   qof_book_merge.c

qof_headers =  \
   gnc-date.h        \
   gnc-engine-util.h \
   gnc-numeric.h     \
   gnc-event.h       \
   gnc-trace.h       \
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
   qofid.h           \
   qofid-p.h         \
   qofinstance-p.h   \
   qofinstance.h     \
   qofquery.h        \
   qofbook.h         \
   qofobject.h       \
   qofquerycore.h    \
   qofsession.h      \
   qof_book_merge.h

qof_noinst_headers = \
   gnc-event-p.h \
   md5.h  \
   qofclass-p.h  \
   qofmath128.h  \
   qofquery-p.h  \
   qofbook-p.h  \
   qofobject-p.h  \
   qofquerycore-p.h \
   qofsession-p.h \
   qofsql.h
				 
if USE_LIBQOF

lib_LTLIBRARIES = libqof.la

libqof_la_LDFLAGS= -version-info $(LIBQOF_LIBRARY_VERSION) 

libqof_la_SOURCES =  ${qof_library}

qofincludedir = ${pkgincludedir}

qofinclude_HEADERS = ${qof_headers} \
   qofla-dir.h

QOFLIBdir = $(libdir)

qofla-dir.h: qofla-dir.h.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
                -e 's:@-libdir-@:${QOFLIBdir}:g'
	mv $@.tmp $@

qof_builds = qofla-dir.h

else

libqof_la_SOURCES =

qofincludedir = ${pkgincludedir}

qofinclude_HEADERS = 

qof_builds =

endif

libgncmod_engine_la_SOURCES = \
  Account.c \
  FreqSpec.c \
  Recurrence.c \
  Group.c \
  Period.c \
  Query.c \
  SchedXaction.c \
  SX-book.c \
  SX-ttinfo.c \
  Scrub.c \
  Scrub2.c \
  Scrub3.c \
  TransLog.c \
  Transaction.c \
  cap-gains.c \
  cashobjects.c \
  gnc-associate-account.c \
  gnc-budget.c \
  gnc-commodity.c \
  gnc-engine.c \
  gnc-filepath-utils.c \
  gnc-hooks.c \
  gnc-lot.c \
  gnc-pricedb.c \
  gnc-session-scm.c \
  gncmod-engine.c \
  messages.c \
  policy.c

EXTRA_libgncmod_engine_la_SOURCES = iso-4217-currencies.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  Account.h \
  FreqSpec.h \
  Recurrence.h \
  GNCId.h \
  Group.h \
  Period.h \
  SchedXaction.h \
  SX-book.h \
  SX-ttinfo.h \
  Query.h \
  QueryNew.h \
  QueryObject.h \
  QueryCore.h \
  Scrub.h \
  Scrub2.h \
  Scrub3.h \
  TransLog.h \
  Transaction.h \
  cap-gains.h \
  cashobjects.h \
  engine-helpers.h \
  glib-helpers.h \
  gnc-associate-account.h \
  gnc-book.h \
  gnc-budget.h \
  gnc-commodity.h \
  gnc-engine.h \
  gnc-filepath-utils.h \
  gnc-hooks.h \
  gnc-pricedb.h \
  gnc-session.h \
  gnc-session-scm.h \
  gncObject.h \
  kvp-scm.h \
  messages.h \
  policy.h

noinst_HEADERS = \
  AccountP.h \
  FreqSpecP.h \
  GroupP.h \
  QueryP.h \
  SchedXactionP.h \
  ScrubP.h \
  SX-book.h \
  SX-ttinfo.h \
  TransactionP.h \
  gnc-hooks-scm.h \
  gnc-lot.h \
  gnc-lot-p.h \
  gnc-pricedb-p.h \
  gw-engine.h \
  gw-kvp.h \
  policy-p.h \
  ${qof_noinst_headers}

noinst_SCRIPTS = iso-currencies-to-c

libgncmod_engine_la_LIBADD = ${GNUCASH_ENGINE_BASE_LIBS} \
 ${QOF_LIBS} \
 libgw-engine.la \
 libgw-kvp.la \
 ../gnc-module/libgncmodule.la \
 ../../lib/libc/libc-missing.la

libgw_kvp_la_SOURCES = gw-kvp.c kvp-scm.c
libgw_kvp_la_LDFLAGS = ${G_WRAP_LINK_ARGS} ${QOF_LIBS}

libgw_engine_la_SOURCES = gw-engine.c engine-helpers.c glib-helpers.c
libgw_engine_la_LDFLAGS = \
  ${G_WRAP_LINK_ARGS} \
  ${QOF_LIBS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = engine.scm 

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
  commodity-table.scm \
  engine-interface.scm \
  engine-utilities.scm \
  gnc-numeric.scm

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = \
  gw-engine-spec.scm \
  gw-engine.scm \
  gw-kvp-spec.scm \
  gw-kvp.scm

EXTRA_DIST = \
  README.query-api \
  design.txt \
  extensions.txt \
  iso-4217-currencies.scm \
  iso-currencies-to-c \
  qofla-dir.h.in \
  gncla-dir.h.in \
  kvp_doc.txt \
  SX-book-p.h \
  ${qof_library} \
  ${qof_headers} \
  ${gncmod_DATA} \
  ${gncscm_DATA} \
  ${gwmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = gw-engine-spec.scm gw-kvp-spec.scm iso-4217-currencies.scm
#For executing test cases
SCM_FILE_LINKS += engine.scm
endif

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  ln -sf ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

iso-4217-currencies.c: iso-4217-currencies.scm iso-currencies-to-c
	-chmod u+x ${srcdir}/iso-currencies-to-c
	GUILE_LOAD_PATH=@GNC_SRFI_LOAD_PATH@:${GUILE_LOAD_PATH} srcdir=${srcdir} ${srcdir}/iso-currencies-to-c

gw-engine.scm gw-engine.c gw-engine.h: \
  .scm-links gw-engine-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-engine-spec.scm\") \
	 (gw:generate-wrapset \"gw-engine\")"

gw-kvp.scm gw-kvp.c gw-kvp.h: \
  .scm-links gw-kvp-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-kvp-spec.scm\") \
	 (gw:generate-wrapset \"gw-kvp\")"

BUILT_SOURCES = \
  ${qof_builds} \
  iso-4217-currencies.c \
  gw-engine.scm gw-engine.c gw-engine.h \
  gw-kvp.scm gw-kvp.c gw-kvp.h gncla-dir.h

DISTCLEANFILES = gnucash g-wrapped .scm-links ${SCM_FILE_LINKS} \
                 gw-engine.html gw-kvp.html qofla-dir.h gncla-dir.h

gncla-dir.h: gncla-dir.h.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
                -e 's:@-libdir-@:${libdir}:g'
	mv $@.tmp $@
