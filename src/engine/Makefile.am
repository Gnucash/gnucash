SUBDIRS = . test-core test 
PWD := $(shell pwd)

pkglib_LTLIBRARIES = libgncmod-engine.la libgw-engine.la libgw-kvp.la

AM_CFLAGS = \
	-I${top_srcdir}/lib/libc \
	-I${top_srcdir}/src/core-utils \
	-I${top_srcdir}/src \
	-I${top_srcdir}/src/gnc-module \
	-I${top_srcdir}/src/business/business-core/ \
	${QOF_CFLAGS} \
	${GNUCASH_ENGINE_CFLAGS}

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
  Split.c \
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
  gnc-session.c \
  gnc-session-scm.c \
  gncmod-engine.c \
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
  Split.h \
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
  gnc-event.h \
  gnc-filepath-utils.h \
  gnc-hooks.h \
  gnc-pricedb.h \
  gnc-session.h \
  gnc-session-scm.h \
  gncObject.h \
  kvp-scm.h \
  policy.h

noinst_HEADERS = \
  AccountP.h \
  FreqSpecP.h \
  GroupP.h \
  QueryP.h \
  SchedXactionP.h \
  ScrubP.h \
  SplitP.h \
  SX-book.h \
  SX-ttinfo.h \
  TransactionP.h \
  gnc-hooks-scm.h \
  gnc-lot.h \
  gnc-lot-p.h \
  gnc-pricedb-p.h \
  gw-engine.h \
  gw-kvp.h \
  policy-p.h

noinst_SCRIPTS = iso-currencies-to-c

libgncmod_engine_la_LIBADD = \
  ../gnc-module/libgncmodule.la \
  ../core-utils/libcore-utils.la \
  ${G_WRAP_LINK_ARGS} \
  ${QOF_LIBS} \
  ${GNUCASH_ENGINE_BASE_LIBS} \
  ${top_builddir}/lib/libc/libc-missing.la

libgw_kvp_la_SOURCES = gw-kvp.c kvp-scm.c
libgw_kvp_la_LDFLAGS = \
  ${G_WRAP_LINK_ARGS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}
libgw_kvp_la_LIBADD = \
  libgncmod-engine.la \
  libgw-engine.la

libgw_engine_la_SOURCES = gw-engine.c engine-helpers.c glib-helpers.c
libgw_engine_la_LDFLAGS = \
  ${G_WRAP_LINK_ARGS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}
libgw_engine_la_LIBADD = \
  libgncmod-engine.la

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
  gncla-dir.h.in \
  kvp_doc.txt \
  SX-book-p.h \
  ${gncmod_DATA} \
  ${gncscm_DATA} \
  ${gwmod_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = gw-engine-spec.scm gw-kvp-spec.scm iso-4217-currencies.scm
#For executing test cases
SCM_FILE_LINKS += \
  ${gncmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links: 
	rm -f gnucash g-wrapped
	$(LN_S) -f . gnucash 
	$(LN_S) -f . g-wrapped 
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	touch .scm-links

iso-4217-currencies.c: iso-4217-currencies.scm iso-currencies-to-c
	-chmod u+x ${srcdir}/iso-currencies-to-c
	GUILE_LOAD_PATH=@GNC_SRFI_LOAD_PATH@:${GUILE_LOAD_PATH} srcdir=${srcdir} ${srcdir}/iso-currencies-to-c

gw-engine.scm gw-engine.c gw-engine.h: \
  .scm-links gw-engine-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-engine-spec.scm\") \
	 (gw:generate-wrapset \"gw-engine\")"

gw-kvp.scm gw-kvp.c gw-kvp.h: \
  .scm-links gw-kvp-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-kvp-spec.scm\") \
	 (gw:generate-wrapset \"gw-kvp\")"

BUILT_SOURCES = \
  iso-4217-currencies.c \
  gw-engine.scm gw-engine.c gw-engine.h \
  gw-kvp.scm gw-kvp.c gw-kvp.h gncla-dir.h

CLEANFILES = gnucash g-wrapped .scm-links gncla-dir.h
DISTCLEANFILES = ${SCM_FILE_LINKS} gw-engine.html gw-kvp.html

gncla-dir.h: gncla-dir.h.in ${top_builddir}/config.status
	rm -f $@.tmp
	sed < $< > $@.tmp \
                -e 's:@-libdir-@:${libdir}:g'
	mv $@.tmp $@
