SUBDIRS = . test-core test 

pkglib_LTLIBRARIES = libgw-engine.la libgw-kvp.la libgncmod-engine.la 

AM_CFLAGS = -I${top_srcdir}/src/gnc-module ${GNUCASH_ENGINE_CFLAGS}

libgncmod_engine_la_SOURCES = \
  Account.c \
  Backend.c \
  DateUtils.c \
  FreqSpec.c \
  GNCId.c \
  Group.c \
  Period.c \
  Query.c \
  QueryNew.c \
  QueryCore.c \
  QueryObject.c \
  SchedXaction.c \
  SX-ttinfo.c \
  Scrub.c \
  TransLog.c \
  Transaction.c \
  date.c \
  gnc-associate-account.c \
  gnc-book.c \
  gnc-commodity.c \
  gnc-engine-util.c \
  gnc-engine.c \
  gnc-event.c \
  gnc-numeric.c \
  gnc-pricedb.c \
  gnc-session.c \
  gncmod-engine.c \
  guid.c \
  gncObject.c \
  kvp_frame.c \
  kvp-util.c \
  md5.c \
  messages.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  Account.h \
  Backend.h \
  DateUtils.h \
  FreqSpec.h \
  GNCId.h \
  Group.h \
  Period.h \
  SchedXaction.h \
  SX-ttinfo.h \
  Query.h \
  QueryNew.h \
  QueryObject.h \
  QueryCore.h \
  Scrub.h \
  TransLog.h \
  Transaction.h \
  date.h \
  engine-helpers.h \
  glib-helpers.h \
  gnc-associate-account.h \
  gnc-book.h \
  gnc-commodity.h \
  gnc-engine-util.h \
  gnc-engine.h \
  gnc-event.h \
  gnc-numeric.h \
  gnc-pricedb.h \
  gnc-session.h \
  guid.h \
  gncObject.h \
  kvp_frame.h \
  kvp-scm.h \
  messages.h

noinst_HEADERS = \
  AccountP.h \
  BackendP.h \
  FreqSpecP.h \
  GNCIdP.h \
  GroupP.h \
  QueryP.h \
  QueryNewP.h \
  QueryObjectP.h \
  QueryCoreP.h \
  SchedXactionP.h \
  SX-ttinfo.h \
  TransactionP.h \
  gnc-book-p.h \
  gnc-event-p.h \
  gnc-pricedb-p.h \
  gnc-session-p.h \
  gncObjectP.h \
  kvp-util-p.h \
  md5.h

noinst_SCRIPTS = iso-currencies-to-c

libgncmod_engine_la_LDFLAGS = -module
libgncmod_engine_la_LIBADD = ${GNUCASH_ENGINE_BASE_LIBS} \
 ${top_builddir}/src/gnc-module/libgncmodule.la \
 ${top_builddir}/src/engine/libgw-engine.la \
 ${top_builddir}/src/engine/libgw-kvp.la 

libgw_kvp_la_SOURCES = gw-kvp.c kvp-scm.c
libgw_kvp_la_LDFLAGS = -module ${G_WRAP_LINK_ARGS}

libgw_engine_la_SOURCES = gw-engine.c engine-helpers.c glib-helpers.c
libgw_engine_la_LDFLAGS = -module ${G_WRAP_LINK_ARGS}

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = engine.scm 

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
  commodity-table.scm \
  engine-init.scm \
  engine-interface.scm \
  engine-utilities.scm \
  gnc-numeric.scm

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_DATA = \
  gw-engine.scm \
  gw-kvp.scm \
  gw-engine-spec.scm \
  gw-kvp-spec.scm

EXTRA_DIST = \
  .cvsignore \
  README.query-api \
  design.txt \
  extensions.txt \
  iso-4217-currencies.scm \
  iso-currencies-to-c \
  kvp_doc.txt \
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

gncmod-engine.c: gw-engine.h

gnc-commodity.c: iso-4217-currencies.c

iso-4217-currencies.c: iso-4217-currencies.scm iso-currencies-to-c
	chmod u+x ./iso-currencies-to-c
	GUILE_LOAD_PATH=${top_srcdir}/lib/:${GUILE_LOAD_PATH} ${srcdir}/iso-currencies-to-c

gw-engine.scm gw-engine.c gw-engine.h: .scm-links gw-engine-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-engine-spec.scm\") \
	 (gw:generate-wrapset \"gw-engine\")"

gw-kvp.scm gw-kvp.c gw-kvp.h: .scm-links gw-kvp-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-kvp-spec.scm\") \
	 (gw:generate-wrapset \"gw-kvp\")"

BUILT_SOURCES = \
  gw-engine.scm gw-engine.c gw-engine.h \
  gw-kvp.scm gw-kvp.c gw-kvp.h 
CLEANFILES = ${BUILT_SOURCES} gw-engine.html gw-kvp.html \
             iso-4217-currencies.c \
             ${SCM_FILE_LINKS} gnucash g-wrapped .scm-links
