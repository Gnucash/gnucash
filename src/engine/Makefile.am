SUBDIRS = . test-core test 

pkglib_LTLIBRARIES = libgncmod-engine.la

CFLAGS = @CFLAGS@ ${GNUCASH_ENGINE_BASE_CFLAGS}

INCLUDES = -I${top_srcdir}/src/gnc-module

libgncmod_engine_la_SOURCES = \
  Account.c \
  Backend.c \
  DateUtils.c \
  FreqSpec.c \
  GNCId.c \
  Group.c \
  Query.c \
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
  gncmod-engine.c \
  guid.c \
  kvp_frame.c \
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
  SchedXaction.h \
  SX-ttinfo.h \
  Query.h \
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
  guid.h \
  kvp_frame.h \
  messages.h

noinst_HEADERS = \
  AccountP.h \
  BackendP.h \
  FreqSpecP.h \
  GNCIdP.h \
  GroupP.h \
  SX-ttinfo.h \
  TransactionP.h \
  gnc-book-p.h \
  gnc-event-p.h \
  gnc-pricedb-p.h \
  kvp-scm.h \
  md5.h

libgncmod_engine_la_LDFLAGS = -module
libgncmod_engine_la_LIBADD = ${GNUCASH_ENGINE_BASE_LIBS}

libgw_glib_la_SOURCES=gw-glib.c glib-helpers.c
libgw_glib_la_LDFLAGS=-module -lg-wrap-runtime-guile

libgw_engine_la_SOURCES=gw-engine.c engine-helpers.c
libgw_engine_la_LDFLAGS=-module -lg-wrap-runtime-guile

libgw_kvp_la_SOURCES=gw-kvp.c kvp-scm.c
libgw_kvp_la_LDFLAGS=-module -lg-wrap-runtime-guile

gncmoddir=${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA=engine.scm 

gncscmdir=${GNC_SHAREDIR}/scm
gncscm_DATA=commodity-table.scm engine-init.scm engine-interface.scm \
engine-utilities.scm gnc-numeric.scm iso-4217-currencies.scm 

gwmoddir = ${GNC_GWRAP_LIBDIR}
gwmod_LTLIBRARIES = libgw-glib.la libgw-engine.la libgw-kvp.la
gwmod_DATA = gw-engine-spec.scm gw-glib-spec.scm gw-kvp-spec.scm

EXTRA_DIST = \
  .cvsignore \
  README.query-api \
  design.txt \
  extensions.txt \
  kvp_doc.txt \
  ${gncmod_DATA} \
  ${gncscm_DATA} \
  ${gwmod_DATA}

.scm-links: 
	rm -f gnucash g-wrapped
	ln -sf . gnucash 
	ln -sf . g-wrapped 
	touch .scm-links

gw-engine.c gw-engine.h: .scm-links gw-engine-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-engine-spec.scm\") \
	 (gw:generate-module \"gw-engine\")"

gw-glib.c gw-glib.h: .scm-links gw-glib-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (primitive-load \"./gw-glib-spec.scm\") \
	 (gw:generate-module \"gw-glib\")"

gw-kvp.c gw-kvp.h: .scm-links gw-kvp-spec.scm
	FLAVOR=gnome guile -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-kvp-spec.scm\") \
	 (gw:generate-module \"gw-kvp\")"

BUILT_SOURCES += gw-engine.c gw-engine.h gw-glib.c gw-glib.h gw-kvp.c gw-kvp.h
CLEANFILES += gw-engine.c gw-engine.h gw-engine.html gw-glib.c gw-glib.h gw-glib.html gnucash g-wrapped .scm-links gw-kvp.c gw-kvp.h
