SUBDIRS = . test 

lib_LTLIBRARIES = libgncmod-engine.la

CFLAGS = @CFLAGS@ ${GLIB_CFLAGS} ${G_WRAP_COMPILE_ARGS}

INCLUDES = -I${top_srcdir}/src/gnc-module ${GUILE_INCS} -I../

libgncmod_engine_la_SOURCES = \
  Account.c \
  Backend.c \
  DateUtils.c \
  FreqSpec.c \
  GNCId.c \
  Group.c \
  Query.c \
  SchedXaction.c \
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
  md5.c

# NetIO.c

noinst_HEADERS = \
  Account.h \
  AccountP.h \
  Backend.h \
  BackendP.h \
  DateUtils.h \
  GNCId.h \
  GNCIdP.h \
  Group.h \
  GroupP.h \
  Query.h \
  Scrub.h \
  TransLog.h \
  Transaction.h \
  TransactionP.h \
  date.h \
  gnc-associate-account.h \
  gnc-book-p.h \
  gnc-book.h \
  gnc-commodity.h \
  gnc-common.h \
  gnc-engine-util.h \
  gnc-engine.h \
  gnc-event-p.h \
  gnc-event.h \
  gnc-numeric.h \
  gnc-pricedb-p.h \
  gnc-pricedb.h \
  guid.h \
  kvp_frame.h \
  md5.h \
  FreqSpec.h \
  SchedXaction.h

#  NetIO.h 

libgncmod_engine_la_LDFLAGS = -module
libgncmod_engine_la_LIBADD = ${GUILE_LIBS} ${GLIB_LIBS}

libgw_glib_la_SOURCES=gw-glib.c glib-helpers.c
libgw_glib_la_LDFLAGS=-module

libgw_engine_la_SOURCES=gw-engine.c engine-helpers.c
libgw_engine_la_LDFLAGS=-module

gncmoddir=${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA=engine.scm 

gncscmdir=${GNC_SHAREDIR}/scm
gncscm_DATA=commodity-table.scm engine-init.scm engine-interface.scm \
engine-utilities.scm gnc-numeric.scm iso-4217-currencies.scm

gwmoddir=${GNC_SHAREDIR}/guile-modules/g-wrapped
gwmod_LTLIBRARIES=libgw-glib.la libgw-engine.la 
gwmod_DATA=gw-engine-spec.scm gw-glib-spec.scm

EXTRA_DIST = \
  .cvsignore \
  README.query-api \
  design.txt \
  extensions.txt \
  kvp_doc.txt

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

BUILT_SOURCES += gw-engine.c gw-engine.h gw-glib.c gw-glib.h
CLEANFILES += gw-engine.c gw-engine.h gw-engine.html gw-glib.c gw-glib.h gw-glib.html gnucash g-wrapped .scm-links
