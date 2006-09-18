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
  binreloc.c \
  cap-gains.c \
  cashobjects.c \
  gnc-associate-account.c \
  gnc-budget.c \
  gnc-commodity.c \
  gnc-engine.c \
  gnc-filepath-utils.c \
  gnc-hooks.c \
  gnc-lot.c \
  gnc-path.c \
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
  binreloc.h \
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
  gnc-path.h \
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
  policy-p.h

noinst_SCRIPTS = iso-currencies-to-c

libgncmod_engine_la_LIBADD = \
  ../gnc-module/libgncmodule.la \
  ../core-utils/libcore-utils.la \
  ${G_WRAP_LINK_ARGS} \
  ${QOF_LIBS} \
  ${REGEX_LIBS} \
  ${GNUCASH_ENGINE_BASE_LIBS} \
  ${BINRELOC_LIBS} \
  ${top_builddir}/lib/libc/libc-missing.la

libgw_kvp_la_SOURCES = kvp-scm.c
nodist_libgw_kvp_la_SOURCES = gw-kvp.c
libgw_kvp_la_LDFLAGS = \
  ${G_WRAP_LINK_ARGS} \
  ${QOF_LIBS} \
  ${GLIB_LIBS}
libgw_kvp_la_LIBADD = \
  libgncmod-engine.la \
  libgw-engine.la

libgw_engine_la_SOURCES = engine-helpers.c glib-helpers.c
nodist_libgw_engine_la_SOURCES = gw-engine.c
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
  gw-kvp-spec.scm
nodist_gwmod_DATA = \
  gw-engine.scm \
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
	$(RM) -rf gnucash g-wrapped
	mkdir -p  gnucash g-wrapped
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
	touch .scm-links

clean-local:
	$(RM) -rf gnucash g-wrapped

.INTERMEDIATE: gwrap-files1 gwrap-files2

iso-4217-currencies.c: iso-4217-currencies.scm iso-currencies-to-c
	-chmod u+x ${srcdir}/iso-currencies-to-c
	GUILE_LOAD_PATH=@GNC_SRFI_LOAD_PATH@:${GUILE_LOAD_PATH} srcdir=${srcdir} ${srcdir}/iso-currencies-to-c

gw-engine.scm gw-engine.c gw-engine.h: \
  gwrap-files1

gwrap-files1: \
  .scm-links gw-engine-spec.scm ${top_builddir}/config.status
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-engine-spec.scm\") \
	 (gw:generate-wrapset \"gw-engine\")"
	touch $@
	( cd g-wrapped; $(LN_S) -f ../gw-engine*.scm . )

gw-kvp.scm gw-kvp.c gw-kvp.h: \
  gwrap-files2

gwrap-files2: \
  .scm-links gw-kvp-spec.scm ${top_builddir}/config.status gwrap-files1
	FLAVOR=gnome $(GUILE) -c \
	 "(set! %load-path (cons \"${G_WRAP_MODULE_DIR}\" %load-path)) \
	 (set! %load-path (cons \"${PWD}\" %load-path)) \
	 (primitive-load \"./gw-kvp-spec.scm\") \
	 (gw:generate-wrapset \"gw-kvp\")"
	touch $@
	( cd g-wrapped; $(LN_S) -f ../gw-kvp*.scm . )

gncla-dir.h: gncla-dir.h.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
		-e 's#@-LOCALE_DIR-@#${LOCALE_DIR}#g' \
		-e 's#@-GNC_ACCOUNTS_DIR-@#${GNC_ACCOUNTS_DIR}#g' \
		-e 's#@-GNC_GLADE_DIR-@#${GNC_GLADE_DIR}#g' \
		-e 's#@-GCONF_SCHEMA_CONFIG_SOURCE_DIRONLY-@#${GCONF_SCHEMA_CONFIG_SOURCE_DIRONLY}#g' \
		-e 's#@-pkglibdir-@#${pkglibdir}#g' \
		-e 's#@-libdir-@#${libdir}#g' \
		-e 's#@-sysconfdir-@#${sysconfdir}#g' \
		-e 's#@-datadir-@#${datadir}#g' \
		-e 's#@-prefix-@#${prefix}#g'
	mv $@.tmp $@

BUILT_SOURCES = iso-4217-currencies.c \
  gw-engine.scm gw-engine.c gw-engine.h \
  gw-kvp.scm gw-kvp.c gw-kvp.h gncla-dir.h

CLEANFILES = $(BUILT_SOURCES) .scm-links gncla-dir.h \
	${SCM_FILE_LINKS} gw-engine.html gw-kvp.html
