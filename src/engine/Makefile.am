SUBDIRS = . test-core test 

pkglib_LTLIBRARIES = libgncmod-engine.la

AM_CPPFLAGS = \
	-I${top_srcdir}/lib/libc \
	-I${top_srcdir}/src/core-utils \
	-I${top_srcdir}/src \
	-I${top_builddir}/src \
	-I${top_srcdir}/src/gnc-module \
    -I${top_srcdir}/src/libqof/qof \
	${GLIB_CFLAGS} \
	${GUILE_INCS}

libgncmod_engine_la_SOURCES = \
  Account.c \
  Recurrence.c \
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
  swig-engine.c \
  kvp-scm.c \
  engine-helpers.c \
  glib-helpers.c  \
  policy.c

EXTRA_libgncmod_engine_la_SOURCES = iso-4217-currencies.c

gncincludedir = ${GNC_INCLUDE_DIR}
gncinclude_HEADERS = \
  Account.h \
  FreqSpec.h \
  Recurrence.h \
  GNCId.h \
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
  QueryP.h \
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

libgncmod_engine_la_LDFLAGS = -avoid-version

libgncmod_engine_la_LIBADD = \
  ../gnc-module/libgnc-module.la \
  ../core-utils/libgnc-core-utils.la \
  ${top_builddir}/src/libqof/qof/libgnc-qof.la \
  ${GUILE_LIBS} \
  ${REGEX_LIBS} \
  ${GLIB_LIBS} \
  ${BINRELOC_LIBS} \
  ${top_builddir}/lib/libc/libc-missing.la

gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = engine.scm 

gncscmdir = ${GNC_SHAREDIR}/scm
gncscm_DATA = \
  commodity-table.scm \
  engine-interface.scm \
  engine-utilities.scm \
  gnc-numeric.scm

EXTRA_DIST = \
  README.query-api \
  design.txt \
  extensions.txt \
  iso-4217-currencies.scm \
  iso-currencies-to-c \
  gncla-dir.h.in \
  kvp_doc.txt \
  SX-book-p.h \
  engine.i \
  ${gncmod_DATA} \
  ${gncscm_DATA}

if GNUCASH_SEPARATE_BUILDDIR
#For compiling
SCM_FILE_LINKS = iso-4217-currencies.scm
#For executing test cases
SCM_FILE_LINKS += \
  ${gncmod_DATA} \
  ${gncscm_DATA}
endif

.scm-links: 
	$(RM) -rf gnucash
	mkdir -p  gnucash
if GNUCASH_SEPARATE_BUILDDIR
	for X in ${SCM_FILE_LINKS} ; do \
	  $(LN_S) -f ${srcdir}/$$X . ; \
	done
endif
	( cd gnucash; for A in $(gncmod_DATA) ; do $(LN_S) -f ../$$A . ; done )
if ! OS_WIN32
# Windows knows no "ln -s" but uses "cp": must copy every time (see bug #566567).
	touch .scm-links
endif

noinst_DATA = .scm-links

if BUILDING_FROM_SVN
swig-engine.c: engine.i ${top_srcdir}/src/base-typemaps.i \
               ${gncinclude_HEADERS} ${noinst_HEADERS}
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/lib/libqof/qof -I${top_srcdir}/src -o $@ $<
endif

iso-4217-currencies.c: iso-4217-currencies.scm iso-currencies-to-c
	-chmod u+x ${srcdir}/iso-currencies-to-c
	GUILE_LOAD_PATH=@GNC_SRFI_LOAD_PATH@:${GUILE_LOAD_PATH} srcdir=${srcdir} ${srcdir}/iso-currencies-to-c

gncla-dir.h: gncla-dir.h.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
		-e 's#@-DATADIRNAME-@#${DATADIRNAME}#g' \
		-e 's#@-libdir-@#${libdir}#g' \
		-e 's#@-sysconfdir-@#${sysconfdir}#g' \
		-e 's#@-datadir-@#${datadir}#g' \
		-e 's#@-prefix-@#${prefix}#g'
	mv $@.tmp $@

BUILT_SOURCES = iso-4217-currencies.c \
  gncla-dir.h

CLEANFILES = $(BUILT_SOURCES) gnucash .scm-links gncla-dir.h \
	${SCM_FILE_LINKS}
MAINTAINERCLEANFILES = swig-engine.c

INCLUDES = -DG_LOG_DOMAIN=\"gnc.engine\"
