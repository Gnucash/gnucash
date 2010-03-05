SUBDIRS = . test

lib_LTLIBRARIES = libgnc-core-utils.la

libgnc_core_utils_la_SOURCES = \
  binreloc.c \
  gnc-main.c \
  gnc-filepath-utils.c \
  gnc-gconf-utils.c \
  gnc-gdate-utils.c \
  gnc-gkeyfile-utils.c \
  gnc-glib-utils.c \
  gnc-path.c \
  gnc-uri-utils.c \
  swig-core-utils.c

libgnc_core_utils_la_LIBADD = \
  ${GUILE_LIBS} \
  ${GLIB_LIBS} \
  ${BINRELOC_LIBS} \
  ${GCONF_LIBS} \
  ${QOF_LIBS}

noinst_HEADERS = \
  binreloc.h \
  gnc-main.h \
  gnc-filepath-utils.h \
  gnc-gconf-utils.h \
  gnc-gdate-utils.h \
  gnc-gkeyfile-utils.h \
  gnc-glib-utils.h \
  gnc-path.h \
  gnc-uri-utils.h

if BUILDING_FROM_SVN
swig-core-utils.c: core-utils.i ${top_srcdir}/src/base-typemaps.i
	$(SWIG) -guile $(SWIG_ARGS) -Linkage module \
	-I${top_srcdir}/src -o $@ $<
endif

AM_CPPFLAGS = \
  ${GUILE_INCS} \
  ${GLIB_CFLAGS} \
  ${GCONF_CFLAGS} \
  ${QOF_CFLAGS} \
  -I${top_srcdir}/src/libqof/qof \
  -I${top_srcdir}/src


gncmoddir = ${GNC_SHAREDIR}/guile-modules/gnucash
gncmod_DATA = core-utils.scm

if GNUCASH_SEPARATE_BUILDDIR
#Only needed when srcdir and builddir are different
#for running
SCM_FILE_LINKS = ${gncmod_DATA}
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
EXTRA_DIST = $(gncmod_DATA) core-utils.i gncla-dir.h.in
CLEANFILES = gnucash .scm-links
MAINTAINERCLEANFILES = swig-core-utils.c
BUILT_SOURCES = gncla-dir.h

INCLUDES = -DG_LOG_DOMAIN=\"gnc.core-utils\"

gncla-dir.h: gncla-dir.h.in ${top_builddir}/config.status Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
		-e 's#@-DATADIRNAME-@#${DATADIRNAME}#g' \
		-e 's#@-libdir-@#${libdir}#g' \
		-e 's#@-sysconfdir-@#${sysconfdir}#g' \
		-e 's#@-datadir-@#${datadir}#g' \
		-e 's#@-prefix-@#${prefix}#g'
	mv $@.tmp $@

